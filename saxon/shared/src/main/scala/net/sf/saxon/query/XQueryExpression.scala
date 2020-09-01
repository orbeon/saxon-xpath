package net.sf.saxon.query

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.event.ComplexContentOutputter
import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.event.Receiver
import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.ExpressionOwner
import net.sf.saxon.expr.PackageData
import net.sf.saxon.expr.XPathContextMajor
import net.sf.saxon.expr.instruct.Executable
import net.sf.saxon.expr.instruct.GlobalContextRequirement
import net.sf.saxon.expr.instruct.GlobalVariable
import net.sf.saxon.expr.instruct.SlotManager
import net.sf.saxon.expr.parser._
import net.sf.saxon.lib.ErrorReporter
import net.sf.saxon.lib.SerializerFactory
import net.sf.saxon.lib.TraceListener
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.om._
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.s9api.Location
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trace.TraceableComponent
import net.sf.saxon.trans.SaxonErrorCode
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.XmlProcessingException
import net.sf.saxon.tree.iter.ManualIterator
import javax.xml.transform.Result
import javax.xml.transform.Source
import javax.xml.transform.TransformerException
import javax.xml.transform.stream.StreamResult
import java.io.OutputStream
import java.util
import java.util._

import net.sf.saxon.s9api.HostLanguage.HostLanguage


/**
 * XQueryExpression represents a compiled query. This object is immutable and thread-safe,
 * the same compiled query may be executed many times in series or in parallel. The object
 * is intended to be created only by using the compileQuery method of the QueryProcessor class.
 * <p>Various methods are provided for evaluating the query, with different options for
 * delivery of the results.</p>
 */
class XQueryExpression(var expression: Expression,
                       var mainModule: QueryModule,
                       streaming: Boolean)
  extends Location
    with ExpressionOwner
    with TraceableComponent {

  val config: Configuration = mainModule.getConfiguration
  var stackFrameMap: SlotManager = config.makeSlotManager

  var executable: Executable = _

  val exec: Executable = mainModule.getExecutable

  expression.setRetainedStaticContext(mainModule.makeRetainedStaticContext())

  try {
    val visitor: ExpressionVisitor = ExpressionVisitor.make(mainModule)
    val optimizer: Optimizer = visitor.obtainOptimizer()
    visitor.setOptimizeForStreaming(streaming)
    expression = expression.simplify()
    expression.checkForUpdatingSubexpressions()
    val contextReq: GlobalContextRequirement = exec.getGlobalContextRequirement
    val req: ItemType =
      if (contextReq == null) AnyItemType
      else contextReq.getRequiredItemType
    val cit: ContextItemStaticInfo =
      config.makeContextItemStaticInfo(req, maybeUndefined = true)
    var e2: Expression = expression.typeCheck(visitor, cit)
    if (e2 != expression) {
      e2.setRetainedStaticContext(expression.getRetainedStaticContext)
      e2.setParentExpression(null)
      expression = e2
    }
    if (optimizer.isOptionSet(OptimizerOptions.MISCELLANEOUS)) {
      e2 = expression.optimize(visitor, cit)
      if (e2 != expression) {
        e2.setRetainedStaticContext(expression.getRetainedStaticContext)
        e2.setParentExpression(null)
        expression = e2
      }
    }
    if (optimizer.isOptionSet(OptimizerOptions.LOOP_LIFTING)) {
      e2 = LoopLifter.process(expression, visitor, cit)
      if (e2 != expression) {
        e2.setRetainedStaticContext(expression.getRetainedStaticContext)
        e2.setParentExpression(null)
        expression = e2
      }
    }
  } catch {
    case err: XPathException => {
      mainModule.reportStaticError(err)
      throw err
    }

  }

  ExpressionTool.allocateSlots(expression, 0, stackFrameMap)

  ExpressionTool.computeEvaluationModesForUserFunctionCalls(expression)

  val globalMap: List[GlobalVariable] = getPackageData.getGlobalVariableList
  if (globalMap != null) {
    globalMap.forEach { gVar =>
      val top: Expression = gVar.getBody
      if (top != null) {
        ExpressionTool.computeEvaluationModesForUserFunctionCalls(top)
      }
    }
  }

  executable.setConfiguration(config)

  def getExpression(): Expression = expression

  def getBody(): Expression = getExpression

  def getChildExpression(): Expression = expression

  override def setBody(expression: Expression): Unit = {
    this.setChildExpression(expression);
  }

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   * @return the QName of the object declared or manipulated by this instruction or expression
   */
  override def getObjectName(): StructuredQName = null

  override def getTracingTag(): String = "query"

  override def getLocation(): Location = this

  /**
   * Get data about the unit of compilation (XQuery module, XSLT package) to which this
   * container belongs
   */
  def getPackageData(): PackageData = mainModule.getPackageData

  /**
   * Get the Configuration to which this Container belongs
   *
   * @return the Configuration
   */
  def getConfiguration(): Configuration = mainModule.getConfiguration

  def usesContextItem(): Boolean = {
    if (ExpressionTool.dependsOnFocus(expression)) {
      return true
    }
    val map: List[GlobalVariable] = getPackageData.getGlobalVariableList
    if (map != null) {
      map.forEach { gVar =>
        val select: Expression = gVar.getBody
        if (select != null && ExpressionTool.dependsOnFocus(select)) {
          return true
        }
      }
    }
    false
  }

  def isUpdateQuery(): Boolean = false

  def getStackFrameMap(): SlotManager = stackFrameMap

  def explainPathMap(): Unit = ()

  // No action (requires Saxon-EE)
  // No action (requires Saxon-EE)

  /**
   * Get the static context in which this expression was compiled. This is essentially an internal
   * copy of the original user-created StaticQueryContext object, augmented with information obtained
   * from the query prolog of the main query module, and with information about functions and variables
   * imported from other library modules. The user-created StaticQueryContext object is not modified
   * by Saxon, whereas the QueryModule object includes additional information found in the query prolog.
   *
   * @return the QueryModule object representing the static context of the main module of the query.
   *         This is available for inspection, but must not be modified or reused by the application.
   */
  def getMainModule(): QueryModule = mainModule

  /*@NotNull*/

  def getExternalVariableNames(): Array[StructuredQName] = {
    val list: List[StructuredQName] = stackFrameMap.getVariableMap
    val names: Array[StructuredQName] =
      Array.ofDim[StructuredQName](stackFrameMap.getNumberOfVariables)
    for (i <- 0 until names.length) {
      names(i) = list.get(i)
    }
    names
  }

  /*@NotNull*/

  def evaluate(env: DynamicQueryContext): List[Any] = {
    if (isUpdateQuery) {
      throw new XPathException("Cannot call evaluate() on an updating query")
    }
    val list: ArrayList[Any] = new ArrayList[Any](100)
    iterator(env).forEachOrFail((item) =>
      list.add(SequenceTool.convertToJava(item)))
    list
  }

  /*@Nullable*/

  def evaluateSingle(env: DynamicQueryContext): Any = {
    if (isUpdateQuery) {
      throw new XPathException(
        "Cannot call evaluateSingle() on an updating query")
    }

    val itr: SequenceIterator = iterator(env)
    val item: Item = itr.next()

    if (item == null) {
      return null
    }
    SequenceTool.convertToJava(item)
  }

  /*@NotNull*/

  def iterator(env: DynamicQueryContext): SequenceIterator = {
    if (isUpdateQuery) {
      throw new XPathException("Cannot call iterator() on an updating query")
    }
    if (!env.getConfiguration.isCompatible(getExecutable.getConfiguration)) {
      throw new XPathException(
        "The query must be compiled and executed under the same Configuration",
        SaxonErrorCode.SXXP0004)
    }
    val controller: Controller = newController(env)
    try {
      val contextItem: Item = controller.getGlobalContextItem
      if (contextItem.isInstanceOf[NodeInfo] &&
        contextItem.asInstanceOf[NodeInfo].getTreeInfo.isTyped &&
        !getExecutable.isSchemaAware) {
        throw new XPathException(
          "A typed input document can only be used with a schema-aware query")
      }
      val context: XPathContextMajor = initialContext(env, controller)
      // In tracing/debugging mode, evaluate all the global variables first
      if (controller.getTraceListener != null) {
        controller.preEvaluateGlobals(context)
      }
      context.openStackFrame(stackFrameMap)
      val iterator: SequenceIterator = expression.iterate(context)
      if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED)) {
        iterator
      } else {
        new ErrorReportingIterator(iterator, controller.getErrorReporter)
      }
    } catch {
      case err: XPathException => {
        var terr: TransformerException = err
        while (terr.getException.isInstanceOf[TransformerException]) terr =
          terr.getException.asInstanceOf[TransformerException]
        val de: XPathException = XPathException.makeXPathException(terr)
        controller.reportFatalError(de)
        throw de
      }

    }
  }

  def run(env: DynamicQueryContext,
          result: Result,
          outputProperties: Properties): Unit = {
    if (isUpdateQuery) {
      throw new XPathException("Cannot call run() on an updating query")
    }
    if (!env.getConfiguration.isCompatible(getExecutable.getConfiguration)) {
      throw new XPathException(
        "The query must be compiled and executed under the same Configuration",
        SaxonErrorCode.SXXP0004)
    }
    val contextItem: Item = env.getContextItem
    if (contextItem.isInstanceOf[NodeInfo] &&
      contextItem.asInstanceOf[NodeInfo].getTreeInfo.isTyped &&
      !getExecutable.isSchemaAware) {
      throw new XPathException(
        "A typed input document can only be used with a schema-aware query")
    }
    val controller: Controller = newController(env)
    if (result.isInstanceOf[Receiver]) {
      result
        .asInstanceOf[Receiver]
        .getPipelineConfiguration
        .setController(controller)
    }
    val actualProperties: Properties =
      validateOutputProperties(controller, outputProperties)
    val context: XPathContextMajor = initialContext(env, controller)
    // In tracing/debugging mode, evaluate all the global variables first
    val tracer: TraceListener = controller.getTraceListener
    if (tracer != null) {
      controller.preEvaluateGlobals(context)
    }
    context.openStackFrame(stackFrameMap)
    val mustClose: Boolean = result.isInstanceOf[StreamResult] &&
      result.asInstanceOf[StreamResult].getOutputStream == null
    var out: Receiver = null
    if (result.isInstanceOf[Receiver]) {
      out = result.asInstanceOf[Receiver]
    } else {
      val sf: SerializerFactory = context.getConfiguration.getSerializerFactory
      val pipe: PipelineConfiguration = controller.makePipelineConfiguration
      pipe.setHostLanguage(HostLanguage.XQUERY)
      out = sf.getReceiver(result,
        new SerializationProperties(actualProperties),
        pipe)
    }
    val dest: ComplexContentOutputter = new ComplexContentOutputter(out)
    dest.open()
    // Run the query
    try expression.process(dest, context)
    catch {
      case err: XPathException => {
        controller.reportFatalError(err)
        throw err
      }

    } finally try {
      if (tracer != null) {
        tracer.close()
      }
      dest.close()
    } catch {
      case e: XPathException => e.printStackTrace()

    }
    if (result.isInstanceOf[StreamResult]) {
      closeStreamIfNecessary(result.asInstanceOf[StreamResult], mustClose)
    }
  }

  def closeStreamIfNecessary(result: StreamResult,
                             mustClose: Boolean): Unit = {
    if (mustClose) {
      val os: OutputStream = result.getOutputStream
      if (os != null) {
        os.close()
      }
    }
  }

  def runStreamed(dynamicEnv: DynamicQueryContext,
                  source: Source,
                  result: Result,
                  outputProperties: Properties): Unit = {
    throw new XPathException("Streaming requires Saxon-EE")
  }

  def validateOutputProperties(
                                controller: Controller,
                                outputProperties: Properties): Properties = {
    val baseProperties: Properties =
      controller.getExecutable.getPrimarySerializationProperties.getProperties
    val sf: SerializerFactory =
      controller.getConfiguration.getSerializerFactory
    if (outputProperties != null) {
      val iter: util.Enumeration[_] = outputProperties.propertyNames()
      while (iter.hasMoreElements()) {
        val key: String = iter.nextElement().asInstanceOf[String]
        var value: String = outputProperties.getProperty(key)
        try {
          value = sf.checkOutputProperty(key, value)
          baseProperties.setProperty(key, value)
        } catch {
          case dynamicError: XPathException => {
            outputProperties.remove(key)
            val err: XmlProcessingException = new XmlProcessingException(
              dynamicError)
            err.setWarning(true)
            controller.getErrorReporter.report(err)
          }

        }
      }
    }
    if (baseProperties.getProperty("method") == null) {
      // XQuery forces the default method to XML, unlike XSLT where it depends on the contents of the result tree
      baseProperties.setProperty("method", "xml")
    }
    baseProperties
  }

  // Validate the serialization properties requested
  // Validate the serialization properties requested

  /*@NotNull*/

  def runUpdate(dynamicEnv: DynamicQueryContext): Set[MutableNodeInfo] =
    throw new XPathException("Calling runUpdate() on a non-updating query")

  def runUpdate(dynamicEnv: DynamicQueryContext, agent: UpdateAgent): Unit = {
    throw new XPathException("Calling runUpdate() on a non-updating query")
  }

  def initialContext(dynamicEnv: DynamicQueryContext,
                     controller: Controller): XPathContextMajor = {
    val contextItem: Item = controller.getGlobalContextItem
    val context: XPathContextMajor = controller.newXPathContext
    if (contextItem != null) {
      val single: ManualIterator = new ManualIterator(contextItem)
      context.setCurrentIterator(single)
      controller.setGlobalContextItem(contextItem)
    }
    context
  }

  /*@NotNull*/

  def newController(env: DynamicQueryContext): Controller = {
    val controller: Controller =
      new Controller(executable.getConfiguration, executable)
    env.initializeController(controller)
    //controller.getBindery(getPackageData()).allocateGlobals(executable.getGlobalVariableMap());
    controller
  }

  def explain(out: ExpressionPresenter): Unit = {
    out.startElement("query")
    // getKeyManager class does not exist. It will be fixed later.
    //  mainModule.getKeyManager.exportKeys(out, null)
    getExecutable.explainGlobalVariables(out)
    mainModule.explainGlobalFunctions(out)
    out.startElement("body")
    expression.export(out)
    out.endElement()
    out.endElement()
    out.close()
  }

  def getExecutable(): Executable = executable

  def setAllowDocumentProjection(allowed: Boolean): Unit = {
    if (allowed) {
      throw new UnsupportedOperationException(
        "Document projection requires Saxon-EE")
    }
  }

  def isDocumentProjectionAllowed(): Boolean = false

  /*@Nullable*/

  def getPublicId(): String = null

  /*@Nullable*/

  def getSystemId(): String = mainModule.getSystemId

  /**
   * Return the line number where the current document event ends.
   * <p><strong>Warning:</strong> The return value from the method
   * is intended only as an approximation for the sake of error
   * reporting; it is not intended to provide sufficient information
   * to edit the character content of the original XML document.</p>
   * <p>The return value is an approximation of the line number
   * in the document entity or external parsed entity where the
   * markup that triggered the event appears.</p>
   *
   * @return The line number, or -1 if none is available.
   * @see #getColumnNumber
   */
  def getLineNumber(): Int = -1

  /**
   * Return the character position where the current document event ends.
   * <p><strong>Warning:</strong> The return value from the method
   * is intended only as an approximation for the sake of error
   * reporting; it is not intended to provide sufficient information
   * to edit the character content of the original XML document.</p>
   * <p>The return value is an approximation of the column number
   * in the document entity or external parsed entity where the
   * markup that triggered the event appears.</p>
   *
   * @return The column number, or -1 if none is available.
   * @see #getLineNumber
   */
  def getColumnNumber(): Int = -1

  /**
   * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
   * should not be saved for later use. The result of this operation holds the same location information,
   * but in an immutable form.
   */
  def saveLocation(): Location = this

  def getHostLanguage(): HostLanguage = HostLanguage.XQUERY

  def setChildExpression(expr: Expression): Unit = {
    expression = expr
  }

  private class ErrorReportingIterator(private var base: SequenceIterator,
                                       private var reporter: ErrorReporter)
    extends SequenceIterator {

    /*@Nullable*/

    def next(): Item =
      try base.next()
      catch {
        case e1: XPathException => {
          e1.maybeSetLocation(expression.getLocation)
          val err: XmlProcessingException = new XmlProcessingException(e1)
          reporter.report(err)
          e1.setHasBeenReported(true)
          throw e1
        }

      }

    override def close(): Unit = {
      base.close()
    }

  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
