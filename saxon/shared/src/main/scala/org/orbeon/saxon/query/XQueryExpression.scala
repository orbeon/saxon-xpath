////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
package org.orbeon.saxon.query


import org.orbeon.saxon.event.{ComplexContentOutputter, PipelineConfiguration, Receiver}
import org.orbeon.saxon.expr.{Expression, ExpressionOwner, PackageData, XPathContextMajor}
import org.orbeon.saxon.expr.instruct.{Executable, GlobalContextRequirement, GlobalVariable, SlotManager}
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.lib.{ErrorReporter, SerializerFactory, TraceListener}
import org.orbeon.saxon.model.{AnyItemType, ItemType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.s9api.{HostLanguage, Location}
import org.orbeon.saxon.serialize.SerializationProperties
import org.orbeon.saxon.trace.{ExpressionPresenter, TraceableComponent}
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException, XmlProcessingException}
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.utils.{Configuration, Controller}

import java.io.OutputStream
import java.util
import java.util._
import javax.xml.transform.{Result, Source, TransformerException}
import javax.xml.transform.stream.StreamResult


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
    val visitor   = ExpressionVisitor.make(mainModule)
    val optimizer = visitor.obtainOptimizer()
    visitor.setOptimizeForStreaming(streaming)
    expression = expression.simplify()
    expression.checkForUpdatingSubexpressions()
    val contextReq = exec.getGlobalContextRequirement
    val req =
      if (contextReq == null) 
        AnyItemType
      else
        contextReq.getRequiredItemType
    val cit = config.makeContextItemStaticInfo(req, maybeUndefined = true)
    var e2  = expression.typeCheck(visitor, cit)
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
    case err: XPathException =>
      mainModule.reportStaticError(err)
      throw err
  }

  ExpressionTool.allocateSlots(expression, 0, stackFrameMap)

  ExpressionTool.computeEvaluationModesForUserFunctionCalls(expression)

  val globalMap = getPackageData.getGlobalVariableList
  if (globalMap != null)
    globalMap.forEach { gVar =>
      val top = gVar.getBody
      if (top != null)
        ExpressionTool.computeEvaluationModesForUserFunctionCalls(top)
    }

  executable.setConfiguration(config)

  def getExpression: Expression = expression

  def getBody: Expression = getExpression

  def getChildExpression: Expression = expression

  def setBody(expression: Expression): Unit = 
    this.setChildExpression(expression);

  /**
   * Get a name identifying the object of the expression, for example a function name, template name,
   * variable name, key name, element name, etc. This is used only where the name is known statically.
   *
   * @return the QName of the object declared or manipulated by this instruction or expression
   */
  def getObjectName: StructuredQName = null

  def getTracingTag: String = "query"

  def getLocation: Location = this

  /**
   * Get data about the unit of compilation (XQuery module, XSLT package) to which this
   * container belongs
   */
  def getPackageData: PackageData = mainModule.getPackageData

  /**
   * Get the Configuration to which this Container belongs
   *
   * @return the Configuration
   */
  def getConfiguration: Configuration = mainModule.getConfiguration

  def usesContextItem(): Boolean = {
    if (ExpressionTool.dependsOnFocus(expression)) 
      return true
    val map = getPackageData.getGlobalVariableList
    if (map != null) 
      map.forEach { gVar =>
        val select = gVar.getBody
        if (select != null && ExpressionTool.dependsOnFocus(select)) 
          return true
      }
    false
  }

  def isUpdateQuery: Boolean = false

  def getStackFrameMap: SlotManager = stackFrameMap

  def explainPathMap(): Unit = ()

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
  def getMainModule: QueryModule = mainModule

  /*@NotNull*/
  def getExternalVariableNames: Array[StructuredQName] = {
    val list  = stackFrameMap.getVariableMap
    val names = Array.ofDim[StructuredQName](stackFrameMap.getNumberOfVariables)
    for (i <- names.indices)
      names(i) = list.get(i)
    names
  }

  /*@NotNull*/

  def evaluate(env: DynamicQueryContext): List[Any] = {
    if (isUpdateQuery) 
      throw new XPathException("Cannot call evaluate() on an updating query")
    val list = new ArrayList[Any](100)
    iterator(env).forEachOrFail((item) =>
      list.add(SequenceTool.convertToJava(item)))
    list
  }

  /*@Nullable*/
  def evaluateSingle(env: DynamicQueryContext): Any = {
    if (isUpdateQuery) 
      throw new XPathException("Cannot call evaluateSingle() on an updating query")

    val itr  = iterator(env)
    val item = itr.next()

    if (item == null)
      null
    else
      SequenceTool.convertToJava(item)
  }

  /*@NotNull*/
  def iterator(env: DynamicQueryContext): SequenceIterator = {
    if (isUpdateQuery) 
      throw new XPathException("Cannot call iterator on an updating query")
    if (! env.getConfiguration.isCompatible(getExecutable.getConfiguration)) 
      throw new XPathException(
        "The query must be compiled and executed under the same Configuration",
        SaxonErrorCode.SXXP0004
      )
    val controller = newController(env)
    try {
      val contextItem = controller.getGlobalContextItem
      contextItem match {
        case nodeInfo: NodeInfo if !getExecutable.isSchemaAware && nodeInfo.getTreeInfo.isTyped =>
          throw new XPathException(
            "A typed input document can only be used with a schema-aware query"
          )
        case _                                                                                  =>
      }
      val context = initialContext(env, controller)
      // In tracing/debugging mode, evaluate all the global variables first
      if (controller.getTraceListener != null) 
        controller.preEvaluateGlobals(context)
      context.openStackFrame(stackFrameMap)
      val iterator = expression.iterate(context)
      if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED)) 
        iterator
      else 
        new ErrorReportingIterator(iterator, controller.getErrorReporter)
    } catch {
      case err: XPathException =>
        var terr: TransformerException = err
        while (terr.getException.isInstanceOf[TransformerException]) terr =
          terr.getException.asInstanceOf[TransformerException]
        val de = XPathException.makeXPathException(terr)
        controller.reportFatalError(de)
        throw de
    }
  }

  def run(env: DynamicQueryContext,
          result: Result,
          outputProperties: Properties): Unit = {
    if (isUpdateQuery) 
      throw new XPathException("Cannot call run() on an updating query")
    if (!env.getConfiguration.isCompatible(getExecutable.getConfiguration)) 
      throw new XPathException(
        "The query must be compiled and executed under the same Configuration",
        SaxonErrorCode.SXXP0004)
    val contextItem = env.getContextItem
    contextItem match {
      case info: NodeInfo if ! getExecutable.isSchemaAware && info.getTreeInfo.isTyped =>
        throw new XPathException(
          "A typed input document can only be used with a schema-aware query"
        )
      case _ =>
    }
    val controller: Controller = newController(env)
    result match {
      case receiver: Receiver =>
        receiver
          .getPipelineConfiguration
          .setController(controller)
      case _                  =>
    }
    val actualProperties = validateOutputProperties(controller, outputProperties)
    val context = initialContext(env, controller)
    // In tracing/debugging mode, evaluate all the global variables first
    val tracer  = controller.getTraceListener
    if (tracer != null) 
      controller.preEvaluateGlobals(context)
    context.openStackFrame(stackFrameMap)
    val mustClose = result.isInstanceOf[StreamResult] &&
      result.asInstanceOf[StreamResult].getOutputStream == null
    var out: Receiver = null
    result match {
      case receiver: Receiver =>
        out = receiver
      case _                  =>
        val sf   = context.getConfiguration.getSerializerFactory
        val pipe = controller.makePipelineConfiguration
        pipe.setHostLanguage(HostLanguage.XQUERY)
        out = sf.getReceiver(
          result,
          new SerializationProperties(actualProperties),
          pipe
        )
    }
    val dest = new ComplexContentOutputter(out)
    dest.open()
    // Run the query
    try 
      expression.process(dest, context)
    catch {
      case err: XPathException =>
        controller.reportFatalError(err)
        throw err
    } finally try {
      if (tracer != null) 
        tracer.close()
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
      val os = result.getOutputStream
      if (os != null) 
        os.close()
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
    val baseProperties = controller.getExecutable.getPrimarySerializationProperties.getProperties
    val sf             = controller.getConfiguration.getSerializerFactory
    if (outputProperties != null) {
      val iter = outputProperties.propertyNames()
      while (iter.hasMoreElements) {
        val key = iter.nextElement().asInstanceOf[String]
        var value = outputProperties.getProperty(key)
        try {
          value = sf.checkOutputProperty(key, value)
          baseProperties.setProperty(key, value)
        } catch {
          case dynamicError: XPathException =>
            outputProperties.remove(key)
            val err = new XmlProcessingException(dynamicError)
            err.setWarning(true)
            controller.getErrorReporter.report(err)
        }
      }
    }
    if (baseProperties.getProperty("method") == null) 
      // XQuery forces the default method to XML, unlike XSLT where it depends on the contents of the result tree
      baseProperties.setProperty("method", "xml")
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
    val contextItem = controller.getGlobalContextItem
    val context     = controller.newXPathContext
    if (contextItem != null) {
      val single = new ManualIterator(contextItem)
      context.setCurrentIterator(single)
      controller.setGlobalContextItem(contextItem)
    }
    context
  }

  /*@NotNull*/

  def newController(env: DynamicQueryContext): Controller = {
    val controller = new Controller(executable.getConfiguration, executable)
    env.initializeController(controller)
    //controller.getBindery(getPackageData()).allocateGlobals(executable.getGlobalVariableMap());
    controller
  }

  def explain(out: ExpressionPresenter): Unit = {
    out.startElement("query")
    getExecutable.explainGlobalVariables(out)
    mainModule.explainGlobalFunctions(out)
    out.startElement("body")
    expression.export(out)
    out.endElement()
    out.endElement()
    out.close()
  }

  def getExecutable: Executable = executable

  def setAllowDocumentProjection(allowed: Boolean): Unit =
    if (allowed) 
      throw new UnsupportedOperationException(
        "Document projection requires Saxon-EE")

  def isDocumentProjectionAllowed: Boolean = false

  /*@Nullable*/
  def getPublicId: String = null

  /*@Nullable*/
  def getSystemId: String = mainModule.getSystemId

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
  def getLineNumber: Int = -1

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
  def getColumnNumber: Int = -1

  /**
   * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
   * should not be saved for later use. The result of this operation holds the same location information,
   * but in an immutable form.
   */
  def saveLocation: Location = this

  def getHostLanguage: HostLanguage = HostLanguage.XQUERY

  def setChildExpression(expr: Expression): Unit = 
    expression = expr

  private class ErrorReportingIterator(private var base: SequenceIterator,
                                       private var reporter: ErrorReporter)
    extends SequenceIterator {

    /*@Nullable*/
    def next(): Item =
      try base.next()
      catch {
        case e1: XPathException => {
          e1.maybeSetLocation(expression.getLocation)
          val err = new XmlProcessingException(e1)
          reporter.report(err)
          e1.setHasBeenReported(true)
          throw e1
        }
      }

    override def close(): Unit = 
      base.close()
  }
}
