package org.orbeon.saxon.query

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.om.NamespaceResolver
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans._
import org.orbeon.saxon.tree.jiter.PairIterator
import org.orbeon.saxon.value.SequenceType
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import XQueryFunction._
import org.orbeon.saxon.s9api.HostLanguage.{HostLanguage, XQUERY}
import org.orbeon.saxon.trans.FunctionStreamability.{FunctionStreamability => _, _}

import scala.beans.{BeanProperty, BooleanBeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object XQueryFunction {

  def getIdentificationKey(qName: StructuredQName, arity: Int): SymbolicName =
    new SymbolicName.F(qName, arity)

}

class XQueryFunction extends Declaration with Location {

  @BeanProperty
  var functionName: StructuredQName = _

  private var arguments: List[UserFunctionParameter] = new ArrayList(8)

  @BeanProperty
  var resultType: SequenceType = _

  @BeanProperty
  var body: Expression = null

  private var references: List[UserFunctionResolvable] = new ArrayList(10)

  private var location: Location = _

  private var compiledFunction: UserFunction = null

  @BooleanBeanProperty
  var memoFunction: Boolean = _

  @BeanProperty
  var namespaceResolver: NamespaceResolver = _

  @BeanProperty
  var staticContext: QueryModule = _

  var isUpdating: Boolean = false


  var annotations: AnnotationList = AnnotationList.EMPTY

  def getPackageData: PackageData = staticContext.getPackageData

  def addArgument(argument: UserFunctionParameter): Unit = {
    arguments.add(argument)
  }

  def setLocation(location: Location): Unit = {
    this.location = location
  }

  def getDisplayName: String = functionName.getDisplayName

  def getIdentificationKey: SymbolicName =
    new SymbolicName.F(functionName, arguments.size)

  def getArgumentTypes: Array[SequenceType] = {
    val types: Array[SequenceType] = Array.ofDim[SequenceType](arguments.size)
    for (i <- 0 until arguments.size) {
      types(i) = arguments.get(i).getRequiredType
    }
    types
  }

  def getParameterDefinitions: Array[UserFunctionParameter] = {
    val params: Array[UserFunctionParameter] =
      Array.ofDim[UserFunctionParameter](arguments.size)
    arguments.toArray(params)
  }

  def getNumberOfArguments: Int = arguments.size

  def registerReference(ufc: UserFunctionResolvable): Unit = {
    references.add(ufc)
  }

  def setUpdating(isUpdating: Boolean): Unit = {
    this.isUpdating = isUpdating
  }

  def setAnnotations(annotations: AnnotationList): Unit = {
    this.annotations = annotations
    if (compiledFunction != null) {
      compiledFunction.setAnnotations(annotations)
    }
    if (annotations.includes(Annotation.UPDATING)) {
      setUpdating(true)
    }
  }

  def getAnnotations: AnnotationList = annotations

  def hasAnnotation(name: StructuredQName): Boolean =
    annotations.includes(name)

  def isPrivate: Boolean = hasAnnotation(Annotation.PRIVATE)

  def compile(): Unit = {
    val config: Configuration = staticContext.getConfiguration
    try {
      if (compiledFunction == null) {
        val map: SlotManager = config.makeSlotManager
        val params: Array[UserFunctionParameter] = getParameterDefinitions
        for (i <- 0 until params.length) {
          params(i).setSlotNumber(i)
          map.allocateSlotNumber(params(i).getVariableQName)
        }
        var rsc: RetainedStaticContext = null
        try {
          rsc = getStaticContext.makeRetainedStaticContext()
          body.setRetainedStaticContext(rsc)
          val visitor: ExpressionVisitor =
            ExpressionVisitor.make(staticContext)
          body =
            body.simplify().typeCheck(visitor, ContextItemStaticInfo.ABSENT)
          val role: RoleDiagnostic = new RoleDiagnostic(
            RoleDiagnostic.FUNCTION_RESULT,
            functionName.getDisplayName,
            0)
          body = config
            .getTypeChecker(false)
            .staticTypeCheck(body, resultType, role, visitor)
        } catch {
          case e: XPathException => {
            e.maybeSetLocation(this)
            if (e.isReportableStatically) {
              throw e
            } else {
              val newBody: Expression = new ErrorExpression(
                new XmlProcessingException(e))
              ExpressionTool.copyLocationInfo(body, newBody)
              body = newBody
            }
          }

        }
        compiledFunction = config.newUserFunction(memoFunction, FunctionStreamability.UNCLASSIFIED)
        compiledFunction.setRetainedStaticContext(rsc)
        compiledFunction.setPackageData(staticContext.getPackageData)
        compiledFunction.setBody(body)
        compiledFunction.setFunctionName(functionName)
        compiledFunction.setParameterDefinitions(params)
        compiledFunction.setResultType(getResultType)
        compiledFunction.setLineNumber(location.getLineNumber)
        compiledFunction.setSystemId(location.getSystemId)
        compiledFunction.setStackFrameMap(map)
        compiledFunction.setUpdating(isUpdating)
        compiledFunction.setAnnotations(annotations)
        if (staticContext.getUserQueryContext.isCompileWithTracing) {
          namespaceResolver = staticContext.getNamespaceResolver
          val trace: ComponentTracer = new ComponentTracer(compiledFunction)
          trace.setLocation(location)
          body = trace
        }
      }
      fixupReferences()
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(this)
        throw e
      }

    }
  }

  def optimize(): Unit = {
    body.checkForUpdatingSubexpressions()
    if (isUpdating) {
      if (ExpressionTool.isNotAllowedInUpdatingContext(body)) {
        val err = new XPathException(
          "The body of an updating function must be an updating expression",
          "XUST0002")
        err.setLocator(body.getLocation)
        throw err
      }
    } else {
      if (body.isUpdatingExpression) {
        val err = new XPathException(
          "The body of a non-updating function must be a non-updating expression",
          "XUST0001")
        err.setLocator(body.getLocation)
        throw err
      }
    }
    val visitor: ExpressionVisitor = ExpressionVisitor.make(staticContext)
    val config: Configuration = staticContext.getConfiguration
    val opt: Optimizer = visitor.obtainOptimizer()
    val arity: Int = arguments.size
    if (opt.isOptionSet(OptimizerOptions.MISCELLANEOUS)) {
      body = body.optimize(visitor, ContextItemStaticInfo.ABSENT)
    }
    body.setParentExpression(null)
    if (opt.isOptionSet(OptimizerOptions.LOOP_LIFTING)) {
      body = LoopLifter.process(body, visitor, ContextItemStaticInfo.ABSENT)
    }
    if (opt.isOptionSet(OptimizerOptions.EXTRACT_GLOBALS)) {
      val exec: Executable =
        getStaticContext.getExecutable
      val manager: GlobalVariableManager = new GlobalVariableManager() {
        def addGlobalVariable(variable: GlobalVariable): Unit = {
          var pd: PackageData = staticContext.getPackageData
          variable.setPackageData(pd)
          var sm: SlotManager = pd.getGlobalSlotManager
          var slot: Int = sm.allocateSlotNumber(variable.getVariableQName)
          variable.compile(exec, slot)
          pd.addGlobalVariable(variable)
        }

        override def getEquivalentVariable(
                                            select: Expression): GlobalVariable = null
      }
      val b2: Expression =
        opt.promoteExpressionsToGlobal(body, manager, visitor)
      if (b2 != null) {
        body = body.optimize(visitor, ContextItemStaticInfo.ABSENT)
      }
    }
    if (opt.getOptimizerOptions.isSet(OptimizerOptions.TAIL_CALLS) &&
      !isUpdating) {
      val tailCalls: Int =
        ExpressionTool.markTailFunctionCalls(body, functionName, arity)
      if (tailCalls != 0) {
        compiledFunction.setBody(body)
        compiledFunction.setTailRecursive(tailCalls > 0, tailCalls > 1)
        body = new TailCallLoop(compiledFunction, body)
      }
    }
    compiledFunction.setBody(body)
    compiledFunction.computeEvaluationMode()
    ExpressionTool.allocateSlots(body,
      arity,
      compiledFunction.getStackFrameMap)
    if (config.isGenerateByteCode(XQUERY)) {
      if (config.getCountDown == 0) {
        val compilerService: ICompilerService =
          config.makeCompilerService(XQUERY)
        val cbody: Expression = opt.compileToByteCode(
          compilerService,
          body,
          getFunctionName.getDisplayName,
          Expression.PROCESS_METHOD | Expression.ITERATE_METHOD)
        if (cbody != null) {
          body = cbody
        }
      } else {
        opt.injectByteCodeCandidates(body)
        body = opt.makeByteCodeCandidate(
          compiledFunction,
          body,
          getDisplayName,
          Expression.PROCESS_METHOD | Expression.ITERATE_METHOD)
      }
      compiledFunction.setBody(body)
      compiledFunction.computeEvaluationMode()
    }
  }

  def fixupReferences(): Unit = {
    for (ufc <- references.asScala) {
      ufc.setFunction(compiledFunction)
    }
  }

  def checkReferences(visitor: ExpressionVisitor): Unit = {
    for (ufr <- references.asScala if ufr.isInstanceOf[UserFunctionCall]) {
      val ufc: UserFunctionCall = ufr.asInstanceOf[UserFunctionCall]
      ufc.checkFunctionCall(compiledFunction, visitor)
    }
    references = new ArrayList(0)
  }

  def explain(out: ExpressionPresenter): Unit = {
    out.startElement("declareFunction")
    out.emitAttribute("name", functionName.getDisplayName)
    out.emitAttribute("arity", "" + getNumberOfArguments)
    if (compiledFunction == null) {
      out.emitAttribute("unreferenced", "true")
    } else {
      if (compiledFunction.isMemoFunction) {
        out.emitAttribute("memo", "true")
      }
      out.emitAttribute(
        "tailRecursive",
        if (compiledFunction.isTailRecursive) "true" else "false")
      body.export(out)
    }
    out.endElement()
  }

  def getUserFunction: UserFunction = compiledFunction

  def getObjectName: StructuredQName = functionName

  def getSystemId: String = location.getSystemId

  def getLineNumber: Int = location.getLineNumber

  def getPublicId: String = null

  def getColumnNumber(): Int = -1

  def saveLocation(): Location = this

  def getProperty(name: String): AnyRef =
    if ("name" == name) {
      functionName.getDisplayName
    } else if ("as" == name) {
      resultType.toString
    } else {
      null
    }

  def getProperties: Iterator[String] = new PairIterator("name", "as")

  def getHostLanguage: HostLanguage = XQUERY

}
