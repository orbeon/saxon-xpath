package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller
import net.sf.saxon.event.Outputter
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.expr.sort.AtomicComparer
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trace.TraceableComponent
import net.sf.saxon.trans.FunctionStreamability
import net.sf.saxon.trans.SymbolicName
import net.sf.saxon.trans.Visibility
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.EmptySequence
import net.sf.saxon.value.SequenceType

import scala.jdk.CollectionConverters._
import java.util.function.BiConsumer

import UserFunction._
import FunctionStreamability._
import net.sf.saxon.query.AnnotationList

import scala.beans.{BeanProperty, BooleanBeanProperty}

object UserFunction {

  object Determinism extends Enumeration {

    val DETERMINISTIC: Determinism = new Determinism()

    val PROACTIVE: Determinism = new Determinism()

    val ELIDABLE: Determinism = new Determinism()

    class Determinism extends Val

    implicit def convertValue(v: Value): Determinism =
      v.asInstanceOf[Determinism]

  }

  private val MAX_INLININGS: Int = 100

  private def containsUserFunctionCalls(exp: Expression): Boolean = {
    if (exp.isInstanceOf[UserFunctionCall]) {
      return true
    }
    for (o <- exp.operands().asScala
         if containsUserFunctionCalls(o.getChildExpression)) {
      true
    }
    false
  }

}

class UserFunction
  extends Actor
    with Function
    with ContextOriginator
    with TraceableComponent {

  @BeanProperty
  var functionName: StructuredQName = _

  private var tailCalls: Boolean = false

  @BooleanBeanProperty
  var tailRecursive: Boolean = false

  @BeanProperty
  var parameterDefinitions: Array[UserFunctionParameter] = _

  private var resultType: SequenceType = _

  @BeanProperty
  var declaredResultType: SequenceType = _

   var evaluator: Evaluator = null

  var isUpdating: Boolean = false

  private var inlineable: Int = -1

  private var inliningCount: Int = 0

  @BooleanBeanProperty
  var overrideExtensionFunction: Boolean = true

  @BeanProperty
  var annotations: AnnotationList = AnnotationList.EMPTY

  @BeanProperty
  var declaredStreamability: FunctionStreamability.FunctionStreamability = FunctionStreamability.UNCLASSIFIED

  @BeanProperty
  var determinism: Determinism.Determinism = Determinism.PROACTIVE

  private var refCount: Int = 0

  def getDescription(): String = {
    val name: StructuredQName = getFunctionName
    if (name.hasURI(NamespaceConstant.ANONYMOUS)) {
      var first: Boolean = true
      val sb: StringBuilder = new StringBuilder("function(")
      for (param <- getParameterDefinitions) {
        if (first) {
          first = false
        } else {
          sb.append(", ")
        }
        sb.append("$").append(param.getVariableQName.getDisplayName)
      }
      sb.append("){")
      val body: Expression = getBody
      if (body == null) {
        sb.append("...")
      } else {
        sb.append(body.toShortString())
      }
      sb.append("}")
      sb.toString
    } else {
      name.getDisplayName
    }
  }

  override def getTracingTag(): String = "xsl:function"

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    consumer.accept("name", getFunctionName)
    consumer.accept("arity", getArity)
  }

  def getObjectName(): StructuredQName = functionName

  def getSymbolicName(): SymbolicName.F =
    new SymbolicName.F(functionName, getArity)

  def getFunctionItemType(): FunctionItemType = {
    val argTypes: Array[SequenceType] =
      Array.ofDim[SequenceType](parameterDefinitions.length)
    for (i <- 0 until parameterDefinitions.length) {
      val ufp: UserFunctionParameter = parameterDefinitions(i)
      argTypes(i) = ufp.getRequiredType
    }
    new SpecificFunctionType(argTypes, resultType, annotations)
  }

  def getOperandRoles(): Array[OperandRole] = {
    val roles: Array[OperandRole] = Array.ofDim[OperandRole](getArity)
    var first: OperandUsage.OperandUsage = null
    declaredStreamability match {
      case UNCLASSIFIED =>
        var required: SequenceType = getArgumentType(0)
        first = OperandRole.getTypeDeterminedUsage(required.getPrimaryType)
      case ABSORBING => first = OperandUsage.ABSORPTION
      case INSPECTION => first = OperandUsage.INSPECTION
      case FILTER => first = OperandUsage.TRANSMISSION
      case SHALLOW_DESCENT => first = OperandUsage.TRANSMISSION
      case DEEP_DESCENT => first = OperandUsage.TRANSMISSION
      case ASCENT => first = OperandUsage.TRANSMISSION

    }
    roles(0) = new OperandRole(0, first, getArgumentType(0))
    for (i <- 1 until roles.length) {
      val required: SequenceType = getArgumentType(i)
      roles(i) = new OperandRole(
        0,
        OperandRole.getTypeDeterminedUsage(required.getPrimaryType),
        required)
    }
    roles
  }

  def acceptsNodesWithoutAtomization(): Boolean = {
    for (i <- 0 until getArity) {
      val `type`: ItemType = getArgumentType(i).getPrimaryType
      if (`type`.isInstanceOf[NodeTest] || `type` == AnyItemType.getInstance) {
        true
      }
    }
    false
  }

  def computeEvaluationMode(): Unit = {
    evaluator =
      if (tailRecursive) ExpressionTool.eagerEvaluator(getBody)
      else ExpressionTool.lazyEvaluator(getBody, repeatable = true)
  }

  def isInlineable(): java.lang.Boolean = {
    if (inlineable != -1) {
      return inlineable > 0 && inliningCount < MAX_INLININGS
    }
    if (body == null) {
      return null
    }
    if (body.hasSpecialProperty(StaticProperty.HAS_SIDE_EFFECTS) ||
      tailCalls) {
      return false
    }
    val component: Component = getDeclaringComponent
    if (component != null) {
      val visibility: Visibility.Visibility = getDeclaringComponent.getVisibility
      if (visibility == Visibility.PRIVATE || visibility == Visibility.FINAL) {
        if (inlineable < 0) {
          null
        } else {
          inlineable > 0
        }
      } else {
        false
      }
    } else {
      null
    }
  }

  def setInlineable(inlineable: Boolean): Unit = {
    this.inlineable = if (inlineable) 1 else 0
  }

  def markAsInlined(): Unit = {
    {
      inliningCount += 1;
    }
  }

  def setResultType(resultType: SequenceType): Unit = {
    this.declaredResultType = resultType
    this.resultType = resultType
  }

  def setTailRecursive(tailCalls: Boolean, recursiveTailCalls: Boolean): Unit = {
    this.tailCalls = tailCalls
    tailRecursive = recursiveTailCalls
  }

  def containsTailCalls(): Boolean = tailCalls

  def setUpdating(isUpdating: Boolean): Unit = {
    this.isUpdating = isUpdating
  }

  def getResultType(): SequenceType = {
    if (resultType == SequenceType.ANY_SEQUENCE && getBody != null) {
      if (!containsUserFunctionCalls(getBody)) {
        resultType = SequenceType.makeSequenceType(getBody.getItemType,
          getBody.getCardinality)
      }
    }
    resultType
  }

  def getArgumentType(n: Int): SequenceType =
    parameterDefinitions(n).getRequiredType

  def getEvaluator(): Evaluator = {
    if (evaluator == null) {
      computeEvaluationMode()
    }
    evaluator
  }

  def setEvaluationMode(mode: EvaluationMode.EvaluationMode): Unit = {
    evaluator = mode.getEvaluator
  }

  def getArity(): Int = parameterDefinitions.length

  def isMemoFunction(): Boolean = false

  def typeCheck(visitor: ExpressionVisitor): Unit = {
    val exp: Expression = getBody
    if (exp.isInstanceOf[ValueOf] &&
      exp.asInstanceOf[ValueOf].getSelect.getItemType.isAtomicType &&
      declaredResultType.getPrimaryType.isAtomicType &&
      declaredResultType.getPrimaryType != BuiltInAtomicType.STRING) {
      visitor.getStaticContext.issueWarning(
        "A function that computes atomic values should use xsl:sequence rather than xsl:value-of",
        getLocation)
    }
    ExpressionTool.resetPropertiesWithinSubtree(exp)
    var exp2: Expression = exp
    try {
      val info: ContextItemStaticInfo = ContextItemStaticInfo.ABSENT
      exp2 = exp.typeCheck(visitor, info)
      if (resultType != null) {
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.FUNCTION_RESULT,
          if (functionName == null) ""
          else functionName.getDisplayName + "#" + getArity,
          0)
        role.setErrorCode(
          if (getPackageData.isXSLT && getFunctionName != null) "XTTE0780"
          else "XPTY0004")
        exp2 = visitor.getConfiguration
          .getTypeChecker(false)
          .staticTypeCheck(exp2, resultType, role, visitor)
      }
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

    }
    if (exp2 != exp) {
      this.body = exp2
    }
  }

  def makeNewContext(oldContext: XPathContext,
                     originator: ContextOriginator): XPathContextMajor = {
    val c2: XPathContextMajor = oldContext.newCleanContext()
    c2.setTemporaryOutputState(StandardNames.XSL_FUNCTION)
    c2.setCurrentOutputUri(null)
    c2.setCurrentComponent(getDeclaringComponent)
    c2.setOrigin(originator)
    c2
  }

  def call(context: XPathContext, actualArgs: Array[Sequence]): Sequence = {
    if (evaluator == null) {
      computeEvaluationMode()
    }
    val c2: XPathContextMajor = context.asInstanceOf[XPathContextMajor]
    c2.setStackFrame(getStackFrameMap, actualArgs)
    var result: Sequence = null
    try result = evaluator.evaluate(getBody, c2)
    catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        err.maybeSetContext(c2)
        throw err
      }

      case err2: Exception => {
        val message: String = "Internal error evaluating function " +
          (if (functionName == null) "(unnamed)"
          else functionName.getDisplayName) +
          (if (getLineNumber > 0) " at line " + getLineNumber else "") +
          (if (getSystemId != null) " in module " + getSystemId else "")
        throw new RuntimeException(message, err2)
      }

    }
    result
  }

  def process(context: XPathContextMajor,
              actualArgs: Array[Sequence],
              output: Outputter): Unit = {
    context.setStackFrame(getStackFrameMap, actualArgs)
    getBody.process(output, context)
  }

  def call(actualArgs: Array[Sequence], controller: Controller): Sequence =
    call(controller.newXPathContext, actualArgs)

  def callUpdating(actualArgs: Array[Sequence],
                   context: XPathContextMajor,
                   pul: PendingUpdateList): Unit = {
    context.setStackFrame(getStackFrameMap, actualArgs)
    try getBody.evaluatePendingUpdates(context, pul)
    catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

    }
  }

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("function")
    if (getFunctionName != null) {
      presenter.emitAttribute("name", getFunctionName)
      presenter.emitAttribute("line", getLineNumber.toString())
      presenter.emitAttribute("module", getSystemId)
      presenter.emitAttribute("eval",
        getEvaluator.getEvaluationMode.getCode.toString)
    }
    var flags: String = ""
    if (determinism == Determinism.PROACTIVE) {
      flags += "p"
    } else if (determinism == Determinism.ELIDABLE) {
      flags += "e"
    } else {
      flags += "d"
    }
    if (isMemoFunction) {
      flags += "m"
    }
    declaredStreamability match {
      case UNCLASSIFIED => flags += "U"
      case ABSORBING => flags += "A"
      case INSPECTION => flags += "I"
      case FILTER => flags += "F"
      case SHALLOW_DESCENT => flags += "S"
      case DEEP_DESCENT => flags += "D"
      case ASCENT => flags += "C"

    }
    presenter.emitAttribute("flags", flags)
    presenter.emitAttribute("as", getDeclaredResultType.toAlphaCode)
    presenter.emitAttribute("slots",
      getStackFrameMap.getNumberOfVariables.toString)
    for (p <- parameterDefinitions) {
      presenter.startElement("arg")
      presenter.emitAttribute("name", p.getVariableQName)
      presenter.emitAttribute("as", p.getRequiredType.toAlphaCode)
      presenter.endElement()
    }
    presenter.setChildRole("body")
    getBody.export(presenter)
    presenter.endElement()
  }

  override def isExportable(): Boolean =
    refCount > 0 ||
      (getDeclaredVisibility != null && getDeclaredVisibility != Visibility.PRIVATE) ||
      getPackageData.asInstanceOf[StylesheetPackage].isRetainUnusedFunctions

  def isTrustedResultType(): Boolean = false

  def isMap(): Boolean = false

  def isArray(): Boolean = false

  def deepEquals(other: Function,
                 context: XPathContext,
                 comparer: AtomicComparer,
                 flags: Int): Boolean = {
    val err: XPathException = new XPathException(
      "Cannot compare functions using deep-equal",
      "FOTY0015")
    err.setIsTypeError(true)
    err.setXPathContext(context)
    throw err
  }

  override def itemAt(n: Int): Function = if (n == 0) this else null

  override def subsequence(start: Int, length: Int): GroundedValue =
    if (start <= 0 && (start + length) > 0) this else EmptySequence.getInstance

  override def getLength(): Int = 1

  override def effectiveBooleanValue(): Boolean =
    ExpressionTool.effectiveBooleanValue(this)

  override def reduce(): UserFunction = this

  override def head(): UserFunction = this

  def getStringValue(): String =
    throw new UnsupportedOperationException("A function has no string value")

  def getStringValueCS(): CharSequence = getStringValue

  def atomize(): AtomicSequence =
    throw new XPathException("Functions cannot be atomized", "FOTY0013")

  def incrementReferenceCount(): Unit = refCount += 1


  def getReferenceCount(): Int = refCount

  def prepareForStreaming(): Unit = ()

}
