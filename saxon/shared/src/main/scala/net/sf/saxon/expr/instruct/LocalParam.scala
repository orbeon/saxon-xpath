package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import LocalParam._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object LocalParam {

  private val REQUIRED: Int = 4

  private val TUNNEL: Int = 8

  private val IMPLICITLY_REQUIRED: Int = 16

}

class LocalParam extends Instruction with LocalBinding {

  private var conversionOp: Operand = null

  private var conversionEvaluator: Evaluator = null

  private var properties: Byte = 0

  private var selectOp: Operand = null

  var variableQName: StructuredQName = _

  @BeanProperty
  var requiredType: SequenceType = _

  var slotNumber: Int = -999

  var referenceCount: Int = 10

  var evaluator: Evaluator = null

  def setSelectExpression(select: Expression): Unit = {
    if (select != null) {
      if (selectOp == null) {
        selectOp = new Operand(this, select, OperandRole.NAVIGATE)
      } else {
        selectOp.setChildExpression(select)
      }
    } else {
      selectOp = null
    }
    evaluator = null
  }

  def getSelectExpression(): Expression =
    if (selectOp == null) null else selectOp.getChildExpression

  def setRequiredParam(requiredParam: Boolean): Unit = {
    if (requiredParam) {
      properties = (properties | REQUIRED.toByte).toByte
    } else {
      properties = (properties & ~REQUIRED).toByte
    }
  }

  def setImplicitlyRequiredParam(requiredParam: Boolean): Unit = {
    if (requiredParam) {
      properties = (properties | IMPLICITLY_REQUIRED).toByte
    } else {
      properties = (properties & ~IMPLICITLY_REQUIRED).toByte
    }
  }

  def setTunnel(tunnel: Boolean): Unit = {
    if (tunnel) {
      properties = (properties | TUNNEL).toByte
    } else {
      properties = (properties & ~TUNNEL).toByte
    }
  }

  def setReferenceCount(refCount: Int): Unit = {
    referenceCount = refCount
  }

  override def getCardinality(): Int = StaticProperty.EMPTY

  def isAssignable(): Boolean = false

  def isGlobal(): Boolean = false

  def getLocalSlotNumber(): Int = slotNumber

  def isRequiredParam(): Boolean = (properties & REQUIRED) != 0

  def isImplicitlyRequiredParam(): Boolean =
    (properties & IMPLICITLY_REQUIRED) != 0

  def isTunnelParam(): Boolean = (properties & TUNNEL) != 0

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Expression = {
    val e2: Expression = super.typeCheck(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    checkAgainstRequiredType(visitor)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e2: Expression = super.optimize(visitor, contextItemType)
    if (e2 != this) {
      return e2
    }
    this
  }

  def computeEvaluationMode(): Unit = {
    if (getSelectExpression != null) {
      evaluator =
        if (referenceCount == FilterExpression.FILTERED)
          Evaluator.MAKE_INDEXED_VARIABLE
        else
          ExpressionTool.lazyEvaluator(getSelectExpression, referenceCount > 1)
    }
  }

  def copy(rebindings: RebindingMap): LocalParam = {
    val p2: LocalParam = new LocalParam()
    if (conversionOp != null) {
      assert(getConversion != null)
      p2.setConversion(getConversion.copy(rebindings))
    }
    p2.conversionEvaluator = conversionEvaluator
    p2.properties = properties
    if (selectOp != null) {
      assert(getSelectExpression != null)
      p2.setSelectExpression(getSelectExpression.copy(rebindings))
    }
    p2.variableQName = variableQName
    p2.requiredType = requiredType
    p2.slotNumber = slotNumber
    p2.referenceCount = referenceCount
    p2.evaluator = evaluator
    p2
  }

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = {}

  def checkAgainstRequiredType(visitor: ExpressionVisitor): Unit = {
    val role: RoleDiagnostic = new RoleDiagnostic(RoleDiagnostic.VARIABLE,
      variableQName.getDisplayName,
      0)
    val r: SequenceType = requiredType
    var select: Expression = getSelectExpression
    if (r != null && select != null) {
      select = visitor.getConfiguration
        .getTypeChecker(false)
        .staticTypeCheck(select, requiredType, role, visitor)
    }
  }

  def getSelectValue(context: XPathContext): Sequence = {
    val select: Expression = getSelectExpression
    if (select == null) {
      throw new AssertionError("Internal error: No select expression")
    } else if (select.isInstanceOf[Literal]) {
      select.asInstanceOf[Literal].value
    } else {
      val savedOutputState: Int = context.getTemporaryOutputState
      context.setTemporaryOutputState(StandardNames.XSL_WITH_PARAM)
      var result: Sequence = null
      val eval: Evaluator =
        if (evaluator == null) Evaluator.EAGER_SEQUENCE else evaluator
      result = eval.evaluate(select, context)
      context.setTemporaryOutputState(savedOutputState)
      result
    }
  }

  def getSlotNumber(): Int = slotNumber

  def setSlotNumber(s: Int): Unit = {
    slotNumber = s
  }

  def setVariableQName(s: StructuredQName): Unit = {
    variableQName = s
  }

  def getVariableQName(): StructuredQName = variableQName

  def setConversion(convertor: Expression): Unit = {
    if (convertor != null) {
      if (conversionOp == null) {
        conversionOp = new Operand(this, convertor, OperandRole.SINGLE_ATOMIC)
      }
      conversionEvaluator = ExpressionTool.eagerEvaluator(convertor)
    } else {
      conversionOp = null
    }
  }

  def getConversion(): Expression =
    if (conversionOp == null) null else conversionOp.getChildExpression

  def getConversionEvaluationMode(): EvaluationMode.EvaluationMode =
    conversionEvaluator.getEvaluationMode

  override def getInstructionNameCode(): Int = StandardNames.XSL_PARAM

  override def operands(): java.lang.Iterable[Operand] =
    operandSparseList(selectOp, conversionOp)

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val wasSupplied: Int =
      context.useLocalParameter(variableQName, slotNumber, isTunnelParam)
    wasSupplied match {
      case ParameterSet.SUPPLIED_AND_CHECKED =>
      case ParameterSet.SUPPLIED =>
        if (conversionOp != null) {
          context.setLocalVariable(
            slotNumber,
            conversionEvaluator.evaluate(getConversion, context))
        }
      case ParameterSet.NOT_SUPPLIED =>
        if (isRequiredParam || isImplicitlyRequiredParam) {
          val name: String = "$" + getVariableQName.getDisplayName
          val suppliedAsTunnel: Int = context.useLocalParameter(variableQName,
            slotNumber,
            !isTunnelParam)
          var message: String = "No value supplied for required parameter " + name
          if (isImplicitlyRequiredParam) {
            message += ". A value is required because " +
              "the default value is not a valid instance of the required type"
          }
          if (suppliedAsTunnel != ParameterSet.NOT_SUPPLIED) {
            if (isTunnelParam) {
              message += ". A non-tunnel parameter with this name was supplied, but a tunnel parameter is required"
            } else {
              message += ". A tunnel parameter with this name was supplied, but a non-tunnel parameter is required"
            }
          }
          val e: XPathException = new XPathException(message)
          e.setXPathContext(context)
          e.setErrorCode("XTDE0700")
          throw e
        }
        context.setLocalVariable(slotNumber, getSelectValue(context))

    }
    null
  }

  def getIntegerBoundsForVariable(): Array[IntegerValue] = null

  def evaluateVariable(c: XPathContext): Sequence =
    c.evaluateLocalVariable(slotNumber)

  def isCompatible(other: LocalParam): Boolean =
    getVariableQName == other.getVariableQName && getRequiredType == other.getRequiredType &&
      isTunnelParam == other.isTunnelParam

  override def isLiftable(forStreaming: Boolean): Boolean = false

  override def hasVariableBinding(binding: Binding): Boolean = this == binding

  override def getItemType(): ItemType = ErrorType.getInstance

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  override def computeSpecialProperties(): Int =
    StaticProperty.HAS_SIDE_EFFECTS

  override def mayCreateNewNodes(): Boolean = false

  override def getExpressionName(): String = "param"

  override def toShortString(): String = "$" + getVariableQName.getDisplayName

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("param", this)
    out.emitAttribute("name", getVariableQName)
    out.emitAttribute("slot", "" + getSlotNumber)
    val flags: String = getFlags
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    val options: ExpressionPresenter.ExportOptions =
      out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
    if (getRequiredType != SequenceType.ANY_SEQUENCE) {
      out.emitAttribute("as", getRequiredType.toAlphaCode)
    }
    if (getSelectExpression != null) {
      out.setChildRole("select")
      getSelectExpression.export(out)
    }
    val conversion: Expression = getConversion
    if (conversion != null) {
      out.setChildRole("conversion")
      conversion.export(out)
    }
    out.endElement()
  }

  private def getFlags(): String = {
    var flags: String = ""
    if (isTunnelParam) {
      flags += "t"
    }
    if (isRequiredParam) {
      flags += "r"
    }
    if (isImplicitlyRequiredParam) {
      flags += "i"
    }
    flags
  }

  override def setIndexedVariable(): Unit = {}

  override def isIndexedVariable(): Boolean = false

}
