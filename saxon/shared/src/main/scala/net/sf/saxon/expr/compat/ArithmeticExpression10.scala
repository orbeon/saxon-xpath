package net.sf.saxon.expr.compat

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.Choose

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.Number_1

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model._

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

class ArithmeticExpression10(p0: Expression, operator: Int, p1: Expression)
  extends ArithmeticExpression(p0, operator, p1)
    with Callable {

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {

    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)

    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy

    if (Literal.isEmptySequence(getLhsExpression))
      Literal.makeLiteral(DoubleValue.NaN, this)
    if (Literal.isEmptySequence(getRhsExpression))
      Literal.makeLiteral(DoubleValue.NaN, this)

    val oldOp0 = getLhsExpression
    val oldOp1 = getRhsExpression

    val atomicType = SequenceType.OPTIONAL_ATOMIC
    val tc = visitor.getConfiguration.getTypeChecker(true)
    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(operator), 0)
    this.setLhsExpression(tc.staticTypeCheck(getLhsExpression, atomicType, role0, visitor))
    val role1 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(operator), 1)
    this.setRhsExpression(tc.staticTypeCheck(getRhsExpression, atomicType, role1, visitor))

    val itemType0 = getLhsExpression.getItemType
    if (itemType0 eq ErrorType)
      Literal.makeLiteral(DoubleValue.NaN, this)

    var type0 = itemType0.getPrimitiveItemType.asInstanceOf[AtomicType]
    val itemType1 = getRhsExpression.getItemType
    if (itemType1 eq ErrorType)
      Literal.makeLiteral(DoubleValue.NaN, this)

    var type1 = itemType1.getPrimitiveItemType.asInstanceOf[AtomicType]

    if (th.isSubType(type0, BuiltInAtomicType.INTEGER) && th.isSubType(
      type1,
      BuiltInAtomicType.INTEGER) &&
      (operator == Token.PLUS || operator == Token.MINUS || operator == Token.MULT)) {
      val arith: ArithmeticExpression =
        new ArithmeticExpression(getLhsExpression, operator, getRhsExpression)
      val n: Expression =
        SystemFunction.makeCall("number", getRetainedStaticContext, arith)
      n.typeCheck(visitor, contextInfo)
    }
    if (calculator == null) {
      this.setLhsExpression(
        createConversionCode(getLhsExpression, config, type0))
    }
    type0 = getLhsExpression.getItemType.getPrimitiveItemType
      .asInstanceOf[AtomicType]
    if (calculator == null) {
      this.setRhsExpression(
        createConversionCode(getRhsExpression, config, type1))
    }
    type1 = getRhsExpression.getItemType.getPrimitiveItemType
      .asInstanceOf[AtomicType]
    if (getLhsExpression != oldOp0) {
      adoptChildExpression(getLhsExpression)
    }
    if (getRhsExpression != oldOp1) {
      adoptChildExpression(getRhsExpression)
    }
    if (operator == Token.NEGATE) {
      getRhsExpression match {
        case literal: Literal =>
          val v: GroundedValue = literal.getValue
          v match {
            case value: NumericValue =>
              Literal.makeLiteral(value.negate(), this)
            case _ =>
          }
        case _ =>
      }
      val ne: NegateExpression = new NegateExpression(getRhsExpression)
      ne.setBackwardsCompatible(true)
      ne.typeCheck(visitor, contextInfo)
    }
    val mustResolve: Boolean =
      !(type0 == BuiltInAtomicType.ANY_ATOMIC || type1 == BuiltInAtomicType.ANY_ATOMIC ||
        type0 == NumericType.getInstance ||
        type1 == NumericType.getInstance)
    calculator = assignCalculator(type0, type1, mustResolve)
    try
      if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal]) {
        Literal.makeLiteral(
          evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext())
            .materialize(),
          this)
      }
    catch {
      case _: XPathException =>
    }
    this
  }

  override def setCalculator(calc: Calculator): Unit = {
    this.calculator = calc
  }

  private def assignCalculator(type0: AtomicType,
                               type1: AtomicType,
                               mustResolve: Boolean): Calculator = {
    val calculator: Calculator = Calculator.getCalculator(
      type0.getFingerprint,
      type1.getFingerprint,
      ArithmeticExpression.mapOpCode(operator),
      mustResolve)
    if (calculator == null) {
      val de: XPathException = new XPathException(
        "Arithmetic operator is not defined for arguments of types (" +
          type0.getDescription +
          ", " +
          type1.getDescription +
          ")")
      de.setLocation(getLocation)
      de.setErrorCode("XPTY0004")
      throw de
    }
    calculator
  }

  private def createConversionCode(operand: Expression,
                                   config: Configuration,
                                   `type`: AtomicType): Expression = {
    var expr = operand
    val th: TypeHierarchy = config.getTypeHierarchy
    if (Cardinality.allowsMany(expr.getCardinality)) {
      val fie: Expression =
        FirstItemExpression.makeFirstItemExpression(expr)
      ExpressionTool.copyLocationInfo(this, fie)
      expr = fie
    }
    if (th.isSubType(`type`, BuiltInAtomicType.DOUBLE) || th.isSubType(
      `type`,
      BuiltInAtomicType.DATE) ||
      th.isSubType(`type`, BuiltInAtomicType.TIME) ||
      th.isSubType(`type`, BuiltInAtomicType.DATE_TIME) ||
      th.isSubType(`type`, BuiltInAtomicType.DURATION)) {
      return expr
    }
    if (
      th.isSubType(`type`, BuiltInAtomicType.BOOLEAN) || th.isSubType(`type`, BuiltInAtomicType.STRING) ||
      th.isSubType(`type`, BuiltInAtomicType.UNTYPED_ATOMIC) ||
      th.isSubType(`type`, BuiltInAtomicType.FLOAT) ||
      th.isSubType(`type`, BuiltInAtomicType.DECIMAL)
    ) {
      return {
        if (expr.isInstanceOf[Literal]) {
          val `val`: GroundedValue = expr.asInstanceOf[Literal].getValue
          Literal.makeLiteral(Number_1.convert(`val`.asInstanceOf[AtomicValue], config), this)
        } else {
          SystemFunction.makeCall("number", getRetainedStaticContext, expr)
        }
      }
    }
    val let: LetExpression = new LetExpression()
    let.setRequiredType(SequenceType.OPTIONAL_ATOMIC)
    let.setVariableQName(
      new StructuredQName("nn", NamespaceConstant.SAXON, "nn" + let.hashCode))
    let.setSequence(expr)
    var `var`: LocalVariableReference = new LocalVariableReference(let)
    val isDouble: Expression =
      new InstanceOfExpression(`var`, BuiltInAtomicType.DOUBLE.zeroOrOne())
    `var` = new LocalVariableReference(let)
    val isDecimal: Expression =
      new InstanceOfExpression(`var`, BuiltInAtomicType.DECIMAL.zeroOrOne())
    `var` = new LocalVariableReference(let)
    val isFloat: Expression =
      new InstanceOfExpression(`var`, BuiltInAtomicType.FLOAT.zeroOrOne())
    `var` = new LocalVariableReference(let)
    val isString: Expression =
      new InstanceOfExpression(`var`, BuiltInAtomicType.STRING.zeroOrOne())
    `var` = new LocalVariableReference(let)
    val isUntypedAtomic: Expression = new InstanceOfExpression(
      `var`,
      BuiltInAtomicType.UNTYPED_ATOMIC.zeroOrOne())
    `var` = new LocalVariableReference(let)
    val isBoolean: Expression =
      new InstanceOfExpression(`var`, BuiltInAtomicType.BOOLEAN.zeroOrOne())
    var condition: Expression = new OrExpression(isDouble, isDecimal)
    condition = new OrExpression(condition, isFloat)
    condition = new OrExpression(condition, isString)
    condition = new OrExpression(condition, isUntypedAtomic)
    condition = new OrExpression(condition, isBoolean)
    `var` = new LocalVariableReference(let)
    val fn: Expression =
      SystemFunction.makeCall("number", getRetainedStaticContext, `var`)
    `var` = new LocalVariableReference(let)
    `var`.setStaticType(SequenceType.SINGLE_ATOMIC, null, 0)
    val action: Expression = Choose.makeConditional(condition, fn, `var`)
    let.setAction(action)
    let
  }

  override def getItemType(): PlainType =
    if (calculator == null) {
      BuiltInAtomicType.ANY_ATOMIC
    } else {
      var t1: ItemType = getLhsExpression.getItemType
      if (!t1.isInstanceOf[AtomicType]) {
        t1 = t1.getAtomizedItemType
      }
      var t2: ItemType = getRhsExpression.getItemType
      if (!t2.isInstanceOf[AtomicType]) {
        t2 = t2.getAtomizedItemType
      }
      calculator.getResultType(
        t1.getPrimitiveItemType.asInstanceOf[AtomicType],
        t2.getPrimitiveItemType.asInstanceOf[AtomicType])
    }

  override def copy(rebindings: RebindingMap): Expression = {
    val a2: ArithmeticExpression10 = new ArithmeticExpression10(
      getLhsExpression.copy(rebindings),
      operator,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, a2)
    a2.calculator = calculator
    a2
  }

  override def tag(): String = "arith10"

  override def explainExtraAttributes(out: ExpressionPresenter): Unit =
    out.emitAttribute("calc", calculator.code())

  override def evaluateItem(context: XPathContext): AtomicValue = {
    var calc: Calculator = calculator
    val v1 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v1 == null) {
      return DoubleValue.NaN
    }
    val v2: AtomicValue =
      getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v2 == null) {
      return DoubleValue.NaN
    }
    if (calc == null) {
      calc = assignCalculator(v1.getPrimitiveType, v2.getPrimitiveType, mustResolve = true)
    }
    calc.compute(v1, v2, context)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): AtomicValue = {
    var calc: Calculator = calculator
    val v1: AtomicValue = arguments(0).head().asInstanceOf[AtomicValue]
    if (v1 == null) {
      return DoubleValue.NaN
    }
    val v2: AtomicValue = arguments(1).head().asInstanceOf[AtomicValue]
    if (v2 == null) {
      return DoubleValue.NaN
    }
    if (calc == null) {
      calc = assignCalculator(v1.getPrimitiveType, v2.getPrimitiveType, mustResolve = true)
    }
    calc.compute(v1, v2, context)
  }

}
