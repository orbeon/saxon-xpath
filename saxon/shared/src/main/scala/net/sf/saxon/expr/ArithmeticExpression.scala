package net.sf.saxon.expr

import net.sf.saxon.expr.ArithmeticExpression._
import net.sf.saxon.expr.parser._
import net.sf.saxon.model._
import net.sf.saxon.om.StandardNames
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value._


object ArithmeticExpression {

  def compute(value0: AtomicValue,
              operator: Int,
              value1: AtomicValue,
              context: XPathContext): AtomicValue = {
    val p0: Int = value0.getPrimitiveType.getFingerprint
    val p1: Int = value1.getPrimitiveType.getFingerprint
    val calculator: Calculator =
      Calculator.getCalculator(p0, p1, operator, false)
    calculator.compute(value0, value1, context)
  }

  def mapOpCode(op: Int): Int = op match {
    case Token.PLUS => Calculator.PLUS
    case Token.MINUS | Token.NEGATE => Calculator.MINUS
    case Token.MULT => Calculator.TIMES
    case Token.DIV => Calculator.DIV
    case Token.IDIV => Calculator.IDIV
    case Token.MOD => Calculator.MOD
    case _ => throw new IllegalArgumentException
  }
}

class ArithmeticExpression(p0: Expression, operator: Int, p1: Expression) extends BinaryExpression(p0, operator, p1) {

   var calculator: Calculator = _

  private var itemType: PlainType = _

  override def getExpressionName(): String = "arithmetic"

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NOT_UNTYPED_ATOMIC
  }

  def setCalculator(calculator: Calculator): Unit = {
    this.calculator = calculator
  }

  def getCalculator(): Calculator = calculator

  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {

    resetLocalStaticProperties()
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)

    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy
    val tc = config.getTypeChecker(false)

    val oldOp0 = getLhsExpression
    val oldOp1 = getRhsExpression

    val atomicType = SequenceType.OPTIONAL_ATOMIC
    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(operator), 0)

    setLhsExpression(tc.staticTypeCheck(getLhsExpression, atomicType, role0, visitor))

    val itemType0 = getLhsExpression.getItemType
    if (itemType0.isInstanceOf[ErrorType])
      return Literal.makeEmptySequence()

    var type0 =
      itemType0.getPrimitiveItemType.asInstanceOf[AtomicType]

    if (type0.getFingerprint == StandardNames.XS_UNTYPED_ATOMIC) {
      setLhsExpression(
        UntypedSequenceConverter.makeUntypedSequenceConverter(
          config,
          getLhsExpression,
          BuiltInAtomicType.DOUBLE))
      type0 = BuiltInAtomicType.DOUBLE
    } else if ((getLhsExpression.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC) == 0 &&
      th.relationship(type0, BuiltInAtomicType.UNTYPED_ATOMIC) != Affinity.DISJOINT) {
      setLhsExpression(
        UntypedSequenceConverter.makeUntypedSequenceConverter(
          config,
          getLhsExpression,
          BuiltInAtomicType.DOUBLE))
      type0 = getLhsExpression.getItemType.getPrimitiveItemType
        .asInstanceOf[AtomicType]
    }

    val role1 =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, Token.tokens(operator), 1)

    setRhsExpression(tc.staticTypeCheck(getRhsExpression, atomicType, role1, visitor))

    val itemType1 = getRhsExpression.getItemType
    if (itemType1.isInstanceOf[ErrorType])
      return Literal.makeEmptySequence()

    var type1 =
      itemType1.getPrimitiveItemType.asInstanceOf[AtomicType]

    if (type1.getFingerprint == StandardNames.XS_UNTYPED_ATOMIC) {
      setRhsExpression(
        UntypedSequenceConverter.makeUntypedSequenceConverter(
          config,
          getRhsExpression,
          BuiltInAtomicType.DOUBLE))
      type1 = BuiltInAtomicType.DOUBLE
    } else if ((getRhsExpression.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC) ==
      0 &&
      th.relationship(type1, BuiltInAtomicType.UNTYPED_ATOMIC) !=
        Affinity.DISJOINT) {
      setRhsExpression(
        UntypedSequenceConverter.makeUntypedSequenceConverter(
          config,
          getRhsExpression,
          BuiltInAtomicType.DOUBLE))
      type1 = getRhsExpression.getItemType.getPrimitiveItemType
        .asInstanceOf[AtomicType]
    }

    if (itemType0.getUType.union(itemType1.getUType).overlaps(UType.EXTENSION)) {
      val de = new XPathException("Arithmetic operators are not defined for external objects")
      de.setLocation(getLocation)
      de.setErrorCode("XPTY0004")
      throw de
    }

    if (getLhsExpression != oldOp0)
      adoptChildExpression(getLhsExpression)
    if (getRhsExpression != oldOp1)
      adoptChildExpression(getRhsExpression)
    if (Literal.isEmptySequence(getLhsExpression) || Literal.isEmptySequence(getRhsExpression))
      return Literal.makeEmptySequence()

    if (operator == Token.NEGATE) {
      if (getRhsExpression.isInstanceOf[Literal] &&
        getRhsExpression
          .asInstanceOf[Literal]
          .getValue
          .isInstanceOf[NumericValue]) {
        val nv = getRhsExpression
          .asInstanceOf[Literal]
          .getValue
          .asInstanceOf[NumericValue]
        return Literal.makeLiteral(nv.negate(), this)
      } else {
        val ne = new NegateExpression(getRhsExpression)
        ne.setBackwardsCompatible(false)
        return ne.typeCheck(visitor, contextInfo)
      }
    }

    val mustResolve =
      !(type0 == BuiltInAtomicType.ANY_ATOMIC || type1 == BuiltInAtomicType.ANY_ATOMIC ||
        type0 == NumericType.getInstance ||
        type1 == NumericType.getInstance)

    calculator = Calculator.getCalculator(
      type0.getFingerprint,
      type1.getFingerprint,
      mapOpCode(operator),
      mustResolve
    )

    if (calculator == null) {
      val de = new XPathException(
        "Arithmetic operator is not defined for arguments of types (" +
          type0.getDescription +
          ", " +
          type1.getDescription +
          ")")
      de.setLocation(getLocation)
      de.setIsTypeError(true)
      de.setErrorCode("XPTY0004")
      throw de
    }
    if (calculator.code().matches("d.d")) {
      if (getLhsExpression
        .isInstanceOf[Literal] && type0 != BuiltInAtomicType.DOUBLE) {
        val value =
          getLhsExpression.asInstanceOf[Literal].getValue
        if (value.isInstanceOf[NumericValue]) {
          setLhsExpression(Literal.makeLiteral(
            new DoubleValue(value.asInstanceOf[NumericValue].getDoubleValue),
            this))
        }
      }
      if (getRhsExpression
        .isInstanceOf[Literal] && type1 != BuiltInAtomicType.DOUBLE) {
        val value =
          getRhsExpression.asInstanceOf[Literal].getValue
        if (value.isInstanceOf[NumericValue]) {
          setLhsExpression(Literal.makeLiteral(
            new DoubleValue(value.asInstanceOf[NumericValue].getDoubleValue),
            this))
        }
      }
    }
    try if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal]) {
      return Literal.makeLiteral(evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext()).materialize(), this)
    } catch {
      case _: XPathException =>
        // if early evaluation fails, suppress the error: the value might
        // not be needed at run-time, or it might be due to context such as the implicit timezone
        // not being available yet
    }
    this
  }

  override def getIntegerBounds(): Array[IntegerValue] = {
    val bounds0: Array[IntegerValue] = getLhsExpression.getIntegerBounds
    val bounds1: Array[IntegerValue] = getRhsExpression.getIntegerBounds
    if (bounds0 == null || bounds1 == null) {
      null
    } else {
      operator match {
        case Token.PLUS =>
          Array(bounds0(0).plus(bounds1(0)), bounds0(1).plus(bounds1(1)))
        case Token.MINUS =>
          Array(bounds0(0).minus(bounds1(1)), bounds0(1).minus(bounds1(0)))
        case Token.MULT =>
          if (getRhsExpression.isInstanceOf[Literal]) {
            val val1  = bounds1(0)
            if (val1.signum() > 0) {
              Array(bounds0(0).times(val1), bounds0(1).times(val1))
            } else {
              null
            }
          } else if (getLhsExpression.isInstanceOf[Literal]) {
            val val0 = bounds1(0)
            if (val0.signum() > 0) {
              Array(bounds1(0).times(val0), bounds1(1).times(val0))
            } else {
              null
            }
          } else
            null
        case Token.DIV | Token.IDIV =>
          if (getRhsExpression.isInstanceOf[Literal]) {
            val val1 = bounds1(0)
            if (val1.signum() > 0) {
              try Array(bounds0(0).idiv(val1), bounds0(1).idiv(val1))
              catch {
                case _: XPathException =>
                  null
              }
            } else
              null
          } else
            null
        case _ =>
          null
      }
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val ae: ArithmeticExpression = new ArithmeticExpression(
      getLhsExpression.copy(rebindings),
      operator,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, ae)
    ae.calculator = calculator
    ae
  }

  def getItemType(): PlainType = {
    if (itemType != null) {
      itemType
    }
    if (calculator == null) {
      BuiltInAtomicType.ANY_ATOMIC
    } else {
      var t1: ItemType = getLhsExpression.getItemType
      if (!(t1.isInstanceOf[AtomicType])) {
        t1 = t1.getAtomizedItemType
      }
      var t2: ItemType = getRhsExpression.getItemType
      if (!(t2.isInstanceOf[AtomicType])) {
        t2 = t2.getAtomizedItemType
      }
      var resultType: PlainType = calculator.getResultType(
        t1.getPrimitiveItemType.asInstanceOf[AtomicType],
        t2.getPrimitiveItemType.asInstanceOf[AtomicType])
      if (resultType == BuiltInAtomicType.ANY_ATOMIC) {
        val th: TypeHierarchy = getConfiguration.getTypeHierarchy
        if ((operator == Token.PLUS || operator == Token.MINUS) &&
          (NumericType.isNumericType(t2) || NumericType.isNumericType(t1))) {
          resultType = NumericType.getInstance
        }
      }
      itemType = resultType
      itemType
    }
  }

  override def getStaticUType(contextItemType: UType): UType =
    if (getParentExpression.isInstanceOf[FilterExpression] &&
      getParentExpression.asInstanceOf[FilterExpression].getRhsExpression ==
        this) {
      UType.NUMERIC
    } else if (operator == Token.NEGATE) {
      UType.NUMERIC
    } else {
      UType.ANY_ATOMIC
    }

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    itemType = null
  }

  override def evaluateItem(context: XPathContext): AtomicValue = {
    val v0 = getLhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v0 == null) {
      return null
    }
    val v1 = getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    if (v1 == null) {
      return null
    }

    try calculator.compute(v0, v1, context)
    catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetFailingExpression(this)
        e.maybeSetContext(context)
        throw e
    }
  }

  override def tag(): String = "arith"

  override def explainExtraAttributes(out: ExpressionPresenter): Unit =
    if (calculator != null)
      out.emitAttribute("calc", calculator.code())

}
