package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model._

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._


class IntegerRangeTest(value: Expression, min: Expression, max: Expression)
  extends Expression {

  private var valueOp: Operand =
    new Operand(this, value, OperandRole.ATOMIC_SEQUENCE)

  private var minOp: Operand =
    new Operand(this, min, OperandRole.SINGLE_ATOMIC)

  private var maxOp: Operand =
    new Operand(this, max, OperandRole.SINGLE_ATOMIC)

  override def operands(): java.lang.Iterable[Operand] =
    operandList(valueOp, minOp, maxOp)

  def getValue(): Expression = valueOp.getChildExpression

  def setValue(value: Expression): Unit = {
    valueOp.setChildExpression(value)
  }

  def getMin(): Expression = minOp.getChildExpression

  def setMin(min: Expression): Unit = {
    minOp.setChildExpression(min)
  }

  def getMax(): Expression = maxOp.getChildExpression

  def setMax(max: Expression): Unit = {
    maxOp.setChildExpression(max)
  }

override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = this

  override def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Expression = {
    if (Literal.isEmptySequence(getMin) || Literal.isEmptySequence(getMax) ||
      Literal.isEmptySequence(getValue)) {
      new Literal(BooleanValue.FALSE)
    }
    if (getMin.isInstanceOf[Literal] && getMax.isInstanceOf[Literal] &&
      getValue.isInstanceOf[Literal]) {
      val result: BooleanValue = evaluateItem(visitor.makeDynamicContext())
      new Literal(result)
    }
    this
  }

  def getItemType(): ItemType = BuiltInAtomicType.BOOLEAN

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  def copy(rebindings: RebindingMap): Expression = {
    val exp: IntegerRangeTest = new IntegerRangeTest(getValue.copy(rebindings),
      getMin.copy(rebindings),
      getMax.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[IntegerRangeTest] &&
      other.asInstanceOf[IntegerRangeTest].getValue.isEqual(getValue) &&
      other.asInstanceOf[IntegerRangeTest].getMin.isEqual(getMin) &&
      other.asInstanceOf[IntegerRangeTest].getMax.isEqual(getMax)

  override def computeHashCode(): Int = {
    var h: Int = getValue.hashCode + 77
    h ^= getMin.hashCode ^ getMax.hashCode
    h
  }

  override def evaluateItem(c: XPathContext): BooleanValue = {
    var minVal: IntegerValue = null
    var maxVal: IntegerValue = null
    var toDouble: StringConverter = null
    val iter: SequenceIterator = getValue.iterate(c)
    var atom: AtomicValue = null
    while (({
      atom = iter.next().asInstanceOf[AtomicValue]
      atom
    }) != null) {
      if (minVal == null) {
        minVal = getMin.evaluateItem(c).asInstanceOf[IntegerValue]
        if (minVal == null) {
          BooleanValue.FALSE
        }
        maxVal = getMax.evaluateItem(c).asInstanceOf[IntegerValue]
        if (maxVal == null || maxVal.compareTo(minVal) < 0) {
          BooleanValue.FALSE
        }
      }
      var v: NumericValue = null
      if (atom.isInstanceOf[UntypedAtomicValue]) {
        if (toDouble == null) {
          toDouble = BuiltInAtomicType.DOUBLE.getStringConverter(
            c.getConfiguration.getConversionRules)
        }
        val result: ConversionResult =
          toDouble.convertString(atom.getStringValueCS)
        if (result.isInstanceOf[ValidationFailure]) {
          val e: XPathException = new XPathException(
            "Failed to convert untypedAtomic value {" + atom.getStringValueCS +
              "}  to xs:integer",
            "FORG0001")
          e.setLocation(getLocation)
          throw e
        } else {
          v = result.asAtomic().asInstanceOf[DoubleValue]
        }
      } else if (atom.isInstanceOf[NumericValue]) {
        v = atom.asInstanceOf[NumericValue]
      } else {
        val e: XPathException = new XPathException(
          "Cannot compare value of type " + atom.getUType + " to xs:integer",
          "XPTY0004")
        e.setIsTypeError(true)
        e.setLocation(getLocation)
        throw e
      }
      if (v.isWholeNumber && v.compareTo(minVal) >= 0 && v.compareTo(maxVal) <= 0) {
        BooleanValue.TRUE
      }
    }
    BooleanValue.FALSE
  }

  override def getExpressionName(): String = "intRangeTest"

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("intRangeTest", this)
    getValue.export(destination)
    getMin.export(destination)
    getMax.export(destination)
    destination.endElement()
  }

  override def toString(): String =
    ExpressionTool.parenthesize(getValue) + " = (" + ExpressionTool
      .parenthesize(getMin) +
      " to " +
      ExpressionTool.parenthesize(getMax) +
      ")"

}
