package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.MemoSequence
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.value.NumericValue

class SubscriptExpression(base: Expression, subscript: Expression) extends SingleItemFilter(base) {

  private var subscriptOp: Operand =
    new Operand(this, subscript, OperandRole.SINGLE_ATOMIC)

  def getSubscript: Expression = subscriptOp.getChildExpression

  def setSubscript(subscript: Expression): Unit = {
    subscriptOp.setChildExpression(subscript)
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    if (Literal.isConstantOne(getSubscript)) {
      FirstItemExpression.makeFirstItemExpression(getBaseExpression)
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: SubscriptExpression = new SubscriptExpression(
      getBaseExpression.copy(rebindings),
      getSubscript.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def operands: java.lang.Iterable[Operand] = operandList(getOperand, subscriptOp)

  def getSubscriptExpression: Expression = getSubscript

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def equals(other: Any): Boolean =
    other.isInstanceOf[SubscriptExpression] &&
      getBaseExpression.isEqual(
        other.asInstanceOf[SubscriptExpression].getBaseExpression) &&
      getSubscript.isEqual(
        other.asInstanceOf[SubscriptExpression].getSubscript)

  override def computeHashCode(): Int =
    getBaseExpression.hashCode ^ getSubscript.hashCode

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_ONE

  override def getStreamerName: String = "SubscriptExpression"

  override def evaluateItem(context: XPathContext): Item = {
    val memoSequence = new MemoSequence()
    val index: NumericValue =
      getSubscript.evaluateItem(context).asInstanceOf[NumericValue]
    if (index == null) {
      return null
    }
    val intindex: Int = index.asSubscript()
    if (intindex != -1) {
      var item: Item = null
      val iter: SequenceIterator = getBaseExpression.iterate(context)
      if (intindex == 1) {
        item = iter.next()
      } else if (iter.isInstanceOf[memoSequence.ProgressiveIterator]) {
        val mem: MemoSequence =
          iter.asInstanceOf[memoSequence.ProgressiveIterator].getMemoSequence
        item = mem.itemAt(intindex - 1)
      } else if (iter.getProperties.contains(
        SequenceIterator.Property.GROUNDED)) {
        val value: GroundedValue = iter.materialize
        item = value.itemAt(intindex - 1)
      } else {
        val tail: SequenceIterator = TailIterator.make(iter, intindex)
        item = tail.next()
        tail.close()
      }
      item
    } else {
      null
    }
  }

  override def getExpressionName: String = "subscript"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("subscript", this)
    getBaseExpression.export(destination)
    getSubscript.export(destination)
    destination.endElement()
  }

  override def toString: String =
    ExpressionTool.parenthesize(getBaseExpression) + "[" +
      getSubscript +
      "]"

  override def toShortString: String =
    ExpressionTool.parenthesize(getBaseExpression) + "[" +
      getSubscript.toShortString +
      "]"

}
