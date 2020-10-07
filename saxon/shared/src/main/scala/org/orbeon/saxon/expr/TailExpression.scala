package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

class TailExpression(base: Expression, var start: Int)
  extends UnaryExpression(base) {

  override def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    if (getBaseExpression.isInstanceOf[Literal]) {
      val value: GroundedValue = iterate(
        visitor.getStaticContext.makeEarlyEvaluationContext()).materialize()
      Literal.makeLiteral(value, this)
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: TailExpression =
      new TailExpression(getBaseExpression.copy(rebindings), start)
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def getItemType: ItemType = getBaseExpression.getItemType

  override def computeCardinality(): Int =
    getBaseExpression.getCardinality | StaticProperty.ALLOWS_ZERO

   override def getOperandRole(): OperandRole =
    OperandRole.SAME_FOCUS_ACTION

  def getStart: Int = start

  override def equals(other: Any): Boolean =
    other.isInstanceOf[TailExpression] &&
      getBaseExpression.isEqual(
        other.asInstanceOf[TailExpression].getBaseExpression) &&
      start == other.asInstanceOf[TailExpression].start

  override def computeHashCode(): Int = super.computeHashCode() ^ start

  override def getStreamerName: String = "TailExpression"

  override def iterate(context: XPathContext): SequenceIterator = {
    val baseIter: SequenceIterator = getBaseExpression.iterate(context)
    TailIterator.make(baseIter, start)
  }

  override def getExpressionName: String = "tail"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("tail", this)
    destination.emitAttribute("start", start.toString)
    getBaseExpression.export(destination)
    destination.endElement()
  }

  override def toString: String =
    if (start == 2) {
      "tail(" + getBaseExpression + ")"
    } else {
      ExpressionTool.parenthesize(getBaseExpression) + "[position ge " +
        start +
        "]"
    }

  override def toShortString: String =
    if (start == 2) {
      "tail(" + getBaseExpression.toShortString + ")"
    } else {
      getBaseExpression.toShortString + "[position ge " +
        start +
        "]"
    }

}
