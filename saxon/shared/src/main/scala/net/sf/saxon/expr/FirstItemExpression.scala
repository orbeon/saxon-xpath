package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern._

import net.sf.saxon.trans.XPathException

import FirstItemExpression._


object FirstItemExpression {

  def makeFirstItemExpression(base: Expression): Expression =
    if (base.isInstanceOf[FirstItemExpression]) {
      base
    } else {
      new FirstItemExpression(base)
    }

}

class FirstItemExpression(base: Expression) extends SingleItemFilter(base) {

  def copy(rebindings: RebindingMap): Expression = {
    val e2: Expression = new FirstItemExpression(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, e2)
    e2
  }

  override def toPattern(config: Configuration): Pattern = {
    val basePattern: Pattern = getBaseExpression.toPattern(config)
    val `type`: ItemType = basePattern.getItemType
    if (`type`.isInstanceOf[NodeTest]) {
      val baseExpr: Expression = getBaseExpression
      if (baseExpr.isInstanceOf[AxisExpression] &&
        baseExpr.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD &&
        basePattern.isInstanceOf[NodeTestPattern]) {
        new SimplePositionalPattern(`type`.asInstanceOf[NodeTest], 1)
      } else {
        new GeneralNodePattern(this, `type`.asInstanceOf[NodeTest])
      }
    } else {
      // For a non-node pattern, the predicate [1] is always true
      basePattern
    }
  }

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): Item = {
    val iter: SequenceIterator = getBaseExpression.iterate(context)
    val result: Item = iter.next()
    iter.close()
    result
  }

  override def getExpressionName(): String = "first"

  override def toShortString: String =
    getBaseExpression.toShortString + "[1]"

  override def getStreamerName(): String = "FirstItemExpression"

}
