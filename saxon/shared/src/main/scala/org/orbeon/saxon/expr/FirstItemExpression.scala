package org.orbeon.saxon.expr

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.pattern._

import org.orbeon.saxon.trans.XPathException

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

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): Item = {
    val iter: SequenceIterator = getBaseExpression.iterate(context)
    val result: Item = iter.next()
    iter.close()
    result
  }

  override def getExpressionName: String = "first"

  override def toShortString: String =
    getBaseExpression.toShortString + "[1]"

  override def getStreamerName: String = "FirstItemExpression"

}
