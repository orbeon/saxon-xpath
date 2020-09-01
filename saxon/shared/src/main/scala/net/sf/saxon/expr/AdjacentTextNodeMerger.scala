package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.Block

import net.sf.saxon.expr.instruct.Choose

import net.sf.saxon.expr.instruct.ValueOf

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model._

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AdjacentTextNodeMergingIterator

import scala.util.control.Breaks._

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Cardinality

object AdjacentTextNodeMerger {

  def makeAdjacentTextNodeMerger(base: Expression): Expression =
    if (base.isInstanceOf[Literal] &&
      base.asInstanceOf[Literal].getValue.isInstanceOf[AtomicSequence]) {
      base
    } else {
      new AdjacentTextNodeMerger(base)
    }

  def isTextNode(item: Item): Boolean =
    item.isInstanceOf[NodeInfo] &&
      item.asInstanceOf[NodeInfo].getNodeKind == Type.TEXT

}

class AdjacentTextNodeMerger(p0: Expression) extends UnaryExpression(p0) {

  def getOperandRole(): OperandRole = OperandRole.SAME_FOCUS_ACTION

  override def simplify(): Expression = {
    val operand: Expression = getBaseExpression
    if (operand.isInstanceOf[Literal] &&
      operand.asInstanceOf[Literal].getValue.isInstanceOf[AtomicValue]) {
      operand
    } else {
      super.simplify()
    }
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    if (th.relationship(getBaseExpression.getItemType, NodeKindTest.TEXT) ==
      Affinity.DISJOINT) {
      val base: Expression = getBaseExpression
      base.setParentExpression(getParentExpression)
      return base
    }
    if (!Cardinality.allowsMany(getBaseExpression.getCardinality)) {
      val base: Expression = getBaseExpression
      base.setParentExpression(getParentExpression)
      return base
    }
    if (getBaseExpression.isInstanceOf[Choose]) {
      val choose: Choose = getBaseExpression.asInstanceOf[Choose]
      for (i <- 0 until choose.size) {
        val atm2: AdjacentTextNodeMerger = new AdjacentTextNodeMerger(
          choose.getAction(i))
        choose.setAction(i, atm2.typeCheck(visitor, contextInfo))
      }
      return choose
    }
    if (getBaseExpression.isInstanceOf[Block]) {
      val block: Block = getBaseExpression.asInstanceOf[Block]
      val actions: Array[Operand] = block.getOperanda
      var prevtext: Boolean = false
      var needed: Boolean = false
      var maybeEmpty: Boolean = false
      breakable {
        for (o <- actions) {
          val action: Expression = o.getChildExpression
          var maybetext: Boolean = false
          if (action.isInstanceOf[ValueOf]) {
            maybetext = true
            val content: Expression = action.asInstanceOf[ValueOf].getSelect
            if (content.isInstanceOf[StringLiteral]) {
              maybeEmpty |= content
                .asInstanceOf[StringLiteral]
                .getStringValue
                .isEmpty
            } else {
              maybeEmpty = true
            }
          } else {
            maybetext = th.relationship(action.getItemType, NodeKindTest.TEXT) !=
              Affinity.DISJOINT
            maybeEmpty |= maybetext
          }
          if (prevtext && maybetext) {
            needed = true
            break()
          }
          if (maybetext && Cardinality.allowsMany(action.getCardinality)) {
            needed = true
            break()
          }
          prevtext = maybetext
        }
      }
      if (!needed) {
        if (maybeEmpty) {
          new EmptyTextNodeRemover(block)
        } else {
          return block
        }
      }
    }
    this
  }

  override def getItemType: ItemType = getBaseExpression.getItemType

  override def getStaticUType(contextItemType: UType): UType =
    getBaseExpression.getStaticUType(contextItemType)

  override def computeCardinality(): Int =
    getBaseExpression.getCardinality | StaticProperty.ALLOWS_ZERO

  def copy(rebindings: RebindingMap): Expression = {
    val a2: AdjacentTextNodeMerger = new AdjacentTextNodeMerger(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, a2)
    a2
  }

  import Expression._

  def getImplementationMethod: Int = PROCESS_METHOD | ITERATE_METHOD | ITEM_FEED_METHOD | WATCH_METHOD

  override def getStreamerName: String = "AdjacentTextNodeMerger"

  override def iterate(context: XPathContext): SequenceIterator =
    new AdjacentTextNodeMergingIterator(getBaseExpression.iterate(context))

  override def getExpressionName: String = "mergeAdj"

}
