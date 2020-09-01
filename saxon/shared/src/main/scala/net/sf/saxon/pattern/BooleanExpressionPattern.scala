package net.sf.saxon.pattern

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ManualIterator

class BooleanExpressionPattern(expression: Expression)
  extends Pattern
    with PatternWithPredicate {

  private var expressionOp: Operand =
    new Operand(this, expression, OperandRole.SINGLE_ATOMIC)

  this.priority = 1

  override def getPredicate(): Expression = expressionOp.getChildExpression

  override def operands: java.lang.Iterable[Operand] = expressionOp

  override def getUType(): UType =
    if (getPredicate.isInstanceOf[InstanceOfExpression]) {
      getPredicate
        .asInstanceOf[InstanceOfExpression]
        .getRequiredItemType
        .getUType
    } else {
      UType.ANY
    }

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int =
    ExpressionTool.allocateSlots(getPredicate, nextFree, slotManager)

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    val cit: ContextItemStaticInfo =
      visitor.getConfiguration.getDefaultContextItemStaticInfo
    expressionOp.setChildExpression(getPredicate.typeCheck(visitor, cit))
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Pattern = {
    val cit: ContextItemStaticInfo =
      visitor.getConfiguration.getDefaultContextItemStaticInfo
    expressionOp.setChildExpression(getPredicate.optimize(visitor, cit))
    this
  }

  def matches(item: Item, context: XPathContext): Boolean = {
    val c2: XPathContext = context.newMinorContext()
    val iter: ManualIterator = new ManualIterator(item)
    c2.setCurrentIterator(iter)
    c2.setCurrentOutputUri(null)
    try getPredicate.effectiveBooleanValue(c2)
    catch {
      case e: XPathException => false

    }
  }

  override def getItemType: ItemType = {
    if (getPredicate.isInstanceOf[InstanceOfExpression]) {
      val ioe: InstanceOfExpression =
        getPredicate.asInstanceOf[InstanceOfExpression]
      if (ioe.getBaseExpression.isInstanceOf[ContextItemExpression]) {
        ioe.getRequiredItemType
      }
    }
    AnyItemType
  }

  override def getFingerprint(): Int = -1

  override def toString: String = ".[" + getPredicate + "]"

  override def equals(other: Any): Boolean =
    (other.isInstanceOf[BooleanExpressionPattern]) &&
      other
        .asInstanceOf[BooleanExpressionPattern]
        .getPredicate
        .isEqual(getPredicate)

  override def computeHashCode(): Int = 0x7aeffea9 ^ getPredicate.hashCode

  def copy(rebindings: RebindingMap): Pattern = {
    val n: BooleanExpressionPattern = new BooleanExpressionPattern(
      getPredicate.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.booleanExp")
    getPredicate.export(presenter)
    presenter.endElement()
  }

}
