package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ManualIterator

class BooleanExpressionPattern(expression: Expression)
  extends Pattern
    with PatternWithPredicate {

  private var expressionOp: Operand =
    new Operand(this, expression, OperandRole.SINGLE_ATOMIC)

  this.priority = 1

  override def getPredicate(): Expression = expressionOp.getChildExpression

  override def operands: java.lang.Iterable[Operand] = expressionOp

  override def getUType: UType =
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
       return ioe.getRequiredItemType
      }
    }
    AnyItemType
  }

  override def getFingerprint: Int = -1

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
