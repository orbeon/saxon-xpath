package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.instruct.ForEachGroup

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.expr.sort.GroupIterator

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.om._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import CurrentGroupCall._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object CurrentGroupCall {

  def findControllingInstruction(exp: Expression): ForEachGroup = {
    var child: Expression = exp
    var parent: Expression = exp.getParentExpression
    while (parent != null) {
      if (parent.isInstanceOf[ForEachGroup] &&
        (child ==
          parent.asInstanceOf[ForEachGroup].getActionExpression ||
          child ==
            parent.asInstanceOf[ForEachGroup].getSortKeyDefinitionList)) {
        parent.asInstanceOf[ForEachGroup]
      }
      child = parent
      parent = parent.getParentExpression
    }
    null
  }

}

class CurrentGroupCall extends Expression with Callable {

  var isInHigherOrderOperand: Boolean = false

  private var itemType: ItemType = AnyItemType

  @BeanProperty
  var controllingInstruction: ForEachGroup = findControllingInstruction(this)

  override def getScopingExpression(): Expression = getControllingInstruction

  def setControllingInstruction(instruction: ForEachGroup,
                                itemType: ItemType,
                                isHigherOrder: Boolean): Unit = {
    resetLocalStaticProperties()
    this.controllingInstruction = instruction
    this.isInHigherOrderOperand = isHigherOrder
    this.itemType = itemType
  }

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    this.controllingInstruction = null
    this.itemType = AnyItemType
  }

  override def getItemType(): ItemType = {
    if (itemType == AnyItemType && controllingInstruction != null) {
      itemType = controllingInstruction.getSelectExpression.getItemType
    }
    itemType
  }

  override def getIntrinsicDependencies(): Int = StaticProperty.DEPENDS_ON_CURRENT_GROUP

   override def computeCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_MORE

  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("currentGroup")
    out.endElement()
  }

  override def computeSpecialProperties(): Int =
    if (getControllingInstruction == null) {
      0
    } else {
      controllingInstruction.getSelectExpression.getSpecialProperties
    }

  override def copy(rebindings: RebindingMap): Expression = {
    val cg: CurrentGroupCall = new CurrentGroupCall()
    cg.isInHigherOrderOperand = isInHigherOrderOperand
    cg.itemType = itemType
    cg.controllingInstruction = controllingInstruction
    cg
  }

 override def iterate(c: XPathContext): SequenceIterator = {
    val gi: GroupIterator = c.getCurrentGroupIterator
    if (gi == null) {
      val err =
        new XPathException("There is no current group", "XTDE1061")
      err.setLocation(getLocation)
      throw err
    }
    gi.iterateCurrentGroup()
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(iterate(context))

  override def toString: String = "current-group()"

  override def toShortString(): String = toString

  override def getStreamerName(): String = "CurrentGroup"

}
