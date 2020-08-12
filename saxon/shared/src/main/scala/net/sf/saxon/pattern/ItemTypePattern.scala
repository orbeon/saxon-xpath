package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.AlphaCode

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.trace.ExpressionPresenter

import scala.beans.{BeanProperty}

class ItemTypePattern(@BeanProperty var itemTyp: ItemType) extends Pattern {


  this.priority = itemTyp.getDefaultPriority

  def matches(item: Item, context: XPathContext): Boolean =
    itemTyp.matches(item, context.getConfiguration.getTypeHierarchy)

  override def getUType(): UType = itemTyp.getUType

  override def toString(): String = itemTyp.toString

  override def getItemType(): ItemType = itemTyp

  override def equals(other: Any): Boolean =
    (other.isInstanceOf[ItemTypePattern]) &&
      other.asInstanceOf[ItemTypePattern].itemTyp == itemTyp

  override def computeHashCode(): Int = 0x7a83d1a8 ^ itemTyp.hashCode

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.nodeTest")
    presenter.emitAttribute("test", AlphaCode.fromItemType(itemTyp))
    presenter.endElement()
  }

  def copy(rebindings: RebindingMap): Pattern = new ItemTypePattern(itemTyp)

}
