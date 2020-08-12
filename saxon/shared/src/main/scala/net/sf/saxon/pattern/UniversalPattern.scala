package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.trace.ExpressionPresenter

class UniversalPattern extends Pattern {

  this.priority = -1

  def matches(item: Item, context: XPathContext): Boolean = true

  override def getUType(): UType = UType.ANY

  override def getItemType(): ItemType = AnyItemType.getInstance

  override def getFingerprint(): Int = -1

  override def toString(): String = "."

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.any")
    presenter.endElement()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[UniversalPattern]

  override def computeHashCode(): Int = 0x7aeccea8

  def copy(rebindings: RebindingMap): Pattern = this

}
