package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.trace.ExpressionPresenter

class UniversalPattern extends Pattern {

  this.priority = -1

  def matches(item: Item, context: XPathContext): Boolean = true

  override def getUType: UType = UType.ANY

  override def getItemType: ItemType = AnyItemType

  override def getFingerprint: Int = -1

  override def toString: String = "."

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.any")
    presenter.endElement()
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[UniversalPattern]

  override def computeHashCode(): Int = 0x7aeccea8

  def copy(rebindings: RebindingMap): Pattern = this

}
