package net.sf.saxon.s9api

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.streams.XdmStream

import net.sf.saxon.value.AtomicValue

import java.util.Map

import java.util.stream.Stream

object XdmItem {

   def wrapItem(item: Item): XdmItem =
    if (item == null) null else XdmValue.wrap(item).asInstanceOf[XdmItem]

   def wrapItem(item: NodeInfo): XdmNode =
    if (item == null) null else XdmValue.wrap(item).asInstanceOf[XdmNode]

   def wrapItem(item: AtomicValue): XdmAtomicValue =
    if (item == null) null
    else XdmValue.wrap(item).asInstanceOf[XdmAtomicValue]

}

abstract class XdmItem extends XdmValue {

  def this(item: Item) = {
    this()
    new XdmValue()
    this.setValue(item)
  }

  override def getUnderlyingValue: Item =
    super.getUnderlyingValue.asInstanceOf[Item]

  def getStringValue: String = getUnderlyingValue.getStringValue

  def isNode: Boolean = getUnderlyingValue.isInstanceOf[NodeInfo]

  def isAtomicValue: Boolean = getUnderlyingValue.isInstanceOf[AtomicValue]

  override def size(): Int = 1

  def asMap(): Map[XdmAtomicValue, XdmValue] = null

  override def stream(): XdmStream[_ <: XdmItem] = new XdmStream(Stream.of(this))

  def matches(`type`: ItemType): Boolean = `type`.matches(this)

}
