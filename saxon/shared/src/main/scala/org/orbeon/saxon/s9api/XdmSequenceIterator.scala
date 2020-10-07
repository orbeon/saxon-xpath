package org.orbeon.saxon.s9api

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.s9api.streams.XdmStream

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AxisIterator

import org.orbeon.saxon.tree.iter.SingletonIterator

import org.orbeon.saxon.tree.iter.UnfailingIterator

import java.util.Iterator

import java.util.Spliterator

import java.util.Spliterators

import java.util.stream.Stream

import java.util.stream.StreamSupport

import XdmSequenceIterator._

object XdmSequenceIterator {

  private val BEFORE_ITEM: Int = 0

  private val ON_ITEM: Int = 1

  private val FINISHED: Int = 2

  def ofNodes(base: AxisIterator): XdmSequenceIterator[XdmNode] =
    new XdmSequenceIterator(base)

  def ofAtomicValues(
                      base: UnfailingIterator): XdmSequenceIterator[XdmAtomicValue] =
    new XdmSequenceIterator(base)

  def ofNode(node: XdmNode): XdmSequenceIterator[XdmNode] =
    new XdmSequenceIterator(
      SingletonIterator.makeIterator(node.getUnderlyingNode))

}

class XdmSequenceIterator[T <: XdmItem] extends Iterator[T] {

  private var nextIt: T = _

  private var state: Int = _

  var base: SequenceIterator = _

  def this(base: SequenceIterator) {
    this()
    this.base = base
    this.state = BEFORE_ITEM
  }

  def this(base: UnfailingIterator) = {
    this()
    this.base = base
    this.state = BEFORE_ITEM
  }

  def hasNext: Boolean = state match {
    case ON_ITEM => true
    case FINISHED => false
    case BEFORE_ITEM => {
      nextIt = XdmItem.wrapItem(base.next()).asInstanceOf[T]
      if (nextIt == null) {
        state = FINISHED
        false
      } else {
        state = ON_ITEM
        true
      }
    }
    case _ => throw new IllegalStateException()

  }

  def next(): T = state match {
    case ON_ITEM =>
      state = BEFORE_ITEM
      nextIt
    case FINISHED => throw new java.util.NoSuchElementException()
    case BEFORE_ITEM =>
      if (hasNext) {
        nextIt
      } else {
        throw new java.util.NoSuchElementException()
      }
    case _ => throw new IllegalStateException()

  }

  override def remove(): Unit = {
    throw new UnsupportedOperationException()
  }

  def close(): Unit = {
    base.close()
    state = FINISHED
  }

  /*def stream(): XdmStream[T] = {
    var base: Stream[T] = StreamSupport.stream(
      Spliterators.spliteratorUnknownSize(this, Spliterator.ORDERED),
      false)
    base = base.onClose((res : XdmSequenceIterator.this.res.close))
    new XdmStream(base)
  }*/

}
