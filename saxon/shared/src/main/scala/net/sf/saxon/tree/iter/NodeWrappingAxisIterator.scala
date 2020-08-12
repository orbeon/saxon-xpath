package net.sf.saxon.tree.iter

import net.sf.saxon.om.NodeInfo
import java.util.EnumSet
import java.util.Iterator

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


class NodeWrappingAxisIterator[B](var base: Iterator[_ <: B],
                                   private val wrappingFunction: NodeWrappingFunction[_ >: B, NodeInfo])
  extends AxisIterator
    with LookaheadIterator {

  def getBaseIterator(): Iterator[_ <: B] = base

  def getNodeWrappingFunction(): NodeWrappingFunction[_ >: B, NodeInfo] =
    wrappingFunction

  def hasNext(): Boolean = base.hasNext

  def next(): NodeInfo = {
    while (base.hasNext) {
      val next: B = base.next()
      if (!isIgnorable(next)) {
        wrappingFunction.wrap(next)
      }
    }
    null
  }

  def isIgnorable(node: B): Boolean = false

  override def getProperties(): Set[Property] = Set(Property.LOOKAHEAD)

}