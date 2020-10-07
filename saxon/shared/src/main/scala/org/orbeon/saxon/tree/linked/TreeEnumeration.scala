package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.iter.LookaheadIterator
import java.util.EnumSet
import java.util.function.Predicate

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


abstract class TreeEnumeration(
                                var nextImpl: NodeImpl,
                                var nodeTest: Predicate[_ >: NodeInfo])
  extends AxisIterator
    with LookaheadIterator {

   var start: NodeImpl = _

   var current: NodeImpl = null

   var position: Int = 0

   def conforms(node: NodeImpl): Boolean =
    node == null || nodeTest == null || nodeTest.test(node)

   def advance(): Unit = {
    do step() while (!conforms(nextImpl));
  }

   def step(): Unit

  def hasNext: Boolean = nextImpl != null

  def next(): NodeInfo =
    if (nextImpl == null) {
      current = null
      position = -1
      null
    } else {
      current = nextImpl
      position = position + 1
      advance()
      current
    }

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)

}
