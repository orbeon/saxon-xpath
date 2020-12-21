package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.om.AttributeInfo
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.iter.LookaheadIterator
import java.util.EnumSet
import java.util.function.Predicate

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


class AttributeAxisIterator(node: ElementImpl,
                            private val nodeTest: Predicate[_ >: NodeInfo])
  extends AxisIterator
    with LookaheadIterator {

  private val element: ElementImpl = node
  private var nextInfo: NodeInfo = _
  private var index: Int = 0
  private val length: Int = node.attributes.size

  advance()

  def hasNext: Boolean = nextInfo != null

  def next(): NodeInfo =
    if (nextInfo == null) {
      null
    } else {
      val current: NodeInfo = nextInfo
      advance()
      current
    }

  private def advance(): Unit =
    while (true)
      if (index >= length) {
        nextInfo = null
        return
      } else {
        val info = element.attributes.itemAt(index)
        if (info.isInstanceOf[AttributeInfo.Deleted]) {
          index += 1
        } else {
          nextInfo = new AttributeImpl(element, index)
          index += 1
          if (nodeTest.test(nextInfo))
            return
        }
      }

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)
}