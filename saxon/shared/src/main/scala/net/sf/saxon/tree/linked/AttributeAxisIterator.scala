package net.sf.saxon.tree.linked

import net.sf.saxon.om.AttributeInfo
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import java.util.EnumSet
import java.util.function.Predicate

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


class AttributeAxisIterator(node: ElementImpl,
                            private val nodeTest: Predicate[_ >: NodeInfo])
  extends AxisIterator
    with LookaheadIterator {

  private val element: ElementImpl = node

  private var nextInfo: NodeInfo = _

  private var index: Int = 0

  private val length: Int = node.attributes().size

  advance()

  def hasNext(): Boolean = nextInfo != null

  def next(): NodeInfo =
    if (nextInfo == null) {
      null
    } else {
      val current: NodeInfo = nextInfo
      advance()
      current
    }

  private def advance(): Unit = {
    while (true) if (index >= length) {
      nextInfo = null
      return
    } else {
      val info: AttributeInfo = element.attributes().itemAt(index)
      if (info.isInstanceOf[AttributeInfo.Deleted]) {
        index += 1
      } else {
        nextInfo = new AttributeImpl(element, index)
        index += 1
        if (nodeTest.test(nextInfo)) {
          return
        }
      }
    }
  }

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)

}