package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{AtomicSequence, AtomizedValueIterator, NodeInfo}
import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property
import org.orbeon.saxon.tree.iter.{AxisIterator, LookaheadIterator}


class NamedChildIterator(private var tree: TinyTree,
                         private var node: TinyNodeImpl,
                         private var fingerprint: Int)
  extends AxisIterator
    with LookaheadIterator
    with AtomizedValueIterator {


  private var nextNodeNr: Int = node.nodeNr + 1

  private var needToAdvance: Boolean = false

  private val startNode: TinyNodeImpl = node

  if (((tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT) || (tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint)
    needToAdvance = true

  def next(): NodeInfo = {
    if (needToAdvance) {
      val thisNode = nextNodeNr
      do {
        nextNodeNr = tree.next(nextNodeNr)
        if (nextNodeNr < thisNode) {
          nextNodeNr = -1
          needToAdvance = false
          return null
        }
      } while (((tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint) || ((tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT))
    } else if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    val nextNode = tree.getNode(nextNodeNr)
    nextNode.setParentNode(startNode)
    nextNode
  }

  def nextAtomizedValue(): AtomicSequence = {
    if (needToAdvance) {
      val thisNode: Int = nextNodeNr
      do {
        nextNodeNr = tree.next(nextNodeNr)
        if (nextNodeNr < thisNode) {
          nextNodeNr = -1
          needToAdvance = false
          return null
        }
      } while (((tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint) || (tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT)
    } else if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    tree.getTypedValueOfElement(nextNodeNr)
  }

  def hasNext: Boolean = {
    var n = nextNodeNr
    if (needToAdvance) {
      val thisNode = n
      do {
        n = tree.next(n)
        if (n < thisNode)
          return false
      } while ((tree.nodeKind(n) & 0xf) != Type.ELEMENT || (tree.nameCode(n) & 0xfffff) != fingerprint)
      true
    } else {
      n != -1
    }
  }

  override def getProperties: Set[Property] =
    Set(Property.LOOKAHEAD, Property.ATOMIZING)
}
