package net.sf.saxon.tree.tiny

import net.sf.saxon.model.Type
import net.sf.saxon.om.AtomicSequence
import net.sf.saxon.om.AtomizedValueIterator
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import java.util.EnumSet

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property

class NamedChildIterator(private var tree: TinyTree,
                         private var node: TinyNodeImpl,
                         private var fingerprint: Int)
  extends AxisIterator
    with LookaheadIterator
    with AtomizedValueIterator {


  private var nextNodeNr: Int = node.nodeNr + 1

  private var needToAdvance: Boolean = false

  private var startNode: TinyNodeImpl = node

  if (((tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT) ||
    (tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint) {
    needToAdvance = true
  }

  def next(): NodeInfo = {
    if (needToAdvance) {
      val thisNode: Int = nextNodeNr
      do {
        nextNodeNr = tree.next(nextNodeNr)
        if (nextNodeNr < thisNode) {
          nextNodeNr = -1
          needToAdvance = false
          null
        }
      } while (((tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint) ||
        ((tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT));
    } else if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    val nextNode: TinyNodeImpl = tree.getNode(nextNodeNr)
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
          null
        }
      } while (((tree.nameCode(nextNodeNr) & 0xfffff) != fingerprint) ||
        (tree.nodeKind(nextNodeNr) & 0xf) != Type.ELEMENT);
    } else if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    tree.getTypedValueOfElement(nextNodeNr)
  }

  def hasNext(): Boolean = {
    var n: Int = nextNodeNr
    if (needToAdvance) {
      val thisNode: Int = n
      do {
        n = tree.next(n)
        if (n < thisNode) {
          false
        }
      } while ((tree.nodeKind(n) & 0xf) != Type.ELEMENT || (tree.nameCode(n) & 0xfffff) != fingerprint);
      true
    } else {
      n != -1
    }
  }

  override def getProperties: Set[Property] =
    Set(Property.LOOKAHEAD, Property.ATOMIZING)

}
