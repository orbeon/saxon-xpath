package net.sf.saxon.tree.tiny

import net.sf.saxon.model.Type
import net.sf.saxon.om.AtomicSequence
import net.sf.saxon.om.AtomizedValueIterator
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.value.UntypedAtomicValue
import net.sf.saxon.z.IntSetPredicate
import java.util.EnumSet
import java.util.function.IntPredicate
import java.util.function.Predicate

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


class SiblingIterator(private var tree: TinyTree,
                      private var node: TinyNodeImpl,
                      private var nodeTest: Predicate[_ >: NodeInfo],
                      private var getChildren: Boolean)
  extends AxisIterator
    with LookaheadIterator
    with AtomizedValueIterator {
  private var test = nodeTest

  private var nextNodeNr: Int = _

  private var parentNode: TinyNodeImpl = _

  private var needToAdvance: Boolean = false

  private val matcher: IntPredicate =
    if (nodeTest.isInstanceOf[NodeTest])
      nodeTest.asInstanceOf[NodeTest].getMatcher(tree)
    else IntSetPredicate.ALWAYS_TRUE

  if (getChildren) {
    parentNode = node
    nextNodeNr = node.nodeNr + 1
  } else {
    parentNode = node.getParent
    if (parentNode == null) {
      nextNodeNr = -1
    } else {
      nextNodeNr = tree.next(node.nodeNr)
      while (tree.nodeKind(nextNodeNr) == Type.PARENT_POINTER) nextNodeNr =
        tree.next(nextNodeNr)
      if (nextNodeNr < node.nodeNr) {
        nextNodeNr = -1
      }
    }
  }

  if (nextNodeNr >= 0 && nodeTest != null) {
    if (!matcher.test(nextNodeNr)) {
      needToAdvance = true
    }
  }

  def next(): NodeInfo = {
    if (needToAdvance) {
      val thisNode: Int = nextNodeNr
      val tNext: Array[Int] = tree.next
      val nTest: Predicate[_ >: NodeInfo] = test
      if (nTest == null) {
        do nextNodeNr = tNext(nextNodeNr) while (tree.nodeKind(nextNodeNr) == Type.PARENT_POINTER);
      } else {
        do nextNodeNr = tNext(nextNodeNr) while (nextNodeNr >= thisNode && !matcher
          .test(nextNodeNr));
      }
      if (nextNodeNr < thisNode) {
        nextNodeNr = -1
        needToAdvance = false
        return null
      }
    }
    if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    val nextNode: TinyNodeImpl = tree.getNode(nextNodeNr)
    nextNode.setParentNode(parentNode)
    nextNode
  }

  def nextAtomizedValue(): AtomicSequence = {
    if (needToAdvance) {
      val thisNode: Int = nextNodeNr
      val nTest: Predicate[_ >: NodeInfo] = test
      val tNext: Array[Int] = tree.next
      if (nTest == null) {
        do nextNodeNr = tNext(nextNodeNr) while (tree.nodeKind(nextNodeNr) == Type.PARENT_POINTER);
      } else {
        do nextNodeNr = tNext(nextNodeNr) while (nextNodeNr >= thisNode && !matcher
          .test(nextNodeNr));
      }
      if (nextNodeNr < thisNode) {
        nextNodeNr = -1
        needToAdvance = false
        return null
      }
    }
    if (nextNodeNr == -1) {
      return null
    }
    needToAdvance = true
    val kind: Int = tree.nodeKind(nextNodeNr)
    kind match {
      case Type.TEXT =>
        new UntypedAtomicValue(TinyTextImpl.getStringValue(tree, nextNodeNr))
      case Type.WHITESPACE_TEXT =>
        new UntypedAtomicValue(
          WhitespaceTextImpl.getStringValueCS(tree, nextNodeNr))
      case Type.ELEMENT | Type.TEXTUAL_ELEMENT =>
        tree.getTypedValueOfElement(nextNodeNr)
      case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
        tree.getAtomizedValueOfUntypedNode(nextNodeNr)
      case _ => throw new AssertionError("Unknown node kind on child axis")

    }
  }

  def hasNext(): Boolean = {
    var n: Int = nextNodeNr
    if (needToAdvance) {
      val nTest: Predicate[_ >: NodeInfo] = test
      val tNext: Array[Int] = tree.next
      if (nTest == null) {
        do n = tNext(n) while (tree.nodeKind(n) == Type.PARENT_POINTER);
      } else {
        do n = tNext(n) while (n >= nextNodeNr && !matcher.test(n));
      }
      if (n < nextNodeNr) {
        return false
      }
    }
    n != -1
  }

  override def getProperties(): Set[Property] =
    Set(Property.LOOKAHEAD, Property.ATOMIZING)

}
