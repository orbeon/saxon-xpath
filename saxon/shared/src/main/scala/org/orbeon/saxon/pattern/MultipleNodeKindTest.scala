package org.orbeon.saxon.pattern

import java.util.LinkedList
import java.util.function.IntPredicate

import org.orbeon.saxon.model.PrimitiveUType._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{NodeInfo, NodeName}
import org.orbeon.saxon.tree.tiny.NodeVectorTree
import org.orbeon.saxon.tree.util.FastStringBuffer

object MultipleNodeKindTest {

  val PARENT_NODE: MultipleNodeKindTest = new MultipleNodeKindTest(
    UType.DOCUMENT.union(UType.ELEMENT))

  val DOC_ELEM_ATTR: MultipleNodeKindTest = new MultipleNodeKindTest(
    UType.DOCUMENT.union(UType.ELEMENT).union(UType.ATTRIBUTE))

  val LEAF: MultipleNodeKindTest = new MultipleNodeKindTest(
    UType.TEXT
      .union(UType.COMMENT)
      .union(UType.PI)
      .union(UType.NAMESPACE)
      .union(UType.ATTRIBUTE))

  val CHILD_NODE: MultipleNodeKindTest = new MultipleNodeKindTest(
    UType.ELEMENT.union(UType.TEXT).union(UType.COMMENT).union(UType.PI))

}

class MultipleNodeKindTest(u: UType) extends NodeTest {

  var uType: UType = u

  var nodeKindMask: Int = _

  if (UType.DOCUMENT.overlaps(u)) {
    nodeKindMask |= 1 << Type.DOCUMENT
  }

  if (UType.ELEMENT.overlaps(u)) {
    nodeKindMask |= 1 << Type.ELEMENT
  }

  if (UType.ATTRIBUTE.overlaps(u)) {
    nodeKindMask |= 1 << Type.ATTRIBUTE
  }

  if (UType.TEXT.overlaps(u)) {
    nodeKindMask |= 1 << Type.TEXT
  }

  if (UType.COMMENT.overlaps(u)) {
    nodeKindMask |= 1 << Type.COMMENT
  }

  if (UType.PI.overlaps(u)) {
    nodeKindMask |= 1 << Type.PROCESSING_INSTRUCTION
  }

  if (UType.NAMESPACE.overlaps(u)) {
    nodeKindMask |= 1 << Type.NAMESPACE
  }

  def getUType: UType = uType

  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean =
    (nodeKindMask & (1 << nodeKind)) != 0

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    (nodeNr) =>
    {
      var nodeKind: Int = nodeKindArray(nodeNr) & 0x0f
      if (nodeKind == Type.WHITESPACE_TEXT) {
        nodeKind = Type.TEXT
      }
      (nodeKindMask & (1 << nodeKind)) != 0
    }
  }

  override def test(node: NodeInfo): Boolean = {
    val nodeKind: Int = node.getNodeKind
    (nodeKindMask & (1 << nodeKind)) != 0
  }

  def getDefaultPriority: Double = -0.5

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    val types: LinkedList[PrimitiveUType] =
      new LinkedList[PrimitiveUType](uType.decompose())
    format(types, fsb, (res :ItemType) => res.toString)
    fsb.toString
  }

  override def toExportString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    val types: LinkedList[PrimitiveUType] =
      new LinkedList[PrimitiveUType](uType.decompose())
    format(types, fsb,  (res :ItemType) => res.toExportString)
    fsb.toString
  }

  private def format(list: LinkedList[PrimitiveUType],
                     fsb: FastStringBuffer,
                     show: ItemType => String): Unit = {
    if (list.size == 1) {
      fsb.append(list.get(0).toItemType.toString)
    } else {
      fsb.cat('(')
      fsb.append(list.removeFirst().toItemType.toString)
      fsb.cat('|')
      format(list, fsb, show)
      fsb.cat(')')
    }
  }

  override def hashCode: Int = uType.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case obj: MultipleNodeKindTest => uType == obj.uType
    case _ => false

  }

}
