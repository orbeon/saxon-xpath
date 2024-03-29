package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, Type}
import org.orbeon.saxon.om.{AtomicSequence, NamePool, NodeInfo}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.util.FastStringBuffer


class TinyAttributeImpl(treeImpl: TinyTree, nodeNrImpl: Int) extends TinyNodeImpl {

  this.tree = treeImpl
  this.nodeNr = nodeNrImpl

  override def setSystemId(uri: String): Unit = ()

  override def getSystemId: String = {
    val parent = getParent
    if (parent == null)
      null
    else
      getParent.getSystemId
  }

  override def getParent: TinyNodeImpl = tree.getNode(tree.attParent(nodeNr))

  override def getRoot: NodeInfo = {
    val parent = getParent
    if (parent == null)
      this
    else
      parent.getRoot
  }

  override def getSequenceNumber: Long =
    getParent.getSequenceNumber + 0x8000 + (nodeNr - tree.alpha(tree.attParent(nodeNr)))

  def getNodeKind: Int = Type.ATTRIBUTE

  override def getStringValueCS: CharSequence = tree.attValue(nodeNr)

  def getStringValue: String = tree.attValue(nodeNr).toString

  override def getFingerprint: Int = tree.attCode(nodeNr) & 0xfffff

  def getNameCode: Int = tree.attCode(nodeNr)

  override def getPrefix: String = {
    val code = tree.attCode(nodeNr)
    if (!NamePool.isPrefixed(code))
      ""
    else
      tree.prefixPool.getPrefix(code >> 20)
  }

  override def getDisplayName: String = {
    val code = tree.attCode(nodeNr)
    if (code < 0)
      ""
    else if (NamePool.isPrefixed(code))
      getPrefix + ":" + getLocalPart
    else
      getLocalPart
  }

  override def getLocalPart: String =
    tree.getNamePool.getLocalName(tree.attCode(nodeNr))

  override def getURI: String = {
    val code = tree.attCode(nodeNr)
    if (! NamePool.isPrefixed(code))
      ""
    else
      tree.getNamePool.getURI(code)
  }

  override def getSchemaType: SchemaType =
    if (tree.attType == null)
      BuiltInAtomicType.UNTYPED_ATOMIC
    else
      tree.getAttributeType(nodeNr)

  def atomize(): AtomicSequence = tree.getTypedValueOfAttribute(this, nodeNr)

  override def generateId(buffer: FastStringBuffer): Unit = {
    getParent.generateId(buffer)
    buffer.append("a")
    buffer.append(java.lang.Integer.toString(tree.attCode(nodeNr)))
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    throw new UnsupportedOperationException("copy() applied to attribute node")

  override def getLineNumber  : Int     = getParent.getLineNumber
  override def getColumnNumber: Int     = getParent.getColumnNumber
  override def isId           : Boolean = tree.isIdAttribute(nodeNr)
  override def isIdref        : Boolean = tree.isIdrefAttribute(nodeNr)

  def isDefaultedAttribute: Boolean = tree.isDefaultedAttribute(nodeNr)

  override def hashCode: Int =
    ((tree.getDocumentNumber & 0x3ff).toInt << 20) ^ nodeNr ^ 7 << 17
}
