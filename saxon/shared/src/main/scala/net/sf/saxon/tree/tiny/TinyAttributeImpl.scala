package net.sf.saxon.tree.tiny

import net.sf.saxon.event.Receiver

import net.sf.saxon.lib.FeatureKeys

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.NamePool

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer


class TinyAttributeImpl(treeImpl: TinyTree, nodeNrImpl: Int) extends TinyNodeImpl {

  this.tree = treeImpl

  this.nodeNr = nodeNrImpl

  override def setSystemId(uri: String): Unit = ()

  override def getSystemId: String = {
    val parent: NodeInfo = getParent
    if (parent == null) null else getParent.getSystemId
  }

  override def getParent: TinyNodeImpl = tree.getNode(tree.attParent(nodeNr))

  override def getRoot(): NodeInfo = {
    val parent: NodeInfo = getParent
    if (parent == null) {
      this
    } else {
      parent.getRoot
    }
  }

  override  def getSequenceNumber(): Long =
    getParent.getSequenceNumber +
      0x8000 +
      (nodeNr - tree.alpha(tree.attParent(nodeNr)))

  def getNodeKind: Int = Type.ATTRIBUTE

  override def getStringValueCS: CharSequence = tree.attValue(nodeNr)

  def getStringValue: String = tree.attValue(nodeNr).toString

  override def getFingerprint(): Int = tree.attCode(nodeNr) & 0xfffff

  def getNameCode: Int = tree.attCode(nodeNr)

  override def getPrefix: String = {
    val code: Int = tree.attCode(nodeNr)
    if (!NamePool.isPrefixed(code)) {
      return ""
    }
    tree.prefixPool.getPrefix(code >> 20)
  }

  override def getDisplayName: String = {
    val code: Int = tree.attCode(nodeNr)
    if (code < 0) {
      return ""
    }
    if (NamePool.isPrefixed(code)) {
      getPrefix + ":" + getLocalPart
    } else {
      getLocalPart
    }
  }

  override def getLocalPart: String =
    tree.getNamePool.getLocalName(tree.attCode(nodeNr))

  override def getURI: String = {
    val code: Int = tree.attCode(nodeNr)
    if (!NamePool.isPrefixed(code)) {
      return ""
    }
    tree.getNamePool.getURI(code)
  }

  override def getSchemaType(): SchemaType = {
    if (tree.attType == null) {
      BuiltInAtomicType.UNTYPED_ATOMIC
    }
    tree.getAttributeType(nodeNr)
  }

  def atomize(): AtomicSequence = tree.getTypedValueOfAttribute(this, nodeNr)

  override def generateId(buffer: FastStringBuffer): Unit = {
    getParent.generateId(buffer)
    buffer.append("a")
    buffer.append(java.lang.Integer.toString(tree.attCode(nodeNr)))
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    throw new UnsupportedOperationException("copy() applied to attribute node")
  }

  override def getLineNumber(): Int = getParent.getLineNumber

  override def getColumnNumber(): Int = getParent.getColumnNumber

  override def isId(): Boolean = tree.isIdAttribute(nodeNr)

  override def isIdref(): Boolean = tree.isIdrefAttribute(nodeNr)

  def isDefaultedAttribute: Boolean = tree.isDefaultedAttribute(nodeNr)

  override def hashCode(): Int =
    ((tree.getDocumentNumber & 0x3ff).toInt << 20) ^ nodeNr ^
      7 << 17

}
