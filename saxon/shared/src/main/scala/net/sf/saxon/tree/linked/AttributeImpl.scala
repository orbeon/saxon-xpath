package net.sf.saxon.tree.linked

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer


class AttributeImpl(element: ElementImpl, index: Int) extends NodeImpl {

  this.setRawParent(element)

  this.setSiblingPosition(index)

  private def getAttributeInfo: AttributeInfo =
    getRawParent.attributes.itemAt(getSiblingPosition)

  override def getNodeName: NodeName = {
    if (getRawParent == null || getSiblingPosition == -1) {
      return null
    }
    getAttributeInfo.getNodeName
  }

  override def getFingerprint: Int = {
    if (getRawParent == null || getSiblingPosition == -1) {
      return -1
    }
    getNodeName.obtainFingerprint(getNamePool)
  }

  override def getSchemaType: SchemaType = getAttributeInfo.getType

  override def isId: Boolean = getAttributeInfo.isId

  override def isIdref(): Boolean = {
    if (ReceiverOption.contains(getAttributeInfo.getProperties,
      ReceiverOption.IS_IDREF)) {
      return true
    }
    ElementImpl.isIdRefNode(this)
  }

  override def equals(other: Any): Boolean =
    other match {
      case o: AttributeImpl if this eq o => true
      case o: AttributeImpl => getRawParent == o.getRawParent && getSiblingPosition == o.getSiblingPosition
      case _ => false
    }

  override def hashCode: Int =
    getRawParent.hashCode ^ (getSiblingPosition << 16)

  override def getSequenceNumber: Long = {
    val parseq: Long = getRawParent.getSequenceNumber
    (if (parseq == -1L) parseq else parseq + 0x8000 + getSiblingPosition)
  }

  def getNodeKind: Int = Type.ATTRIBUTE

  def getStringValue: String = getAttributeInfo.getValue

  override def getNextSibling: NodeImpl = null

  override def getPreviousSibling: NodeImpl = null

  override def getPreviousInDocument(): NodeImpl = getParent

  override def getNextInDocument(anchor: NodeImpl): NodeImpl = {
    if (anchor == this) return null
    getParent.getNextInDocument(anchor)
  }

  override def generateId(buffer: FastStringBuffer): Unit = {
    getParent.generateId(buffer)
    buffer.cat('a')
    buffer.append(java.lang.Integer.toString(getSiblingPosition))
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    throw new IllegalArgumentException()
  }

  override def delete(): Unit = {
    if (!isDeleted) {
      if (getRawParent != null) {
        getRawParent.removeAttribute(this)
      }
      this.setRawParent(null)
      this.setSiblingPosition(-1)
    }
  }

  override def isDeleted: Boolean =
    getRawParent == null ||
      getAttributeInfo.isInstanceOf[AttributeInfo.Deleted] ||
      getRawParent.isDeleted

  override def replace(replacement: Array[NodeInfo], inherit: Boolean): Unit = {
    if (isDeleted) {
      throw new IllegalStateException("Cannot replace a deleted node")
    }
    if (getParent == null) {
      throw new IllegalStateException("Cannot replace a parentless node")
    }
    val element: ParentNodeImpl = getRawParent
    delete()
    for (n <- replacement) {
      if (n.getNodeKind != Type.ATTRIBUTE) {
        throw new IllegalArgumentException(
          "Replacement nodes must be attributes")
      }
      element.addAttribute(NameOfNode.makeName(n),
        BuiltInAtomicType.UNTYPED_ATOMIC,
        n.getStringValue,
        ReceiverOption.NONE)
    }
  }

  override def rename(newNameCode: NodeName): Unit = {
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(
        getSiblingPosition,
        new AttributeInfo(newNameCode,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          att.getValue,
          att.getLocation,
          att.getProperties))
      val newURI: String = newNameCode.getURI
      if (!newURI.isEmpty) {
        val newPrefix: String = newNameCode.getPrefix
        val newBinding: NamespaceBinding =
          new NamespaceBinding(newPrefix, newURI)
        val oldURI: String = getRawParent
          .asInstanceOf[ElementImpl]
          .getURIForPrefix(newPrefix, useDefault = false)
        if (oldURI == null) {
          owner.addNamespace(newBinding)
        } else if (oldURI != newURI) {
          throw new IllegalArgumentException(
            "Namespace binding of new name conflicts with existing namespace binding")
        }
      }
    }
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(getSiblingPosition,
        new AttributeInfo(att.getNodeName,
          att.getType,
          stringValue.toString,
          att.getLocation,
          att.getProperties))
    }
  }

  override def removeTypeAnnotation(): Unit = {
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(
        getSiblingPosition,
        new AttributeInfo(att.getNodeName,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          att.getValue,
          att.getLocation,
          att.getProperties))
      owner.removeTypeAnnotation()
    }
  }

  override def setTypeAnnotation(`type`: SchemaType): Unit = {
    if (!(`type`.isInstanceOf[SimpleType])) {
      throw new IllegalArgumentException(
        "Attribute type must be a simple type")
    }
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(getSiblingPosition,
        new AttributeInfo(att.getNodeName,
          `type`.asInstanceOf[SimpleType],
          att.getValue,
          att.getLocation,
          att.getProperties))
      owner.removeTypeAnnotation()
    }
  }

}