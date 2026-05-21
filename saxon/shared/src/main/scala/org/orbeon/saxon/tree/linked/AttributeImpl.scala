package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.event.{Receiver, ReceiverOption}
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, SimpleType, Type}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.util.FastStringBuffer

class AttributeImpl(element: ElementImpl, index: Int) extends NodeImpl {

  this.setRawParent(element)

  this.setSiblingPosition(index)

  private def getAttributeInfo: AttributeInfo =
    getRawParent.attributes.itemAt(getSiblingPosition)

  override def getNodeName: NodeName =
    if (getRawParent == null || getSiblingPosition == -1)
      null
    else
      getAttributeInfo.nodeName

  override def getFingerprint: Int =
    if (getRawParent == null || getSiblingPosition == -1)
      -1
    else
      getNodeName.obtainFingerprint(getNamePool)

  override def getSchemaType: SchemaType = getAttributeInfo.`type`

  override def isId: Boolean = getAttributeInfo.isId

  override def isIdref: Boolean =
    if (ReceiverOption.contains(getAttributeInfo.properties, ReceiverOption.IS_IDREF))
      true
    else
      ElementImpl.isIdRefNode(this)

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
    if (parseq == -1L) parseq else parseq + 0x8000 + getSiblingPosition
  }

  def getNodeKind: Int = Type.ATTRIBUTE

  def getStringValue: String = getAttributeInfo.value

  override def getNextSibling: NodeImpl = null

  override def getPreviousSibling: NodeImpl = null

  override def getPreviousInDocument: NodeImpl = getParent

  override def getNextInDocument(anchor: NodeImpl): NodeImpl =
    if (anchor == this)
      null
    else
      getParent.getNextInDocument(anchor)

  override def generateId(buffer: FastStringBuffer): Unit = {
    getParent.generateId(buffer)
    buffer.cat('a')
    buffer.append(java.lang.Integer.toString(getSiblingPosition))
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    throw new IllegalArgumentException

  override def delete(): Unit =
    if (!isDeleted) {
      if (getRawParent != null)
        getRawParent.removeAttribute(this)
      this.setRawParent(null)
      this.setSiblingPosition(-1)
    }

  override def isDeleted: Boolean =
    getRawParent == null ||
      getAttributeInfo.isInstanceOf[AttributeInfo.Deleted] ||
      getRawParent.isDeleted

  override def replace(replacement: Array[NodeInfo], inherit: Boolean): Unit = {
    if (isDeleted)
      throw new IllegalStateException("Cannot replace a deleted node")
    if (getParent == null)
      throw new IllegalStateException("Cannot replace a parentless node")
    val element: ParentNodeImpl = getRawParent
    delete()
    for (n <- replacement) {
      if (n.getNodeKind != Type.ATTRIBUTE)
        throw new IllegalArgumentException("Replacement nodes must be attributes")
      element.addAttribute(
        NameOfNode.makeName(n),
        BuiltInAtomicType.UNTYPED_ATOMIC,
        n.getStringValue,
        ReceiverOption.NONE
      )
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
          att.value,
          att.location,
          att.properties
        )
      )
      val newURI: String = newNameCode.getURI
      if (newURI.nonEmpty) {
        val newPrefix: String = newNameCode.getPrefix
        val newBinding: NamespaceBinding =
          new NamespaceBinding(newPrefix, newURI)
        val oldURI: String = getRawParent
          .asInstanceOf[ElementImpl]
          .getURIForPrefix(newPrefix, useDefault = false)
        if (oldURI == null)
          owner.addNamespace(newBinding)
        else if (oldURI != newURI)
          throw new IllegalArgumentException("Namespace binding of new name conflicts with existing namespace binding")
      }
    }
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(
        getSiblingPosition,
        new AttributeInfo(
          att.nodeName,
          att.`type`,
          stringValue.toString,
          att.location,
          att.properties
        )
      )
    }
  }

  override def removeTypeAnnotation(): Unit = {
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(
        getSiblingPosition,
        new AttributeInfo(
          att.nodeName,
          BuiltInAtomicType.UNTYPED_ATOMIC,
          att.value,
          att.location,
          att.properties
        )
      )
    }
  }

  override def setTypeAnnotation(`type`: SchemaType): Unit = {
    if (! `type`.isInstanceOf[SimpleType])
      throw new IllegalArgumentException("Attribute type must be a simple type")
    val owner: ElementImpl = getRawParent.asInstanceOf[ElementImpl]
    if (owner != null && !isDeleted) {
      val att: AttributeInfo = getAttributeInfo
      owner.setAttributeInfo(
        getSiblingPosition,
        new AttributeInfo(att.nodeName,
          `type`.asInstanceOf[SimpleType],
          att.value,
          att.location,
          att.properties
        )
      )
      owner.removeTypeAnnotation()
    }
  }
}