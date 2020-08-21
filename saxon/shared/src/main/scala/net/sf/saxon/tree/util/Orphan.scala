////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.util

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Builder

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.SingleNodeIterator

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.UntypedAtomicValue

import java.util.function.Predicate

import scala.beans.{BeanProperty, BooleanBeanProperty}




class Orphan(config: Configuration) extends MutableNodeInfo {

  private var kind: Short = _

  /*@Nullable*/

  private var nodeName: NodeName = null

  private var stringValue: CharSequence = _

  private var typeAnnotation: SchemaType = null

  private var options: Int = ReceiverOption.NONE

  @BeanProperty
  var treeInfo: GenericTreeInfo = new GenericTreeInfo(config)

  treeInfo.setRootNode(this)

  /**
    * Get the System ID for the node. Note this is not the
    * same as the base URI: the base URI can be modified by xml:base, but
    * the system ID cannot. The base URI is used primarily for resolving
    * relative URIs within the content of the document. The system ID is
    * used primarily in conjunction with a line number, for identifying the
    * location of elements within the source XML, in particular when errors
    * are found. For a document node, the System ID represents the value of
    * the document-uri property as defined in the XDM data model.
    *
    * @return the System Identifier of the entity in the source document
    * containing the node, or null if not known or not applicable.
    * @since 8.4
    */
  override def getSystemId(): String = treeInfo.getSystemId

  /**
    * Get the Public ID of the entity containing the node.
    *
    * @return the Public Identifier of the entity in the source document
    * containing the node, or null if not known or not applicable
    * @since 9.7
    */
  override def getPublicId(): String = treeInfo.getPublicId

  /**
    * Set the system identifier for this Source.
    * <p>
    * <p>The system identifier is optional if the source does not
    * get its data from a URL, but it may still be useful to provide one.
    * The application can use a system identifier, for example, to resolve
    * relative URIs and to include in error messages and warnings.</p>
    *
    * @param systemId The system identifier as a URL string.
    */
  override def setSystemId(systemId: String): Unit = {
    treeInfo.setSystemId(systemId)
  }

  /**
    * Get the effective boolean value of this sequence
    * @return the effective boolean value (always true for an Orphan node)
    */
  override def effectiveBooleanValue(): Boolean = true

  def setNodeKind(kind: Short): Unit = {
    this.kind = kind
  }

  def setNodeName(nodeName: NodeName): Unit = {
    this.nodeName = nodeName
  }

  def setStringValue(stringValue: CharSequence): Unit = {
    this.stringValue = stringValue
  }

  def setTypeAnnotation(typeAnnotation: SchemaType): Unit = {
    this.typeAnnotation = typeAnnotation
  }

  def setIsId(id: Boolean): Unit = {
    setOption(ReceiverOption.IS_ID, id)
  }

  private def setOption(option: Int, on: Boolean): Unit = {
    if (on) {
      options |= option
    } else {
      options &= ~option
    }
  }

  private def isOption(option: Int): Boolean =
    ReceiverOption.contains(options, option)

  def setIsIdref(idref: Boolean): Unit = {
    setOption(ReceiverOption.IS_IDREF, idref)
  }

  def setDisableOutputEscaping(doe: Boolean): Unit = {
    setOption(ReceiverOption.DISABLE_ESCAPING, doe)
  }

  def getNodeKind(): Int = kind

  /**
    * Get fingerprint. The fingerprint is a coded form of the expanded name
    * of the node: two nodes
    * with the same name code have the same namespace URI and the same local name.
    * The fingerprint contains no information about the namespace prefix. For a name
    * in the null namespace, the fingerprint is the same as the name code.
    *
    * @return an integer fingerprint; two nodes with the same fingerprint have
    * the same expanded QName. For unnamed nodes (text nodes, comments, document nodes,
    * and namespace nodes for the default namespace), returns -1.
    * @throws UnsupportedOperationException if this kind of node does not hold
    *                                       namepool fingerprints (specifically, if {@link #hasFingerprint()} returns false).
    * @since 8.4 (moved into FingerprintedNode at some stage; then back into NodeInfo at 9.8).
    */
  override def getFingerprint(): Int =
    throw new UnsupportedOperationException()

  /**
    * Ask whether this NodeInfo implementation holds a fingerprint identifying the name of the
    * node in the NamePool. If the answer is true, then the {@link #getFingerprint} method must
    * return the fingerprint of the node. If the answer is false, then the {@link #getFingerprint}
    * method should throw an {@code UnsupportedOperationException}. In the case of unnamed nodes
    * such as text nodes, the result can be either true (in which case getFingerprint() should
    * return -1) or false (in which case getFingerprint may throw an exception).
    *
    * @return true if the implementation of this node provides fingerprints.
    * @since 9.8; previously Saxon relied on using <code>FingerprintedNode</code> as a marker interface.
    */
  override def hasFingerprint(): Boolean = false

  def atomize(): AtomicSequence = getNodeKind match {
    case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
      new StringValue(stringValue)
    case Type.TEXT | Type.DOCUMENT | Type.NAMESPACE =>
      new UntypedAtomicValue(stringValue)
    case _ =>
      if (typeAnnotation == null || typeAnnotation == Untyped.getInstance ||
          typeAnnotation == BuiltInAtomicType.UNTYPED_ATOMIC) {
        new UntypedAtomicValue(stringValue)
      } else {
        typeAnnotation.atomize(this)
      }

  }

  /**
    * Get the type annotation of this node, if any. The type annotation is represented as
    * SchemaType object.
    * <p>Types derived from a DTD are not reflected in the result of this method.</p>
    *
    * @return For element and attribute nodes: the type annotation derived from schema
    *         validation (defaulting to xs:untyped and xs:untypedAtomic in the absence of schema
    *         validation). For comments, text nodes, processing instructions, and namespaces: null.
    *         For document nodes, either xs:untyped if the document has not been validated, or
    *         xs:anyType if it has.
    * @since 9.4
    */
  override def getSchemaType(): SchemaType = {
    if (typeAnnotation == null) {
      if (kind == Type.ELEMENT) {
        Untyped.getInstance
      } else if (kind == Type.ATTRIBUTE) {
        BuiltInAtomicType.UNTYPED_ATOMIC
      }
    }
    typeAnnotation
  }

  def equals(other: NodeInfo): Boolean = this eq other

  override def hashCode(): Int = super.hashCode

  /*@Nullable*/

  def getBaseURI(): String =
    if (kind == Type.PROCESSING_INSTRUCTION) {
      getSystemId
    } else {
      null
    }

  /**
    * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
    * should not be saved for later use. The result of this operation holds the same location information,
    * but in an immutable form.
    */
  def saveLocation(): Location = this

  def compareOrder(other: NodeInfo): Int = {
// are they the same node?
    if (this eq other) {
      0
    }
    if (this.hashCode < other.hashCode) -1 else +1
  }

  def getStringValue(): String = stringValue.toString

  def getStringValueCS(): CharSequence = stringValue

  def getLocalPart(): String =
    if (nodeName == null) {
      ""
    } else {
      nodeName.getLocalPart
    }

  def getURI(): String =
    if (nodeName == null) {
      ""
    } else {
      nodeName.getURI
    }

  def getPrefix(): String =
    if (nodeName == null) {
      ""
    } else {
      nodeName.getPrefix
    }

  def getDisplayName(): String =
    if (nodeName == null) {
      ""
    } else {
      nodeName.getDisplayName
    }

  /*@Nullable*/

  def getParent(): NodeInfo = null

  /*@NotNull*/

  override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
    case AxisInfo.ANCESTOR_OR_SELF | AxisInfo.DESCENDANT_OR_SELF |
        AxisInfo.SELF =>
      SingleNodeIterator.makeIterator(this)
    case AxisInfo.ANCESTOR | AxisInfo.ATTRIBUTE | AxisInfo.CHILD |
        AxisInfo.DESCENDANT | AxisInfo.FOLLOWING | AxisInfo.FOLLOWING_SIBLING |
        AxisInfo.NAMESPACE | AxisInfo.PARENT | AxisInfo.PRECEDING |
        AxisInfo.PRECEDING_SIBLING | AxisInfo.PRECEDING_OR_ANCESTOR =>
      EmptyIterator.ofNodes()
    case _ =>
      throw new IllegalArgumentException("Unknown axis number " + axisNumber)

  }

  /*@NotNull*/

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    axisNumber match {
      case AxisInfo.ANCESTOR_OR_SELF | AxisInfo.DESCENDANT_OR_SELF |
          AxisInfo.SELF =>
        Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.ANCESTOR | AxisInfo.ATTRIBUTE | AxisInfo.CHILD |
          AxisInfo.DESCENDANT | AxisInfo.FOLLOWING |
          AxisInfo.FOLLOWING_SIBLING | AxisInfo.NAMESPACE | AxisInfo.PARENT |
          AxisInfo.PRECEDING | AxisInfo.PRECEDING_SIBLING |
          AxisInfo.PRECEDING_OR_ANCESTOR =>
        EmptyIterator.ofNodes()
      case _ =>
        throw new IllegalArgumentException("Unknown axis number " + axisNumber)

    }

  /**
    * Get the string value of a given attribute of this node
    *
    * @param uri   the namespace URI of the attribute name. Supply the empty string for an attribute
    *              that is in no namespace
    * @param local the local part of the attribute name.
    * @return the attribute value if it exists, or null if it does not exist. Always returns null
    *         if this node is not an element.
    * @since 9.4
    */
  def getAttributeValue(uri: String, local: String): String = null

  /*@NotNull*/

  def getRoot(): NodeInfo = this

  def hasChildNodes(): Boolean = false

  def generateId(buffer: FastStringBuffer): Unit = {
    buffer.cat('Q')
    buffer.append(java.lang.Integer.toString(hashCode))
  }

  /*@Nullable*/

  def getDeclaredNamespaces(
      buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

  /**
    * Get all the namespace bindings that are in-scope for this element.
    * <p>For an element return all the prefix-to-uri bindings that are in scope. This may include
    * a binding to the default namespace (represented by a prefix of ""). It will never include
    * "undeclarations" - that is, the namespace URI will never be empty; the effect of an undeclaration
    * is to remove a binding from the in-scope namespaces, not to add anything.</p>
    * <p>For a node other than an element, returns null.</p>
    *
    * @return the in-scope namespaces for an element, or null for any other kind of node.
    */
  override def getAllNamespaces(): NamespaceMap = null

  override def isId(): Boolean =
    isOption(ReceiverOption.IS_ID) ||
      (kind == Type.ATTRIBUTE && nodeName == StandardNames.XML_ID_NAME)

  override def isIdref(): Boolean = isOption(ReceiverOption.IS_IDREF)

  def isDisableOutputEscaping(): Boolean =
    isOption(ReceiverOption.DISABLE_ESCAPING)

  def insertChildren(source: Array[NodeInfo],
                     atStart: Boolean,
                     inherit: Boolean): Unit = {}
// no action: node is not a document or element node
// no action: node is not a document or element node

  def insertSiblings(source: Array[NodeInfo],
                     before: Boolean,
                     inherit: Boolean): Unit = {}
// no action: node has no parent
// no action: node has no parent

  /**
    * Set the attribute list for this (element) node
    *
    * @param attributes the new attribute list
    * @throws UnsupportedOperationException if this is not an element node
    */
  override def setAttributes(attributes: AttributeMap): Unit = {
    throw new UnsupportedOperationException()
  }

  def removeAttribute(attribute: NodeInfo): Unit = {}
// no action: node is not an element
// no action: node is not an element

  def addAttribute(nameCode: NodeName,
                   attType: SimpleType,
                   value: CharSequence,
                   properties: Int): Unit = {}
// no action: node is not an element
// no action: node is not an element

  def delete(): Unit = {
// no action other than to mark it deleted: node has no parent from which it can be detached
    kind = -1
  }

  def isDeleted(): Boolean = kind == -1

  def replace(replacement: Array[NodeInfo], inherit: Boolean): Unit = {
    throw new IllegalStateException("Cannot replace a parentless node")
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    this.stringValue = stringValue
  }

  def rename(newNameCode: NodeName): Unit = {
    if (kind == Type.ATTRIBUTE || kind == Type.PROCESSING_INSTRUCTION) {
      nodeName = newNameCode
    }
  }

  def addNamespace(nscode: NamespaceBinding): Unit = {}
// no action: node is not an element
// no action: node is not an element

  def removeTypeAnnotation(): Unit = {
    typeAnnotation = BuiltInAtomicType.UNTYPED_ATOMIC
  }

  /*@NotNull*/

  def newBuilder(): Builder =
    throw new UnsupportedOperationException(
      "Cannot create children for an Orphan node")

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A node (implementing the NodeInfo interface) representing an attribute, text node,
  * comment, processing instruction, or namespace that has no parent (and of course no children).
  * Exceptionally it is also used (during whitespace stripping) to represent a standalone element.
  * <p>In general this class does not impose constraints defined in the data model: that is the responsibility
  * of the client. For example, the class does not prevent you from creating a comment or text node that has
  * a name or a non-trivial type annotation.</p>
  *
  * @author Michael H. Kay
  */
