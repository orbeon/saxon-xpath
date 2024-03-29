////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree

import java.util.ArrayList
import java.util.function.Predicate

import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, Type}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.iter.{AxisIterator, EmptyIterator, ListIterator, PrependAxisIterator}
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.StringValue


/**
  * This class represents a namespace node; it is used in several tree models.
  */
object NamespaceNode {

  /*@NotNull*/
  def makeIterator(element: NodeInfo, test: Predicate[_ >: NodeInfo]): AxisIterator = {
    val nodes = new ArrayList[NodeInfo]
    val bindings = element.getAllNamespaces.iterator
    var position = 0
    var foundXML = false
    while (bindings.hasNext) {
      val binding = bindings.next()
      if (binding.getPrefix.==("xml")) {
        foundXML = true
      }
      val node =
        new NamespaceNode(element, binding, {
          position = position + 1
          position
        })

      if (test.test(node))
        nodes.add(node)
    }
    if (! foundXML) {
      val node = new NamespaceNode(element, NamespaceBinding.XML, position)
      if (test.test(node))
        nodes.add(node)
    }
    new ListIterator.OfNodes(nodes)
  }
}

class NamespaceNode(var element: NodeInfo,
                    nscode: NamespaceBinding,
                    var position: Int)
    extends NodeInfo {

  var nsBinding: NamespaceBinding = nscode
  var fingerprint: Int = -1

  /**
    * Get information about the tree to which this NodeInfo belongs
    *
    * @return the TreeInfo
    * @since 9.7
    */
  def getTreeInfo: TreeInfo = element.getTreeInfo

  override def head: NodeInfo = this

  def getNodeKind: Int = Type.NAMESPACE

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NamespaceNode] &&
      element == other.asInstanceOf[NamespaceNode].element &&
      nsBinding == other.asInstanceOf[NamespaceNode].nsBinding

  override def hashCode: Int = element.hashCode ^ (position << 13)

  /*@Nullable*/
  def getSystemId: String = element.getSystemId

  /**
    * Get the Public ID of the entity containing the node.
    *
    * @return the Public Identifier of the entity in the source document
    * containing the node, or null if not known or not applicable
    * @since 9.7
    */
  override def getPublicId: String = element.getPublicId

  /*@Nullable*/
  def getBaseURI: String = // the base URI of a namespace node is the empty sequence
    null

  override def getLineNumber: Int = element.getLineNumber
  override def getColumnNumber: Int = element.getColumnNumber

  /**
    * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
    * should not be saved for later use. The result of this operation holds the same location information,
    * but in an immutable form.
    */
  def saveLocation: Location = this

  def compareOrder(other: NodeInfo): Int =
    other match {
      case nsNode: NamespaceNode if element == nsNode.element =>
        val c = position - nsNode.position
        java.lang.Integer.compare(c, 0)
      case _ =>
        if (element == other)
          +1
        else
          element.compareOrder(other)
    }

  def getStringValue: String = nsBinding.getURI
  def getStringValueCS: CharSequence = getStringValue

  /**
    * Ask whether this NodeInfo implementation holds a fingerprint identifying the name of the
    * node in the NamePool. If the answer is true, then the `#` method must
    * return the fingerprint of the node. If the answer is false, then the `#`
    * method should throw an `UnsupportedOperationException`. In the case of unnamed nodes
    * such as text nodes, the result can be either true (in which case getFingerprint should
    * return -1) or false (in which case getFingerprint may throw an exception).
    *
    * @return true if the implementation of this node provides fingerprints.
    * @since 9.8; previously Saxon relied on using <code>FingerprintedNode</code> as a marker interface.
    */
  override def hasFingerprint: Boolean = true

  def getFingerprint: Int = {
    if (fingerprint == -1) {
      if (nsBinding.getPrefix.isEmpty) {
        return -1
      } else {
        fingerprint = element.getConfiguration.getNamePool.allocateFingerprint("", nsBinding.getPrefix)
      }
    }
    fingerprint
  }

  def getLocalPart: String = nsBinding.getPrefix
  /*@NotNull*/
  def getURI: String = ""
  def getDisplayName: String = getLocalPart
  /*@NotNull*/
  def getPrefix: String = ""

  override def getConfiguration: Configuration = element.getConfiguration
  def getNamePool: NamePool = getConfiguration.getNamePool

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
  override def getSchemaType: SchemaType = BuiltInAtomicType.STRING

  def getParent: NodeInfo = element

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    axisNumber match {
      case AxisInfo.ANCESTOR =>
        element.iterateAxis(AxisInfo.ANCESTOR_OR_SELF, nodeTest)
      case AxisInfo.ANCESTOR_OR_SELF =>
        if (nodeTest.test(this)) {
          new PrependAxisIterator(
            this,
            element.iterateAxis(AxisInfo.ANCESTOR_OR_SELF, nodeTest))
        } else {
          element.iterateAxis(AxisInfo.ANCESTOR_OR_SELF, nodeTest)
        }
      case AxisInfo.ATTRIBUTE | AxisInfo.CHILD | AxisInfo.DESCENDANT |
          AxisInfo.DESCENDANT_OR_SELF | AxisInfo.FOLLOWING_SIBLING |
          AxisInfo.NAMESPACE | AxisInfo.PRECEDING_SIBLING =>
        EmptyIterator.ofNodes
      case AxisInfo.FOLLOWING =>
        new Navigator.AxisFilter(new Navigator.FollowingEnumeration(this),
                                 nodeTest)
      case AxisInfo.PARENT => Navigator.filteredSingleton(element, nodeTest)
      case AxisInfo.PRECEDING =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, false),
          nodeTest)
      case AxisInfo.SELF => Navigator.filteredSingleton(this, nodeTest)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, true),
          nodeTest)
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

  def getRoot: NodeInfo = element.getRoot

  def hasChildNodes: Boolean = false

  def generateId(buffer: FastStringBuffer): Unit = {
    element.generateId(buffer)
    buffer.append("n")
    buffer.append(java.lang.Integer.toString(position))
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    out.append(this)

  /*@Nullable*/
  def getDeclaredNamespaces(buffer: Array[NamespaceBinding]): Array[NamespaceBinding] = null

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
  def getAllNamespaces: NamespaceMap = null

  /**
    * Set the system identifier for this Source.
    * <p>The system identifier is optional if the source does not
    * get its data from a URL, but it may still be useful to provide one.
    * The application can use a system identifier, for example, to resolve
    * relative URIs and to include in error messages and warnings.</p>
    *
    * @param systemId The system identifier as a URL string.
    */
  def setSystemId(systemId: String): Unit = ()

  /*@NotNull*/
  def atomize(): AtomicSequence = new StringValue(getStringValueCS)

  override def isStreamed: Boolean = element.isStreamed
}
