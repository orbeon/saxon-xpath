////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.NamespaceNode

import org.orbeon.saxon.tree.iter.AxisIterator

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.tree.util.Navigator

import java.util.ArrayList

import java.util.List

import java.util.function.Predicate

import java.util.function.Supplier

import VirtualCopy._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


object VirtualCopy {

  def makeVirtualCopy(original: NodeInfo): VirtualCopy = {
    var vc: VirtualCopy = null
    var nodeInfo: NodeInfo = original
    // Don't allow copies of copies of copies: define the new copy in terms of the original
    while (original.isInstanceOf[VirtualCopy]) nodeInfo =
      nodeInfo.asInstanceOf[VirtualCopy].original
    vc = new VirtualCopy(original, original)
    val config: Configuration = original.getConfiguration
    val doc: VirtualTreeInfo = new VirtualTreeInfo(config, vc)
    val docNr: Long =
      config.getDocumentNumberAllocator.allocateDocumentNumber()
    doc.setDocumentNumber(docNr)
    vc.tree = doc
    vc
  }

}

class VirtualCopy(var original: NodeInfo,
                  var root: NodeInfo)
  extends NodeInfo {

  var systemIdSupplier: Supplier[String] = () => this.getBaseURI

  var parent: VirtualCopy = _

  var tree: VirtualTreeInfo = _

  private var dropNamespaces: Boolean = false

  /*@NotNull*/

  def wrap(node: NodeInfo): VirtualCopy = {
    val vc: VirtualCopy = new VirtualCopy(node, root)
    vc.tree = tree
    vc.systemIdSupplier = systemIdSupplier
    vc.dropNamespaces = dropNamespaces
    vc
  }

  def getOriginalNode: NodeInfo = original

  /**
   * Get information about the tree to which this NodeInfo belongs
   *
   * @return the TreeInfo
   * @since 9.7
   */
  def getTreeInfo: VirtualTreeInfo = tree

  def setDropNamespaces(drop: Boolean): Unit = {
    this.dropNamespaces = drop
  }

  override def getAllNamespaces: NamespaceMap =
    if (getNodeKind == Type.ELEMENT) {
      if (dropNamespaces) {
        var nsMap: NamespaceMap = NamespaceMap.emptyMap
        val ns: String = getURI
        if (!ns.isEmpty) {
          nsMap = nsMap.put(getPrefix, ns)
        }
        val iter: AxisIterator = original.iterateAxis(AxisInfo.ATTRIBUTE)
        var att: NodeInfo = null
        while (({
          att = iter.next()
          att
        }) != null) if (att.getURI.!=("")) {
          nsMap = nsMap.put(att.getPrefix, att.getURI)
        }
        nsMap
      } else {
        original.getAllNamespaces
      }
    } else {
      null
    }

  /**
   * Get fingerprint. The fingerprint is a coded form of the expanded name
   * of the node: two nodes
   * with the same name code have the same namespace URI and the same local name.
   * The fingerprint contains no information about the namespace prefix. For a name
   * in the null namespace, the fingerprint is the same as the name code.
   *
   * @return an integer fingerprint; two nodes with the same fingerprint have
   *         the same expanded QName. For unnamed nodes (text nodes, comments, document nodes,
   *         and namespace nodes for the default namespace), returns -1.
   * @throws UnsupportedOperationException if this kind of node does not hold
   *                                       namepool fingerprints (specifically, if { @link #hasFingerprint} returns false).
   * @since 8.4 (moved into FingerprintedNode at some stage; then back into NodeInfo at 9.8).
   */
  override def getFingerprint: Int = original.getFingerprint

  /**
   * Ask whether this NodeInfo implementation holds a fingerprint identifying the name of the
   * node in the NamePool. If the answer is true, then the {@link #getFingerprint} method must
   * return the fingerprint of the node. If the answer is false, then the {@link #getFingerprint}
   * method should throw an {@code UnsupportedOperationException}. In the case of unnamed nodes
   * such as text nodes, the result can be either true (in which case getFingerprint should
   * return -1) or false (in which case getFingerprint may throw an exception).
   *
   * @return true if the implementation of this node provides fingerprints.
   * @since 9.8; previously Saxon relied on using <code>FingerprintedNode</code> as a marker interface.
   */
  override def hasFingerprint: Boolean = original.hasFingerprint

  def getNodeKind: Int = original.getNodeKind

  override def equals(other: Any): Boolean =
    other.isInstanceOf[VirtualCopy] &&
      getTreeInfo == other.asInstanceOf[VirtualCopy].getTreeInfo &&
      original == other.asInstanceOf[VirtualCopy].original

  override def hashCode: Int =
    original.hashCode ^
      ((getTreeInfo.getDocumentNumber & 0x7fffffff).toInt << 19)

  def getSystemId: String = systemIdSupplier.get

  /*@Nullable*/

  override def getBaseURI: String = Navigator.getBaseURI(this)

  override def getLineNumber: Int = original.getLineNumber

  override def getColumnNumber(): Int = original.getColumnNumber

  /**
   * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
   * should not be saved for later use. The result of this operation holds the same location information,
   * but in an immutable form.
   */
  def saveLocation(): Location = this

  def compareOrder(other: NodeInfo): Int =
    if (other.isInstanceOf[VirtualCopy]) {
      val c: Int = root.compareOrder(other.asInstanceOf[VirtualCopy].root)
      if (c == 0) {
        original.compareOrder(other.asInstanceOf[VirtualCopy].original)
      } else {
        c
      }
    } else {
      other.compareOrder(original)
    }

  def getStringValue: String = getStringValueCS.toString

  def getStringValueCS: CharSequence = original.getStringValueCS

  def getLocalPart: String = original.getLocalPart

  def getURI: String = original.getURI

  def getPrefix: String = original.getPrefix

  def getDisplayName: String = original.getDisplayName

  override def getConfiguration: Configuration = original.getConfiguration

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
  override def getSchemaType: SchemaType = original.getSchemaType

  /*@Nullable*/

  def getParent: NodeInfo = {
    if (original == root) {
      return null
    }
    if (parent == null) {
      val basep: NodeInfo = original.getParent
      if (basep == null) {
        return null
      }
      parent = wrap(basep)
    }
    parent
  }

  def iterateAxis(axisNumber: Int,
                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator = {
    var newParent: VirtualCopy = null
    axisNumber match {
      case AxisInfo.CHILD | AxisInfo.ATTRIBUTE => newParent = this
      case AxisInfo.SELF | AxisInfo.PRECEDING_SIBLING |
           AxisInfo.FOLLOWING_SIBLING =>
        newParent = parent
      // that relies on getParent to escape from the subtree
      case AxisInfo.ANCESTOR =>
        new Navigator.AxisFilter(
          new Navigator.AncestorEnumeration(this, false),
          nodeTest)
      case AxisInfo.ANCESTOR_OR_SELF =>
        new Navigator.AxisFilter(new Navigator.AncestorEnumeration(this, true),
          nodeTest)
      case AxisInfo.NAMESPACE =>
        if (getNodeKind != Type.ELEMENT) {
          EmptyIterator.ofNodes
        }
        NamespaceNode.makeIterator(this, nodeTest)
      case AxisInfo.PARENT => Navigator.filteredSingleton(getParent, nodeTest)
      case AxisInfo.PRECEDING =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, false),
          nodeTest)
      case AxisInfo.FOLLOWING =>
        new Navigator.AxisFilter(new Navigator.FollowingEnumeration(this),
          nodeTest)
      case AxisInfo.PRECEDING_OR_ANCESTOR =>
        new Navigator.AxisFilter(
          new Navigator.PrecedingEnumeration(this, true),
          nodeTest)

    }
    // Ensure that the ancestor, ancestor-or-self, following, and preceding axes use an implementation
    makeCopier(original.iterateAxis(axisNumber, nodeTest),
      newParent,
      !AxisInfo.isSubtreeAxis(axisNumber))
  }

  /**
   * Get the string value of a given attribute of this node
   *
   * @param uri   the namespace URI of the attribute name. Supply the empty string for an attribute
   *              that is in no namespace
   * @param local the local part of the attribute name.
   * @return the attribute value if it exists, or null if it does not exist. Always returns null
   *         if this node is not an element.
   */
  def getAttributeValue(uri: String, local: String): String =
    original.getAttributeValue(uri, local)

  /*@Nullable*/

  def getRoot: NodeInfo = {
    var n: NodeInfo = this
    while (true) {
      val p: NodeInfo = n.getParent
      if (p == null) {
        return n
      }
      n = p
    }
    n
  }

  def hasChildNodes: Boolean = original.hasChildNodes

  def generateId(buffer: FastStringBuffer): Unit = {
    buffer.append("d")
    buffer.append(java.lang.Long.toString(getTreeInfo.getDocumentNumber))
    original.generateId(buffer)
  }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    original.copy(out, copyOptions, locationId)
  }

  def getDeclaredNamespaces(
                             buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    if (getNodeKind == Type.ELEMENT) {
      if (dropNamespaces) {
        val allNamespaces: List[NamespaceBinding] =
          new ArrayList[NamespaceBinding](5)
        val ns: String = getURI
        if (ns.isEmpty) {
          if (getParent != null && !getParent.getURI.isEmpty) {
            allNamespaces.add(new NamespaceBinding("", ""))
          }
        } else {
          allNamespaces.add(new NamespaceBinding(getPrefix, getURI))
        }
        for (att <- original.attributes) {
          val name: NodeName = att.getNodeName
          if (name.getURI != null) {
            val b: NamespaceBinding =
              new NamespaceBinding(name.getPrefix, name.getURI)
            if (!allNamespaces.contains(b)) {
              allNamespaces.add(b)
            }
          }
        }
        allNamespaces.toArray(NamespaceBinding.EMPTY_ARRAY)
      } else {
        if (original == root) {
          val bindings: List[NamespaceBinding] = new ArrayList[NamespaceBinding]()
          //          for (binding <- original.getAllNamespaces) {
          //            bindings.add(binding)
          //          }
          original.getAllNamespaces.asScala.foreach(binding => bindings.add(binding))
          bindings.toArray(NamespaceBinding.EMPTY_ARRAY)
        } else {
          original.getDeclaredNamespaces(buffer)
        }
      }
    } else {
      null
    }

  /**
   * Set the system identifier for this Source.
   * <p>The system identifier is optional if the source does not
   * get its data from a URL, but it may still be useful to provide one.
   * The application can use a system identifier, for example, to resolve
   * relative URIs and to include in error messages and warnings.</p>
   *
   * @param systemId The system identifier as a URL string.
   */
  def setSystemId(systemId: String): Unit = {
    this.systemIdSupplier = () => systemId
  }

  def atomize(): AtomicSequence = original.atomize()

  override def isId: Boolean = original.isId

  override def isIdref(): Boolean = original.isIdref

  override def isNilled(): Boolean = original.isNilled

  /*@Nullable*/

  override def getPublicId: String =
    if (original != null) original.getPublicId else null

  def isIncludedInCopy(sourceNode: NodeInfo): Boolean =
    Navigator.isAncestorOrSelf(root, sourceNode)

  def makeCopier(axis: AxisIterator,
                 newParent: VirtualCopy,
                 testInclusion: Boolean): VirtualCopier =
    new VirtualCopier(axis, newParent, testInclusion)

  class VirtualCopier(var base: AxisIterator,
                      private var parent: VirtualCopy,
                      var testInclusion: Boolean)
    extends AxisIterator {

    /*@Nullable*/

    def next(): NodeInfo = {
      var next: NodeInfo = base.next()
      if (next != null) {
        if (testInclusion && !isIncludedInCopy(next)) {
          //         the subtree.
          return null
        }
        // we're only interested in nodes within the subtree that was copied.
        // Assert: once we find a node outside this subtree, all further nodes will also be outside
        // we're only interested in nodes within the subtree that was copied.
        // Assert: once we find a node outside this subtree, all further nodes will also be outside
        val vc: VirtualCopy = wrap(next)
        vc.parent = parent
        vc.systemIdSupplier = systemIdSupplier
        next = vc
      }
      next
    }

    override def close(): Unit = {
      base.close()
    }

  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class represents a node that is a virtual copy of another node: that is, it behaves as a node that's the
 * same as another node, but has different identity. Moreover, this class can create a virtual copy of a subtree,
 * so that the parent of the virtual copy is null rather than being a virtual copy of the parent of the original.
 * This means that none of the axes when applied to the virtual copy is allowed to stray outside the subtree.
 * The virtual copy is implemented by means of a reference to the node of which
 * it is a copy, but methods that are sensitive to node identity return a different result.
 *
 */
