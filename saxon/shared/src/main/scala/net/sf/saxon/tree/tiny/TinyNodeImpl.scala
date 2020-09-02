////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A node in a TinyTree representing an XML element, character content, or attribute.
  * <p>This is the top-level class in the implementation class hierarchy; it essentially contains
  * all those methods that can be defined using other primitive methods, without direct access
  * to data.</p>
  *
  * @author Michael H. Kay
  */

package net.sf.saxon.tree.tiny

import net.sf.saxon.utils.Configuration
import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.Type
import net.sf.saxon.model.UType
import net.sf.saxon.om._
import net.sf.saxon.pattern.AnyNodeTest
import net.sf.saxon.pattern.NameTest
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.s9api.Location
import net.sf.saxon.tree.NamespaceNode
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.PrependAxisIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.tree.util.Navigator
import java.util.function.Predicate

import TinyNodeImpl._
import net.sf.saxon.om.Genre.Genre

object TinyNodeImpl{

  /*@NotNull*/

  val NODE_LETTER: Array[Char] =
    Array('x', 'e', 'a', 't', 'x', 'x', 'x', 'p', 'c', 'r', 'x', 'x', 'x', 'n')

  def getParentNodeNr(tree: TinyTree, nodeNr: Int): Int = {
    if (tree.depth(nodeNr) == 0) {
      return -1
    }
    var p: Int = tree.next(nodeNr)
    while (p > nodeNr) {
      if (tree.nodeKind(p) == Type.PARENT_POINTER) {
        tree.alpha(p)
      }
      p = tree.next(p)
    }
    p
  }

}

abstract class TinyNodeImpl extends NodeInfo {

   var tree: TinyTree = _

   var nodeNr: Int = _

  /*@Nullable*/

   var parent: TinyNodeImpl = null

  /**
    * Get the genre of this item
    *
    * @return the genre
    */
  override def getGenre: Genre = Genre.NODE

  /**
    * Get information about the tree to which this NodeInfo belongs
    *
    * @return the TreeInfo
    * @since 9.7
    */
  def getTreeInfo: TreeInfo = tree

  override def head: NodeInfo = this

  def getStringValueCS: CharSequence = getStringValue

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
  override def getSchemaType: SchemaType = null

  override def getColumnNumber(): Int = tree.getColumnNumber(nodeNr)

  def setSystemId(uri: String): Unit = {
    tree.setSystemId(nodeNr, uri)
  }

   def setParentNode(parent: TinyNodeImpl): Unit = {
    this.parent = parent
  }

  override def isSameNodeInfo(other: NodeInfo): Boolean =
    (this eq other) ||
      (other.isInstanceOf[TinyNodeImpl] && tree == other
        .asInstanceOf[TinyNodeImpl]
        .tree &&
        nodeNr == other.asInstanceOf[TinyNodeImpl].nodeNr &&
        getNodeKind == other.getNodeKind)

  override def equals(other: Any): Boolean = other match {
    case other: NodeInfo => isSameNodeInfo(other)
    case _ => false

  }

  override def hashCode: Int =
    ((tree.getDocumentNumber & 0x3ff).toInt << 20) ^ nodeNr ^
      (getNodeKind << 14)

  def getSystemId: String = tree.getSystemId(nodeNr)

  def getBaseURI: String = getParent.getBaseURI

  override def getLineNumber: Int = tree.getLineNumber(nodeNr)

  /**
    * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
    * should not be saved for later use. The result of this operation holds the same location information,
    * but in an immutable form.
    */
  def saveLocation(): Location = this

  def getSequenceNumber: Long = nodeNr.toLong << 32

  def compareOrder(other: NodeInfo): Int = {
    val a: Long = getSequenceNumber
    if (other.isInstanceOf[TinyNodeImpl]) {
      val b: Long = other.asInstanceOf[TinyNodeImpl].getSequenceNumber
      java.lang.Long.compare(a, b)
    } else {
      0 - other.compareOrder(this)
    }
  }

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
  override def hasFingerprint: Boolean = true

  def getFingerprint: Int = {
    val nc: Int = tree.nameCode(nodeNr)
    if (nc == -1) {
      return -1
    }
    nc & NamePool.FP_MASK
  }

  def getPrefix: String = {
    val code: Int = tree.nameCode(nodeNr)
    if (code < 0) {
      return ""
    }
    if (!NamePool.isPrefixed(code)) {
      return ""
    }
    tree.prefixPool.getPrefix(code >> 20)
  }

  def getURI: String = {
    val code: Int = tree.nameCode(nodeNr)
    if (code < 0) {
      return ""
    }
    tree.getNamePool.getURI(code & NamePool.FP_MASK)
  }

  def getDisplayName: String = {
    val code: Int = tree.nameCode(nodeNr)
    if (code < 0) {
      return ""
    }
    if (NamePool.isPrefixed(code)) {
      getPrefix + ":" + getLocalPart
    } else {
      getLocalPart
    }
  }

  def getLocalPart: String = {
    val code: Int = tree.nameCode(nodeNr)
    if (code < 0) {
      return ""
    }
    tree.getNamePool.getLocalName(code)
  }

  override def iterateAxis(axisNumber: Int): AxisIterator =
    if (axisNumber == AxisInfo.CHILD) {
      if (hasChildNodes) {
        new SiblingIterator(tree, this, null, true)
      } else {
        EmptyIterator.ofNodes
      }
    } else {
      iterateAxis(axisNumber, AnyNodeTest.getInstance)
    }

  def iterateAxis(axisNumber: Int,
                  predicate: Predicate[_ >: NodeInfo]): AxisIterator =
    if (predicate.isInstanceOf[NodeTest]) {
      val nodeTest: NodeTest = predicate.asInstanceOf[NodeTest]
      val `type`: Int = getNodeKind
      axisNumber match {
        case AxisInfo.ANCESTOR => new AncestorIterator(this, nodeTest)
        case AxisInfo.ANCESTOR_OR_SELF =>
          var ancestors: AxisIterator = new AncestorIterator(this, nodeTest)
          if (nodeTest.test(this)) {
            new PrependAxisIterator(this, ancestors)
          } else {
            ancestors
          }
        case AxisInfo.ATTRIBUTE =>
          if (`type` != Type.ELEMENT) {
            EmptyIterator.ofNodes
          }
          if (tree.alpha(nodeNr) < 0) {
            EmptyIterator.ofNodes
          }
          new AttributeIterator(tree, nodeNr, nodeTest)
        case AxisInfo.CHILD =>
          if (hasChildNodes) {
            if (nodeTest.isInstanceOf[NameTest] &&
              nodeTest.asInstanceOf[NameTest].getNodeKind == Type.ELEMENT) {
              // fast path for common case
              new NamedChildIterator(tree, this, nodeTest.getFingerprint)
            } else {
              new SiblingIterator(tree, this, nodeTest, true)
            }
          } else {
            EmptyIterator.ofNodes
          }
        case AxisInfo.DESCENDANT =>
          if (`type` == Type.DOCUMENT && nodeTest.isInstanceOf[NameTest] &&
            nodeTest.getPrimitiveType == Type.ELEMENT) {
            this
              .asInstanceOf[TinyDocumentImpl]
              .getAllElements(nodeTest.getFingerprint)
          } else if (hasChildNodes) {
            if (nodeTest.getUType.overlaps(UType.TEXT)) {
              new DescendantIterator(tree, this, nodeTest)
            } else {
              new DescendantIteratorSansText(tree, this, nodeTest)
            }
          } else {
            EmptyIterator.ofNodes
          }
        case AxisInfo.DESCENDANT_OR_SELF =>
          var descendants: AxisIterator =
            iterateAxis(AxisInfo.DESCENDANT, nodeTest)
          if (nodeTest.test(this)) {
            new PrependAxisIterator(this, descendants)
          } else {
            descendants
          }
        case AxisInfo.FOLLOWING =>
          if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE) {
            new FollowingIterator(tree, getParent, nodeTest, true)
          } else if (tree.depth(nodeNr) == 0) {
            EmptyIterator.ofNodes
          } else {
            new FollowingIterator(tree, this, nodeTest, false)
          }
        case AxisInfo.FOLLOWING_SIBLING =>
          if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE ||
            tree.depth(nodeNr) == 0) {
            EmptyIterator.ofNodes
          } else {
            new SiblingIterator(tree, this, nodeTest, false)
          }
        case AxisInfo.NAMESPACE =>
          if (`type` != Type.ELEMENT) {
            EmptyIterator.ofNodes
          }
          NamespaceNode.makeIterator(this, nodeTest)
        case AxisInfo.PARENT =>
          var parent: NodeInfo = getParent
          Navigator.filteredSingleton(parent, nodeTest)
        case AxisInfo.PRECEDING =>
          if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE) {
            new PrecedingIterator(tree, getParent, nodeTest, false)
          } else if (tree.depth(nodeNr) == 0) {
            EmptyIterator.ofNodes
          } else {
            new PrecedingIterator(tree, this, nodeTest, false)
          }
        case AxisInfo.PRECEDING_SIBLING =>
          if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE ||
            tree.depth(nodeNr) == 0) {
            EmptyIterator.ofNodes
          } else {
            new PrecedingSiblingIterator(tree, this, nodeTest)
          }
        case AxisInfo.SELF => Navigator.filteredSingleton(this, nodeTest)
        case AxisInfo.PRECEDING_OR_ANCESTOR =>
          if (`type` == Type.DOCUMENT) {
            EmptyIterator.ofNodes
          } else if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE) {
            val el: TinyNodeImpl = getParent
            new PrependAxisIterator(
              el,
              new PrecedingIterator(tree, el, nodeTest, true))
          } else {
            new PrecedingIterator(tree, this, nodeTest, true)
          }
        case _ =>
          throw new IllegalArgumentException(
            "Unknown axis number " + axisNumber)

      }
    } else {
      new Navigator.AxisFilter(
        iterateAxis(axisNumber, AnyNodeTest.getInstance),
        predicate)
    }

  /*@Nullable*/

  def getParent: TinyNodeImpl = {
    if (parent != null) {
      return parent
    }
    val p: Int = getParentNodeNr(tree, nodeNr)
    if (p == -1) {
      return null
    } else {
      parent = tree.getNode(p)
    }
    null
  }

  def hasChildNodes: Boolean = false

  def getAttributeValue(uri: String, local: String): String = null

  def getRoot: NodeInfo = if (nodeNr == 0) this else tree.getRootNode

  override def getConfiguration: Configuration = tree.getConfiguration

  def getNamePool: NamePool = tree.getNamePool

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
  override def getAllNamespaces: NamespaceMap = null

  def generateId(buffer: FastStringBuffer): Unit = {
    buffer.append("d")
    buffer.append(java.lang.Long.toString(tree.getDocumentNumber))
    buffer.cat(NODE_LETTER(getNodeKind))
    buffer.append(java.lang.Integer.toString(nodeNr))
  }

  def isAncestorOrSelf(d: TinyNodeImpl): Boolean = {
    if (tree != d.tree) {
      return false
    }
    var dn: Int = d.nodeNr
    if (d.isInstanceOf[TinyAttributeImpl]) {
      if (this.isInstanceOf[TinyAttributeImpl]) {
        return nodeNr == dn
      } else {
        dn = tree.attParent(dn)
      }
    }
    if (this.isInstanceOf[TinyAttributeImpl]) {
      return false
    }
    if (nodeNr > dn) {
      return false
    }
    if (nodeNr == dn) {
      return true
    }
    if (!(this.isInstanceOf[TinyParentNodeImpl])) {
      return false
    }
    if (tree.depth(nodeNr) >= tree.depth(dn)) {
      return false
    }
    var n: Int = nodeNr
    while (true) {
      val nextSib: Int = tree.next(n)
      if (nextSib < 0 || nextSib > dn) {
        true
      } else if (tree.depth(nextSib) == 0) {
        true
      } else if (nextSib < n) {
        n = nextSib
      } else {
        false
      }
    }
    false
  }

  override def isId: Boolean = false

  override def isIdref(): Boolean = false

  override def isNilled(): Boolean = tree.isNilled(nodeNr)

  /**
    * Ask whether this is a node in a streamed document
    *
    * @return true if the node is in a document being processed using streaming
    */
  override def isStreamed(): Boolean = false

  def getTree: TinyTree = tree

  def getNodeNumber: Int = nodeNr

}

