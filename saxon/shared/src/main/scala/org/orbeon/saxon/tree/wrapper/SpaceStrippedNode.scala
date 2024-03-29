////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import java.util.function.Predicate

import org.orbeon.saxon.event.{Receiver, Stripper}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{ComplexType, SchemaType, Type, UType}
import org.orbeon.saxon.om.{AtomicSequence, AxisInfo, NameOfNode, NodeInfo}
import org.orbeon.saxon.pattern.{NodeKindTest, NodeTest}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.{AxisIterator, EmptyIterator}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.tree.wrapper.SpaceStrippedNode._
import org.orbeon.saxon.value.Whitespace

import scala.util.control.Breaks._


/**
 * A StrippedNode is a view of a node, in a virtual tree that has whitespace
 * text nodes stripped from it. All operations on the node produce the same result
 * as operations on the real underlying node, except that iterations over the axes
 * take care to skip whitespace-only text nodes that are supposed to be stripped.
 * Note that this class is only used in cases where a pre-built tree is supplied as
 * the input to a transformation, and where the stylesheet does whitespace stripping;
 * if a SAXSource or StreamSource is supplied, whitespace is stripped as the tree
 * is built.
 */
object SpaceStrippedNode {

  /*@NotNull*/

  def makeWrapper(node: NodeInfo,
                  docWrapper: SpaceStrippedDocument,
                  parent: SpaceStrippedNode): SpaceStrippedNode = {
    val wrapper: SpaceStrippedNode = new SpaceStrippedNode(node, parent)
    wrapper.docWrapper = docWrapper
    wrapper
  }

  def isPreservedNode(node: NodeInfo,
                      docWrapper: SpaceStrippedDocument,
                      actualParent: NodeInfo): Boolean = {
    // Non-text nodes, non-whitespace nodes, and parentless nodes are preserved
    if (node.getNodeKind != Type.TEXT || actualParent == null || ! Whitespace.isWhite(node.getStringValueCS))
      return true
    // if the node has a simple type annotation, it is preserved
    val `type`: SchemaType = actualParent.getSchemaType
    if (`type`.isSimpleType || `type`.asInstanceOf[ComplexType].isSimpleContent)
      return true
    // if there is an ancestor with xml:space="preserve", it is preserved
    if (docWrapper.containsPreserveSpace()) {
      var p: NodeInfo = actualParent
      // if one of them is on an ancestor of this node
      breakable {
        while (p.getNodeKind == Type.ELEMENT) {
          val `val` = p.getAttributeValue(NamespaceConstant.XML, "space")
          if (`val` != null) {
            if ("preserve" == `val`) {
              return true
            } else if ("default" == `val`) {
              break()
            }
          }
          p = p.getParent
        }
      }
    }
    // the document contains one or more xml:space="preserve" attributes, so we need to see
    // if there is an ancestor whose type has an assertion, it is preserved
    if (docWrapper.containsAssertions) {
      var p: NodeInfo = actualParent
      // if one of them is on an ancestor of this node
      while (p.getNodeKind == Type.ELEMENT) {
        p.getSchemaType match {
          case complexType: ComplexType if complexType.hasAssertions =>
            return true
          case _ =>
        }
        p = p.getParent
      }
    }
    // the document contains one or more xml:space="preserve" attributes, so we need to see
    // otherwise it depends on xsl:strip-space
    try {
      val preserve = docWrapper.getStrippingRule.isSpacePreserving(NameOfNode.makeName(actualParent), null)
      preserve == Stripper.ALWAYS_PRESERVE
    } catch {
      case _: XPathException => true
    }
  }
}

class SpaceStrippedNode()
  extends AbstractVirtualNode
    with WrappingFunction {

  def this(node: NodeInfo, parent: SpaceStrippedNode) = {
    this()
    this.node = node
    this.parent = parent
  }

  /*@NotNull*/
  def makeWrapper(node: NodeInfo, parent: VirtualNode): VirtualNode = {
    val wrapper = new SpaceStrippedNode(node, parent.asInstanceOf[SpaceStrippedNode])
    wrapper.docWrapper = this.docWrapper
    wrapper
  }

  override def atomize(): AtomicSequence =
    if (getNodeKind == Type.ELEMENT)
      getSchemaType.atomize(this)
    else
      node.atomize()

  override def equals(other: Any): Boolean =
    other match {
      case ssn: SpaceStrippedNode =>
        node == ssn.node
      case _ =>
        node == other
    }

  override def compareOrder(other: NodeInfo): Int =
    other match {
      case ssn: SpaceStrippedNode =>
        node.compareOrder(ssn.node)
      case _ =>
        node.compareOrder(other)
    }

  override def getStringValueCS: CharSequence =
    // Might not be the same as the string value of the underlying node because of space stripping
    getNodeKind match {
      case Type.DOCUMENT | Type.ELEMENT =>
        val iter: AxisIterator = iterateAxis(
          AxisInfo.DESCENDANT,
          NodeKindTest.makeNodeKindTest(Type.TEXT))
        val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
        breakable {
          while (true) {
            val it: NodeInfo = iter.next()
            if (it == null) {
              break()
            }
            sb.cat(it.getStringValueCS)
          }
        }
        sb.condense()
      case _ => node.getStringValueCS

    }

  /*@Nullable*/

  def getParent: NodeInfo = {
    if (parent == null) {
      val realParent: NodeInfo = node.getParent
      if (realParent != null) {
        parent = SpaceStrippedNode.makeWrapper(realParent,
          docWrapper.asInstanceOf[SpaceStrippedDocument],
          null)
      }
    }
    parent
  }

  override def iterateAxis(axisNumber: Int,
                           nodeTest: Predicate[_ >: NodeInfo]): AxisIterator =
    if (nodeTest.isInstanceOf[NodeTest] &&
      nodeTest.asInstanceOf[NodeTest].getUType.intersection(UType.TEXT) ==
        UType.VOID ||
      axisNumber == AxisInfo.ATTRIBUTE ||
      axisNumber == AxisInfo.NAMESPACE) {
      // iteration does not include text nodes, so no stripping needed
      new WrappingIterator(node.iterateAxis(axisNumber, nodeTest),
        this,
        getParentForAxis(axisNumber))
    } else {
      new StrippingIterator(node.iterateAxis(axisNumber, nodeTest),
        getParentForAxis(axisNumber))
    }

  /*@Nullable*/
  override def iterateAxis(axisNumber: Int): AxisIterator = axisNumber match {
    case AxisInfo.ATTRIBUTE | AxisInfo.NAMESPACE =>
      new WrappingIterator(node.iterateAxis(axisNumber), this, this)
    case AxisInfo.CHILD =>
      new StrippingIterator(node.iterateAxis(axisNumber), this)
    case AxisInfo.FOLLOWING_SIBLING | AxisInfo.PRECEDING_SIBLING =>
      val parent: SpaceStrippedNode = getParent.asInstanceOf[SpaceStrippedNode]
      if (parent == null) {
        EmptyIterator.ofNodes
      } else {
        new StrippingIterator(node.iterateAxis(axisNumber), parent)
      }
    case _ => new StrippingIterator(node.iterateAxis(axisNumber), null)

  }

  private def getParentForAxis(axisNumber: Int): SpaceStrippedNode =
    axisNumber match {
      case AxisInfo.CHILD | AxisInfo.ATTRIBUTE | AxisInfo.NAMESPACE => this
      case AxisInfo.FOLLOWING_SIBLING | AxisInfo.PRECEDING_SIBLING =>
        getParent.asInstanceOf[SpaceStrippedNode]
      case _ => null

    }

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    // version of the document (test case strip-space-008)
    val temp: Receiver = out
    val stripper: Stripper = new Stripper(
      docWrapper.asInstanceOf[SpaceStrippedDocument].getStrippingRule,
      temp)
    node.copy(stripper, copyOptions, locationId)
  }

  // The underlying code does not do whitespace stripping. So we need to interpose
  // a stripper. Moreover, if the node is typed and we are removing type annotations,
  // then we need to take care that we're not applying space-stripping to the untyped
  // The underlying code does not do whitespace stripping. So we need to interpose
  // a stripper. Moreover, if the node is typed and we are removing type annotations,
  // then we need to take care that we're not applying space-stripping to the untyped

  private class StrippingIterator(var base: AxisIterator,
                                  var parent: SpaceStrippedNode)
    extends AxisIterator {

    var currentVirtualNode: NodeInfo = _

    var position: Int = 0

    /*@Nullable*/

    def next(): NodeInfo = {
      var nextRealNode: NodeInfo = null
      breakable {
        while (true) {
          nextRealNode = base.next()
          if (nextRealNode == null) {
            return null
          }
          if (isPreserved(nextRealNode)) {
            break()
          }
        }
      }
      // otherwise skip this whitespace text node
      // otherwise skip this whitespace text node
      currentVirtualNode = SpaceStrippedNode.makeWrapper(
        nextRealNode,
        docWrapper.asInstanceOf[SpaceStrippedDocument],
        parent
      )
      position += 1
      currentVirtualNode
    }

    private def isPreserved(nextRealNode: NodeInfo): Boolean = {
      if (nextRealNode.getNodeKind != Type.TEXT) {
        return true
      }
      val actualParent: NodeInfo =
        if (parent == null) nextRealNode.getParent else parent.node
      isPreservedNode(nextRealNode,
        docWrapper.asInstanceOf[SpaceStrippedDocument],
        actualParent)
    }

    override def close(): Unit =
      base.close()
  }
}
