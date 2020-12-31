////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pattern

import org.orbeon.saxon.model.{SchemaType, Type, TypeHierarchy, UType}
import org.orbeon.saxon.om.{AxisInfo, Item, NodeInfo, NodeName}
import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.tree.iter.AxisIterator

import scala.beans.BeanProperty


/**
 * A DocumentNodeTest implements the test document-node(element(~,~))
 */
class DocumentNodeTest(@BeanProperty var elementTest: NodeTest)
  extends NodeTest {

  /**
   * Get the corresponding {@link org.orbeon.saxon.model.UType}. A UType is a union of primitive item
   * types.
   *
   * @return the smallest UType that subsumes this item type
   */
  def getUType: UType = UType.DOCUMENT

  /**
   * Test whether this node test is satisfied by a given node. This method is only
   * fully supported for a subset of NodeTests, because it doesn't provide all the information
   * needed to evaluate all node tests. In particular (a) it can't be used to evaluate a node
   * test of the form element(N,T) or schema-element(E) where it is necessary to know whether the
   * node is nilled, and (b) it can't be used to evaluate a node test of the form
   * document-node(element(X)). This in practice means that it is used (a) to evaluate the
   * simple node tests found in the XPath 1.0 subset used in XML Schema, and (b) to evaluate
   * node tests where the node kind is known to be an attribute.
   *
   * @param nodeKind   The kind of node to be matched
   * @param name       identifies the expanded name of the node to be matched.
   *                   The value should be null for a node with no name.
   * @param annotation The actual content type of the node
   */
  override def matches(nodeKind: Int,
                       name: NodeName,
                       annotation: SchemaType): Boolean = {
    if (nodeKind != Type.DOCUMENT) {
      return false
    }
    throw new UnsupportedOperationException(
      "DocumentNodeTest doesn't support this method")
  }

  override def test(node: NodeInfo): Boolean = {
    if (node.getNodeKind != Type.DOCUMENT) {
      return false
    }
    val iter: AxisIterator = node.iterateAxis(AxisInfo.CHILD)
    // children, and the element node matches the element test.
    var found: Boolean = false
    var n: NodeInfo = null
    while ({
      n = iter.next()
      n
    } != null) {
      val kind: Int = n.getNodeKind
      if (kind == Type.TEXT) {
        return false
      } else if (kind == Type.ELEMENT) {
        if (found) {
          return false
        }
        if (elementTest.test(n)) {
          found = true
        } else {
          return false
        }
      }
    }
    found
  }

  // The match is true if there is exactly one element node child, no text node
  // The match is true if there is exactly one element node child, no text node

  def getDefaultPriority: Double = elementTest.getDefaultPriority
  override def getPrimitiveType: Int = Type.DOCUMENT
  override def toString: String = "document-node(" + elementTest + ')'
  override def hashCode: Int = elementTest.hashCode ^ 12345

  override def equals(other: Any): Boolean = other match {
    case other: DocumentNodeTest => other.elementTest == elementTest
    case _ => false

  }

  override def getFullAlphaCode: String =
    getBasicAlphaCode + " e[" + elementTest.getFullAlphaCode +
      "]"

  /**
   * Get extra diagnostic information about why a supplied item does not conform to this
   * item type, if available. If extra information is returned, it should be in the form of a complete
   * sentence, minus the closing full stop. No information should be returned for obvious cases.
   *
   * @param item the item that doesn't match this type
   * @param th   the type hierarchy cache
   * @return optionally, a message explaining why the item does not match the type
   */
  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = {

    val explanation = super.explainMismatch(item, th)
    if (explanation.isDefined)
      return explanation

    val node = item.asInstanceOf[NodeInfo]
    val iter = node.iterateAxis(AxisInfo.CHILD)

    // children, and the element node matches the element test.
    var found = false
    var n: NodeInfo = null
    while ({
      n = iter.next()
      n
    } != null) {
      val kind = n.getNodeKind
      if (kind == Type.TEXT) {
        return Some("The supplied document node has text node children")
      } else if (kind == Type.ELEMENT) {
        if (found)
          return Some("The supplied document node has more than one element child")
        if (elementTest.test(n)) {
          found = true
        } else {
          var s = "The supplied document node has an element child (" +
            Err.depict(n) +
            ") that does not satisfy the element test"
          val more = elementTest.explainMismatch(n, th)
          if (more.isDefined)
            s += ". " + more.get
          return Some(s)
        }
      }
    }
    None
  }
}

