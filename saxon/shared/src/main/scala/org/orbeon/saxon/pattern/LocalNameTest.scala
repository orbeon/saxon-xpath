////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pattern

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om._

import org.orbeon.saxon.tree.tiny.NodeVectorTree

import org.orbeon.saxon.tree.tiny.TinyTree

import org.orbeon.saxon.z.IntSet

import java.util.Map

import java.util.Optional

import java.util.function.IntPredicate

import scala.beans.{BeanProperty, BooleanBeanProperty}




class LocalNameTest(pool: NamePool,
                    @BeanProperty var nodeKind: Int,
                    @BeanProperty var localName: String)
    extends NodeTest
    with QNameTest {

  @BeanProperty
  var namePool: NamePool = pool

  private var uType: UType = UType.fromTypeCode(nodeKind)

  /**
    * Get the corresponding {@link org.orbeon.saxon.model.UType}. A UType is a union of primitive item
    * types.
    *
    * @return the smallest UType that subsumes this item type
    */
  def getUType: UType = uType

  /**
    * Get the set of node names allowed by this NodeTest. Return no result, because the
    * set of names cannot be represented.
    *
    * @return the set of integer fingerprints of the node names that this node test can match; or absent
    * if the set of names cannot be represented (for example, with the name tests *:xxx or xxx:*)
    */
  override def getRequiredNodeNames(): Optional[IntSet] = // See bug 3713
    Optional.empty()

  override def getFullAlphaCode(): String =
    getBasicAlphaCode + " n*:" + localName

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
                       annotation: SchemaType): Boolean =
    name != null && nodeKind == this.nodeKind && localName == name.getLocalPart

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    val nameCodeArray: Array[Int] = tree.getNameCodeArray
    if (nodeKind == Type.ELEMENT && tree.isInstanceOf[TinyTree]) {
      val localNameIndex: Map[String, IntSet] =
        tree.asInstanceOf[TinyTree].getLocalNameIndex
      val intSet: IntSet = localNameIndex.get(localName)
      if (intSet == null) { (i) =>
        false
      } else { (nodeNr) =>
        intSet.contains(nameCodeArray(nodeNr) & NamePool.FP_MASK) &&
        (nodeKindArray(nodeNr) & 0x0f) == Type.ELEMENT
      }
    } else { (nodeNr) =>
      (nodeKindArray(nodeNr) & 0x0f) == nodeKind &&
      localName == namePool.getLocalName(
        nameCodeArray(nodeNr) & NamePool.FP_MASK)
    }
  }

  override def test(node: NodeInfo): Boolean =
    localName == node.getLocalPart && nodeKind == node.getNodeKind

  def matches(qname: StructuredQName): Boolean =
    localName == qname.getLocalPart

  def getDefaultPriority: Double = -0.25

  override def getPrimitiveType: Int = nodeKind

  override def toString: String = nodeKind match {
    case Type.ELEMENT => "*:" + localName
    case Type.ATTRIBUTE => "@*:" + localName
    case _ => // should not be used
      "(*" + nodeKind + "*):" + localName

  }

  override def hashCode: Int = nodeKind << 20 ^ localName.hashCode

  /**
    * Indicates whether some other object is "equal to" this one.
    */
  override def equals(other: Any): Boolean =
    other.isInstanceOf[LocalNameTest] &&
      other.asInstanceOf[LocalNameTest].nodeKind == nodeKind &&
      other.asInstanceOf[LocalNameTest].localName == localName

  /**
    * Export the QNameTest as a string for use in a SEF file (typically in a catch clause).
    *
    * @return a string representation of the QNameTest, suitable for use in export files. The format is
    * a sequence of alternatives separated by vertical bars, where each alternative is one of '*',
    * '*:localname', 'Q{uri}*', or 'Q{uri}local'.
    */
  override def exportQNameTest: String = "*:" + localName

  /**
    * Generate Javascript code to test if a name matches the test.
    *
    * @return JS code as a string. The generated code will be used
    * as the body of a JS function in which the argument name "q" is an
    * XdmQName object holding the name. The XdmQName object has properties
    * uri and local.
    * @param targetVersion the version of Saxon-JS being targeted
    */
  def generateJavaScriptNameTest(targetVersion: Int): String =
    "q.local==='" + localName + "'"

  /**
    * Get extra diagnostic information about why a supplied item does not conform to this
    * item type, if available. If extra information is returned, it should be in the form of a complete
    * sentence, minus the closing full stop. No information should be returned for obvious cases.
    *
    * @param item the item that doesn't match this type
    * @param th   the type hierarchy cache
    * @return optionally, a message explaining why the item does not match the type
    */
  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] = {
    val explanation: Optional[String] = super.explainMismatch(item, th)
    if (explanation.isPresent) {
      return explanation
    }
    Optional.of("The node has the wrong local name")
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * NodeTest is an interface that enables a test of whether a node has a particular
  * name and type. A LocalNameTest matches the node type and the local name,
  * it represents an XPath 2.0 test of the form *:name.
  */
