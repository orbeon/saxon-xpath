////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.model.UType

import net.sf.saxon.om._

import net.sf.saxon.tree.tiny.NodeVectorTree

import net.sf.saxon.tree.util.Navigator

import net.sf.saxon.z.IntSet

import net.sf.saxon.z.IntSingletonSet

import java.util.Optional

import java.util.function.IntPredicate




class SameNameTest(private var origin: NodeInfo)
    extends NodeTest
    with QNameTest {

  def getNodeKind: Int = origin.getNodeKind

  /**
    * Get the corresponding {@link net.sf.saxon.model.UType}. A UType is a union of primitive item
    * types.
    *
    * @return the smallest UType that subsumes this item type
    */
  def getUType(): UType = UType.fromTypeCode(origin.getNodeKind)

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
    if (nodeKind != origin.getNodeKind) {
      return      false
    }
    if (name.hasFingerprint && origin.hasFingerprint) {
      name.getFingerprint == origin.getFingerprint
    } else {
      name.hasURI(origin.getURI) && name.getLocalPart == origin.getLocalPart
    }
  }

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    val nameCodeArray: Array[Int] = tree.getNameCodeArray
    (nodeNr) =>
      {
        var k: Int = nodeKindArray(nodeNr) & 0x0f
        if (k == Type.WHITESPACE_TEXT) {
          k = Type.TEXT
        }
        if (k != origin.getNodeKind) {
          false
        } else if (origin.hasFingerprint) {
          (nameCodeArray(nodeNr) & 0xfffff) == origin.getFingerprint
        } else {
          Navigator.haveSameName(tree.getNode(nodeNr), origin)
        }
      }
  }

  override def test(node: NodeInfo): Boolean =
    node == origin ||
      (node.getNodeKind == origin.getNodeKind && Navigator.haveSameName(
        node,
        origin))

  def matches(qname: StructuredQName): Boolean =
    NameOfNode.makeName(origin).getStructuredQName == qname

  def getDefaultPriority: Double = 0.0

  override def getFingerprint(): Int =
    if (origin.hasFingerprint) {
      origin.getFingerprint
    } else {
      val pool: NamePool = origin.getConfiguration.getNamePool
      pool.allocateFingerprint(origin.getURI, origin.getLocalPart)
    }

  override def getPrimitiveType: Int = origin.getNodeKind

  /*@NotNull*/

  override def getRequiredNodeNames(): Optional[IntSet] =
    Optional.of(new IntSingletonSet(getFingerprint))

  def getNamespaceURI: String = origin.getURI

  def getLocalPart: String = origin.getLocalPart

  override def toString: String = origin.getNodeKind match {
    case Type.ELEMENT =>
      "element(" +
        NameOfNode.makeName(origin).getStructuredQName.getEQName +
        ")"
    case Type.ATTRIBUTE =>
      "attribute(" +
        NameOfNode.makeName(origin).getStructuredQName.getEQName +
        ")"
    case Type.PROCESSING_INSTRUCTION =>
      "processing-instruction(" + origin.getLocalPart + ')'
    case Type.NAMESPACE => "namespace-node(" + origin.getLocalPart + ')'
    case Type.COMMENT => "comment()"
    case Type.DOCUMENT => "document-node()"
    case Type.TEXT => "text()"
    case _ => "***"

  }

  override def hashCode(): Int =
    origin.getNodeKind << 20 ^ origin.getURI.hashCode ^ origin.getLocalPart.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: SameNameTest => test(other.origin)
    case _ => false

  }

  def getEquivalentNameTest: NameTest =
    new NameTest(origin.getNodeKind,
                 origin.getURI,
                 origin.getLocalPart,
                 origin.getConfiguration.getNamePool)

  /**
    * Export the QNameTest as a string for use in a SEF file (typically in a catch clause).
    *
    * @return a string representation of the QNameTest, suitable for use in export files. The format is
    * a sequence of alternatives separated by vertical bars, where each alternative is one of '*',
    * '*:localname', 'Q{uri}*', or 'Q{uri}local'.
    */
  override def exportQNameTest(): String = // Not applicable
    ""

  /**
    * Generate Javascript code to test if a name matches the test.
    *
    * @return JS code as a string. The generated code will be used
    * as the body of a JS function in which the argument name "q" is an
    * XdmQName object holding the name. The XdmQName object has properties
    * uri and local.
    * @param targetVersion the version of Saxon-JS being targeted
    */
  def generateJavaScriptNameTest(targetVersion: Int): String = // Not applicable
    "false"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * NodeTest is an interface that enables a test of whether a node has a particular
  * name and type. A SameNameTest matches a node that has the same node kind and name
  * as a supplied node.
  *
  * <p>Note: it's not safe to use this if the supplied node is mutable.</p>
  *
  * @author Michael H. Kay
  */
