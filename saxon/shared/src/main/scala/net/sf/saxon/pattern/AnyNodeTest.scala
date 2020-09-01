////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import java.util.function.IntPredicate

import net.sf.saxon.model.{SchemaType, Type, UType}
import net.sf.saxon.om.{NodeInfo, NodeName, StructuredQName}
import net.sf.saxon.tree.tiny.NodeVectorTree


/**
  * NodeTest is an interface that enables a test of whether a node has a particular
  * name and type. An AnyNodeTest matches any node.
  *
  * @author Michael H. Kay
  */
object AnyNodeTest {

  private var THE_INSTANCE: AnyNodeTest = new AnyNodeTest()

  def getInstance: AnyNodeTest = THE_INSTANCE

}

class AnyNodeTest private () extends NodeTest with QNameTest {

  /**
    * Get the corresponding {@link net.sf.saxon.model.UType}. A UType is a union of primitive item
    * types.
    *
    * @return the smallest UType that subsumes this item type
    */
  def getUType(): UType = UType.ANY_NODE

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
    nodeKind != Type.PARENT_POINTER

  override def getMatcher(tree: NodeVectorTree): IntPredicate = {
    val nodeKindArray: Array[Byte] = tree.getNodeKindArray
    (nodeNr) =>
      nodeKindArray(nodeNr) != Type.PARENT_POINTER
  }

  override def test(node: NodeInfo): Boolean = true

  def matches(qname: StructuredQName): Boolean = true

  def getDefaultPriority: Double = -0.5

  /*@NotNull*/

  override def toString: String = "node()"

  /**
    * Export the QNameTest as a string for use in a SEF file (typically in a catch clause).
    *
    * @return a string representation of the QNameTest, suitable for use in export files. The format is
    * a sequence of alternatives separated by vertical bars, where each alternative is one of '*',
    * '*:localname', 'Q{uri}*', or 'Q{uri}local'.
    */
  override def exportQNameTest(): String = "*"

  /**
    * Generate Javascript code to test if a name matches the test.
    *
    * @return JS code as a string. The generated code will be used
    * as the body of a JS function in which the argument name "q" is an
    * XdmQName object holding the name. The XdmQName object has properties
    * uri and local.
    * @param targetVersion The version of Saxon-JS being targeted
    */
  def generateJavaScriptNameTest(targetVersion: Int): String = "true"

}
