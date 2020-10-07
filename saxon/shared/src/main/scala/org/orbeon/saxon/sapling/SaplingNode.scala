////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.sapling

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.s9api.SchemaValidator

import org.orbeon.saxon.s9api.Serializer

import org.orbeon.saxon.s9api.XdmNode

import org.orbeon.saxon.trans.XPathException

import javax.xml.transform.Source




abstract class SaplingNode {

  def getNodeKind: Int

  def sendTo(receiver: Receiver): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a sapling node. Sapling nodes represent transient nodes used in the course
  * of tree construction; they are intended to have a very lightweight implementation, making construction
  * as simple as possible, and they do not support any complex navigation or query interfaces. Typically
  * when construction of a sapling tree is complete it will be converted to a regular {@link NodeInfo}
  * or {@link XdmNode} for further processing, or it can be sent to a destination such as a {@link Serializer}
  * or {@link SchemaValidator}.
  *
  * <p>The {@code SaplingDocument} node at the root of a constructed tree implements the JAXP {@link Source} interface
  * which is widely accepted by a variety of interfaces in Saxon: this means that it can readily be used as
  * input to a query, a transformation, a document builder, or a schema validator.</p>
  *
  * <p>Sapling nodes are immutable objects; operations such as {@link SaplingElement#withChild} create a
  * new object and leave the original unchanged. This eliminates the need (found in other models such as
  * the DOM) to make copies of nodes when attaching them to a tree.</p>
  *
  * <p>The data model supported by Sapling nodes is close to the XDM model, but with some limitations:</p>
  *
  * <ul>
  *     <li>All nodes are untyped (elements are of type xs:untyped, attributes are xs:untypedAtomic).</li>
  *     <li>The ID, IDREF, and NILLED properties cannot be set.</li>
  *     <li>Some constraints on the model (for example, rules on the lexical form of names) are not enforced
  *     by the construction API.</li>
  *     <li>Elements cannot have an independent base URI property, except to the extent that this can be
  *     defined using <code>xml:base</code> attributes; only document nodes have their own base URI</li>
  *     <li>Zero length text nodes and adjacent text node siblings are permitted (they will be merged or
  *     eliminated when the sapling tree is converted to a regular node tree)</li>
  * </ul>
  *
  * <p>Attributes are not represented as nodes in their own right, but rather as properties of element nodes.</p>
  *
  * <p>The internal representation of document and element nodes include a list of the node's children; but there
  * are no links back from a node to its parent. This enables a node to be attached as a child to many different
  * parents. This does not create problems, because nodes are immutable, and there are no operations that depend
  * on concepts of node identity or document order.</p>
  *
  * <p>As in XDM, an element node contains a set of in-scope namespaces. There is no requirement that the in-scope
  * namespaces of an element should be in any way related to those of its child or parent elements. Operations
  * that construct a regular tree from a sapling tree will introduce namespace inheritance, whereby namespaces defined
  * on a parent element are inherited by its children. Namespace bindings for the (prefix, URI) pairs used in the
  * names of elements and attributes are added to an element automatically; additional namespace bindings may be
  * added by explicit API request.</p>
  */
