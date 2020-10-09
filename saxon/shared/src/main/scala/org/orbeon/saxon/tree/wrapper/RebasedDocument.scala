////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.{GenericTreeInfo, NodeInfo, TreeInfo}

import scala.beans.BeanProperty


/**
  * A <tt>RebasedDocument</tt> represents a view of a real Document in which all nodes are mapped to a different
  * base URI and/or system ID using supplied mapping functions.
  *
  * <p>It is possible to map either base URIs or system IDs or both.</p>
  *
  * <p>All properties of the nodes other than the base URI and system ID are unchanged.</p>
  *
  * <p>The user-supplied functions supplied to compute the base URI and system ID will be applied
  * to the underlying node in the "real" document. It is of course possible to supply a function that
  * ignores the supplied input.
  *
  * @since 9.9.0.2
  */
class RebasedDocument(
    doc: TreeInfo,
    @BeanProperty var baseUriMapper: NodeInfo => String,
    @BeanProperty var systemIdMapper: NodeInfo => String)
    extends GenericTreeInfo(doc.getConfiguration) {

  @BeanProperty
  var underlyingTree: TreeInfo = doc

  this.setRootNode(wrap(doc.getRootNode))

  def wrap(node: NodeInfo): RebasedNode =
    RebasedNode.makeWrapper(node, this, null)

  /**
    * Ask whether the document contains any nodes whose type annotation is anything other than
    * UNTYPED. (This will be true if and only if the underlying document is untyped).
    *
    * <p>Note: in XSD 1.1 it is possible to define assertions such that the validity of a node
    * depends on its base URI. This class assumes that no-one would be quite so perverse. The validity
    * and type annotation of a virtual node are therefore the same as the validity and type annotation
    * of its underlying node.</p>
    *
    * @return true if the document contains elements whose type is other than UNTYPED
    */
  override def isTyped: Boolean = underlyingTree.isTyped

  /*@Nullable*/

  override def selectID(id: String, getParent: Boolean): NodeInfo = {
    val n: NodeInfo = underlyingTree.selectID(id, getParent = false)
    if (n == null) {
      null
    } else {
      wrap(n)
    }
  }

}

