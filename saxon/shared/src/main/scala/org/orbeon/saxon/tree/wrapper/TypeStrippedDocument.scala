////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A TypeStrippedDocument represents a view of a real Document in which all nodes are
 * untyped
 */
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.GenericTreeInfo
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.TreeInfo


class TypeStrippedDocument(doc: TreeInfo)
    extends GenericTreeInfo(doc.getConfiguration) {

  var underlyingTree: TreeInfo = doc

  this.setRootNode(wrap(doc.getRootNode))

  def wrap(node: NodeInfo): TypeStrippedNode = TypeStrippedNode.makeWrapper(node, this, null)

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


