////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.Type
import org.orbeon.saxon.tree.util.FastStringBuffer


/**
  * TinyParentNodeImpl is an implementation of a non-leaf node (specifically, an Element node
  * or a Document node)
  *
  * @author Michael H. Kay
  */
object TinyParentNodeImpl {

  def getStringValueCS(tree: TinyTree, nodeNr: Int): CharSequence = {

    val level = tree.depth(nodeNr)

    // note, we can't rely on the value being contiguously stored because of whitespace
    // nodes: the data for these may still be present.

    var next = nodeNr + 1

    // we optimize two special cases: firstly, where the node has no children, and secondly,
    // where it has a single text node as a child.

    if (tree.nodeKind(nodeNr) == Type.TEXTUAL_ELEMENT) {
      TinyTextImpl.getStringValue(tree, nodeNr)
    } else if (next < tree.numberOfNodes) {
      // bug 4445
      if (tree.depth(next) <= level) {
        return ""
      } else if (tree.nodeKind(next) == Type.TEXT &&
                 (next + 1 >= tree.numberOfNodes || tree.depth(next + 1) <= level)) {
        return TinyTextImpl.getStringValue(tree, next)
      }
    }

    // now handle the general case

    var sb: FastStringBuffer = null

    while (next < tree.numberOfNodes && tree.depth(next) > level) {
      val kind = tree.nodeKind(next)
      if (kind == Type.TEXT || kind == Type.TEXTUAL_ELEMENT) {
        if (sb == null)
          sb = new FastStringBuffer(FastStringBuffer.C256)
        sb.cat(TinyTextImpl.getStringValue(tree, next))
      } else if (kind == Type.WHITESPACE_TEXT) {
        if (sb == null)
          sb = new FastStringBuffer(FastStringBuffer.C256)
        WhitespaceTextImpl.appendStringValue(tree, next, sb)
      }
      next += 1
    }
    if (sb == null)
      ""
    else
      sb.condense()
  }
}

abstract class TinyParentNodeImpl extends TinyNodeImpl {

  override def hasChildNodes: Boolean =
    nodeNr + 1 < tree.numberOfNodes && tree.depth(nodeNr + 1) > tree.depth(nodeNr)

  def getStringValue: String =
    TinyParentNodeImpl.getStringValueCS(tree, nodeNr).toString

  override def getStringValueCS: CharSequence =
    TinyParentNodeImpl.getStringValueCS(tree, nodeNr)
}

