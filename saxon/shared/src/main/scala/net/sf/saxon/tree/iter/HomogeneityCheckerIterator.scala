////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.expr.sort.DocumentOrderIterator

import net.sf.saxon.expr.sort.GlobalOrderComparer

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List


class HomogeneityCheckerIterator(var base: SequenceIterator, var loc: Location)
  extends SequenceIterator {

  var state: Int = 0

  override def close(): Unit = {
    base.close()
  }

  /*@NotNull*/

  private def reportMixedItems(): XPathException = {
    val err: XPathException = new XPathException(
      "Cannot mix nodes and atomic values in the result of a path expression")
    err.setErrorCode("XPTY0018")
    err.setLocator(loc)
    err
  }

  /*@Nullable*/

  def next(): Item = {
    var item: Item = base.next()
    if (item == null) {
      return null
    }
    //first item in iterator
    if (state == 0) {
      if (item.isInstanceOf[NodeInfo]) {
        val nodes: List[Item] = new ArrayList[Item](50)
        nodes.add(item)
        while (({
          item = base.next()
          item
        }) != null) if (!(item
          .isInstanceOf[NodeInfo])) {
          throw reportMixedItems()
        } else {
          nodes.add(item)
        }
        base = new DocumentOrderIterator(new ListIterator(nodes),
          GlobalOrderComparer.getInstance)
        // first item is a node
        state = 1
        base.next()
      } else {
        // first item is an atomic value or function item
        state = -1
      }
    } else if (state == -1 && item.isInstanceOf[NodeInfo]) {
      throw reportMixedItems()
    }
    item
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An iterator that returns the same items as its base iterator, checking to see that they are either
 * all nodes, or all non-nodes; if they are all nodes, it delivers them in document order.
 */
