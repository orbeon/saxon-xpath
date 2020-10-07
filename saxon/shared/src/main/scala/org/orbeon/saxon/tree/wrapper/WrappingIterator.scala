////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.tree.iter.AxisIterator




class WrappingIterator(var base: AxisIterator,
                       function: WrappingFunction,
                       var parent: VirtualNode)
    extends AxisIterator {

  /*@Nullable*/

  var current: NodeInfo = _

  var atomizing: Boolean = false

  var wrappingFunction: WrappingFunction = function

  /*@Nullable*/

  def next(): NodeInfo = {
    val n: Item = base.next()
    current =
      if (n.isInstanceOf[NodeInfo] && !atomizing)
        wrappingFunction.makeWrapper(n.asInstanceOf[NodeInfo], parent)
      else n.asInstanceOf[NodeInfo]
    current
  }

  /*@Nullable*/

//  def current: NodeInfo = current

  override def close(): Unit = {
    base.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A WrappingIterator delivers wrappers for the nodes delivered
  * by its underlying iterator. It is used when no whitespace stripping
  * is actually needed, e.g. for the attribute axis. But we still need to
  * create wrappers, so that further iteration remains in the virtual layer
  * rather than switching to the real nodes.
  */
