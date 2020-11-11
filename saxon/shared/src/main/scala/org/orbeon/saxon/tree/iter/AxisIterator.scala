////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.iter

import java.util.function.Consumer

import org.orbeon.saxon.om.NodeInfo


/**
  * A SequenceIterator is used to iterate over a sequence of items. An AxisIterator
  * is a SequenceIterator that throws no exceptions, and that always returns
  * nodes. The nodes should all be in the same document (though there are
  * some cases, such as PrependIterator, where this is the responsibility of the
  * user of the class and is not enforced.)
  */
trait AxisIterator extends UnfailingIterator {

  def next(): NodeInfo

  def asIterator: Iterator[NodeInfo] = new Iterator[NodeInfo] {

    var nextVar: NodeInfo = AxisIterator.this.next()

    def hasNext: Boolean = nextVar != null

    def next(): NodeInfo = {
      val curr = nextVar
      nextVar = AxisIterator.this.next()
      curr
    }
  }

  // ORBEON: Only used by `SnapshotFn` from XSLT.
  def forEachNode(consumer: Consumer[_ >: NodeInfo]): Unit = {
    var item: NodeInfo = null
    while ({
      item = next()
      item
    } != null)
      consumer.accept(item)
  }
}

