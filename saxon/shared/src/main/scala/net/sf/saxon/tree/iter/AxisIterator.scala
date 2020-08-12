////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.om.NodeInfo

import java.util.function.Consumer




trait AxisIterator extends UnfailingIterator {

  def next(): NodeInfo

  def asIterator(): Iterator[NodeInfo] = new Iterator[NodeInfo]() {
    var nextVar: NodeInfo = AxisIterator.this.next()

    override def hasNext(): Boolean = nextVar != null

    override def next(): NodeInfo = {
      val curr: NodeInfo = nextVar
      nextVar = AxisIterator.this.next()
      curr
    }
  }

  def forEachNode(consumer: Consumer[_ >: NodeInfo]): Unit = {
    var item: NodeInfo = null
    while ((item = next()) != null) consumer.accept(item)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SequenceIterator is used to iterate over a sequence of items. An AxisIterator
  * is a SequenceIterator that throws no exceptions, and that always returns
  * nodes. The nodes should all be in the same document (though there are
  * some cases, such as PrependIterator, where this is the responsibility of the
  * user of the class and is not enforced.)
  */
