////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException




class PrependSequenceIterator(var start: Item, var base: SequenceIterator) extends SequenceIterator {

  /*@Nullable*/

  def next(): Item =
    if (start != null) {
      val temp: Item = start
      start = null
      temp
    } else {
      base.next()
    }

  override def close(): Unit = {
    base.close()
  }

}