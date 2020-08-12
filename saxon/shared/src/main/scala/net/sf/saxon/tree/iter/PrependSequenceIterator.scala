////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException




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