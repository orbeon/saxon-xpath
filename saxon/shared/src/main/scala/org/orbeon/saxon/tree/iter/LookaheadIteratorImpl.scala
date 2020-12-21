package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trans.XPathException
import java.util.EnumSet

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


object LookaheadIteratorImpl {

  def makeLookaheadIterator(base: SequenceIterator): LookaheadIterator =
    if (base.getProperties.contains(Property.LOOKAHEAD)) {
      base.asInstanceOf[LookaheadIterator]
    } else {
      new LookaheadIteratorImpl(base)
    }

}

class LookaheadIteratorImpl private (private var base: SequenceIterator)
  extends LookaheadIterator {

  private var lNext: Item = base.next()

  def hasNext: Boolean = next != null

  def next(): Item = {
    val current: Item = lNext
    if (lNext != null) {
      lNext = base.next()
    }
    current
  }

  override def close(): Unit = {
    base.close()
  }

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)

}