package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.AdjacentTextNodeMerger
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{Item, NodeInfo, SequenceIterator}
import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property
import org.orbeon.saxon.tree.util.{FastStringBuffer, Orphan}


class AdjacentTextNodeMergingIterator(private var base: SequenceIterator)
  extends LookaheadIterator {

  private var lNext: Item = base.next()

  def hasNext: Boolean = next != null

  def next(): Item = {

    var current = lNext
    if (current == null)
      return null

    lNext = base.next()
    if (AdjacentTextNodeMerger.isTextNode(current)) {
      val fsb = new FastStringBuffer(FastStringBuffer.C256)
      fsb.cat(current.getStringValueCS)
      while (AdjacentTextNodeMerger.isTextNode(lNext)) {
        fsb.cat(next().getStringValueCS)
        lNext = base.next()
      }
      if (fsb.isEmptySB) {
        next()
      } else {
        val o = new Orphan(current.asInstanceOf[NodeInfo].getConfiguration)
        o.setNodeKind(Type.TEXT)
        o.setStringValue(fsb)
        current = o
        current
      }
    } else {
      current
    }
  }

  override def close(): Unit =
    base.close()

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)
}