package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.AdjacentTextNodeMerger
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.tree.util.Orphan
import java.util.EnumSet

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


class AdjacentTextNodeMergingIterator(private var base: SequenceIterator)
  extends LookaheadIterator {

  private var lNext: Item = base.next()

  def hasNext: Boolean = next != null

  def next(): Item = {
    var current: Item = lNext
    if (current == null) {
      return null
    }
    lNext = base.next()
    if (AdjacentTextNodeMerger.isTextNode(current)) {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
      fsb.cat(current.getStringValueCS)
      while (AdjacentTextNodeMerger.isTextNode(lNext)) {
        fsb.cat(next.getStringValueCS)
        lNext = base.next()
      }
      if (fsb.isEmpty) {
        next()
      } else {
        val o: Orphan = new Orphan(
          current.asInstanceOf[NodeInfo].getConfiguration)
        o.setNodeKind(Type.TEXT)
        o.setStringValue(fsb)
        current = o
        current
      }
    } else {
      current
    }
  }

  override def close(): Unit = {
    base.close()
  }

  override def getProperties: Set[Property] = Set(Property.LOOKAHEAD)

}