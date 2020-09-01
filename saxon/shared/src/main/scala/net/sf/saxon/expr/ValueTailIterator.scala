////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import SequenceIterator.Property._

import net.sf.saxon.tree.iter.GroundedIterator

import net.sf.saxon.tree.iter.LookaheadIterator

import java.util.EnumSet


class ValueTailIterator(private var baseValue: GroundedValue,
                        private var start: Int)
  extends SequenceIterator
    with GroundedIterator
    with LookaheadIterator {

  private var pos: Int = 0

  def next(): Item = baseValue.itemAt(start + {
    pos += 1; pos - 1
  })

  def hasNext(): Boolean = baseValue.itemAt(start + pos) != null

  override def materialize(): GroundedValue =
    if (start == 0) {
      baseValue
    } else {
      baseValue.subsequence(start, java.lang.Integer.MAX_VALUE)
    }

  override def getResidue: GroundedValue =
    if (start == 0 && pos == 0) {
      baseValue
    } else {
      baseValue.subsequence(start + pos, java.lang.Integer.MAX_VALUE)
    }

  override def getProperties: Set[Property] = Set(LOOKAHEAD, GROUNDED)

}

