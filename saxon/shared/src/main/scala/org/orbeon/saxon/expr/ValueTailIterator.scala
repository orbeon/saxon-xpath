////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import SequenceIterator.Property._

import org.orbeon.saxon.tree.iter.GroundedIterator

import org.orbeon.saxon.tree.iter.LookaheadIterator

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

  def hasNext: Boolean = baseValue.itemAt(start + pos) != null

  override def materialize: GroundedValue =
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

