////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.{GroundedValue, Item, SequenceIterator}
import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property
import net.sf.saxon.tree.iter.{ArrayIterator, LookaheadIterator}

import scala.util.control.Breaks._

object SubsequenceIterator {

  def make[T <: Item](base: SequenceIterator,
                      min: Int,
                      max: Int): SequenceIterator =
    base match {
      case value: ArrayIterator[_] =>
        value.makeSliceIterator(min, max)
      case _ => if (max == java.lang.Integer.MAX_VALUE) {
        TailIterator.make(base, min)
      } else if (base.getProperties.contains(SequenceIterator.Property.GROUNDED) &&
        min > 4) {
        var value: GroundedValue = base.materialize()
        value = value.subsequence(min - 1, max - min + 1)
        value.iterate()
      } else {
        new SubsequenceIterator(base, min, max)
      }
    }

}

class SubsequenceIterator private(private var base: SequenceIterator,
                                  private var min: Int,
                                  private var max: Int)
  extends SequenceIterator
    with LastPositionFinder
    with LookaheadIterator {

  private var basePosition: Int = 0

  private var nextItem: Item = null

  if (min < 1) {
    min = 1
  }
  if (max < min) {
    nextItem = null
  }

  var i: Int = 1
  breakable {
    while (i <= min) {
      i += 1
      nextItem = base.next()
      basePosition += 1
      if (nextItem == null) {
        break()
      }
    }
  }

  def hasNext: Boolean = nextItem != null

  def next(): Item = {
    if (nextItem == null) {
      return null
    }
    val current: Item = nextItem
    if (basePosition < max) {
      nextItem = base.next()
      basePosition += 1
    } else {
      nextItem = null
      base.close()
    }
    current
  }

  override def close(): Unit = {
    base.close()
  }

  override def getProperties: Set[Property] = {
    val intersectSet: Set[Property] = base.getProperties.intersect(Set(Property.LAST_POSITION_FINDER))
    intersectSet.union(Set(Property.LOOKAHEAD))
  }

  def getLength: Int = {
    val lastBase: Int = base.asInstanceOf[LastPositionFinder].getLength
    val z: Int = Math.min(lastBase, max)
    Math.max(z - min + 1, 0)
  }

}
