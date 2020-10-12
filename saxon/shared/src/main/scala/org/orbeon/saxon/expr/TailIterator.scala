package org.orbeon.saxon.expr

import org.orbeon.saxon.om.{GroundedValue, Item, SequenceIterator}
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.tree.iter.{ArrayIterator, EmptyIterator, LookaheadIterator}

object TailIterator {

  def make[T <: Item](base: SequenceIterator, start: Int): SequenceIterator =
    if (start <= 1) {
      base
    } else base match {
      case value: ArrayIterator[_] =>
        value.makeSliceIterator(start, java.lang.Integer.MAX_VALUE)
      case _ => if (base.getProperties.contains(SequenceIterator.Property.GROUNDED)) {
        val value: GroundedValue = base.materialize
        if (start > value.getLength) {
          EmptyIterator.emptyIterator
        } else {
          new ValueTailIterator(value, start - 1)
        }
      } else {
        for (_ <- 0 until start - 1) {
          val b: Item = base.next()
          if (b == null) {
            return EmptyIterator.emptyIterator
          }
        }
        new TailIterator(base, start)
      }
    }

}

class TailIterator private(private var base: SequenceIterator,
                           private var start: Int)
  extends SequenceIterator
    with LastPositionFinder
    with LookaheadIterator {

  def next(): Item = base.next()

  def hasNext: Boolean = base.asInstanceOf[LookaheadIterator].hasNext

  def getLength: Int = {
    val bl: Int = base.asInstanceOf[LastPositionFinder].getLength - start + 1
    if (bl > 0) bl else 0
  }

  override def close(): Unit =
    base.close()

  override def getProperties: Set[Property] = base.getProperties.intersect(Set(LAST_POSITION_FINDER, LOOKAHEAD))
}
