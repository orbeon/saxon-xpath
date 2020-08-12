package net.sf.saxon.expr

import net.sf.saxon.om.EnumSetTool
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.tree.iter.ArrayIterator
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import java.util.EnumSet

import net.sf.saxon.om.SequenceIterator.Property._

object TailIterator {

  def make[T <: Item](base: SequenceIterator, start: Int): SequenceIterator =
    if (start <= 1) {
      base
    } else if (base.isInstanceOf[ArrayIterator[Item]]) {
      base
        .asInstanceOf[ArrayIterator[Item]]
        .makeSliceIterator(start, java.lang.Integer.MAX_VALUE)
    } else if (base.getProperties.contains(SequenceIterator.Property.GROUNDED)) {
      val value: GroundedValue = base.materialize()
      if (start > value.getLength) {
        EmptyIterator.emptyIterator()
      } else {
        new ValueTailIterator(value, start - 1)
      }
    } else {
      for (i <- 0 until start - 1) {
        val b: Item = base.next()
        if (b == null) {
          EmptyIterator.emptyIterator()
        }
      }
      new TailIterator(base, start)
    }

}

class TailIterator private(private var base: SequenceIterator,
                           private var start: Int)
  extends SequenceIterator
    with LastPositionFinder
    with LookaheadIterator {

  def next(): Item = base.next()

  def hasNext(): Boolean = base.asInstanceOf[LookaheadIterator].hasNext

  def getLength(): Int = {
    val bl: Int = base.asInstanceOf[LastPositionFinder].getLength - start +
      1
    if (bl > 0) bl else 0
  }

  override def close(): Unit = {
    base.close()
  }

  override def getProperties(): Set[Property] = base.getProperties.intersect(Set(LAST_POSITION_FINDER, LOOKAHEAD))

}
