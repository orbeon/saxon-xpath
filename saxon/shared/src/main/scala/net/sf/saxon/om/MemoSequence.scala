////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import java.util

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.MemoSequence.State
import net.sf.saxon.om.MemoSequence.State.State
import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.{ArrayIterator, EmptyIterator, GroundedIterator, SingletonIterator}
import net.sf.saxon.value.{EmptySequence, SequenceExtent}


object MemoSequence {

  object State extends Enumeration {

    val UNREAD: State = new State()

    val MAYBE_MORE: State = new State()

    val ALL_READ: State = new State()

    val BUSY: State = new State()

    val EMPTY: State = new State()

    class State

  }

}

class MemoSequence extends Sequence {

  import State._

  var inputIterator: SequenceIterator = _

  def this(iterator: SequenceIterator) {
    this()
    inputIterator = iterator
  }


  var reservoir: Array[Item] = null

  var used: Int = _

  var state: State = State.UNREAD

  def head(): Item = iterate().next()

  def iterate(): SequenceIterator = synchronized {
    state match {
      case UNREAD =>
        state = BUSY
        if (inputIterator.isInstanceOf[EmptyIterator]) {
          state = EMPTY
          return inputIterator
        }
        reservoir = Array.ofDim[Item](50)
        used = 0
        state = MAYBE_MORE
        new ProgressiveIterator()
      case MAYBE_MORE => new ProgressiveIterator()
      case ALL_READ =>
        used match {
          case 0 =>
            state = EMPTY
            EmptyIterator.emptyIterator()
          case 1 =>
            assert(reservoir != null)
            SingletonIterator.makeIterator(reservoir(0))
          case _ => new ArrayIterator(reservoir, 0, used)

        }
      case BUSY =>
        // Can also happen if variable evaluation is attempted in a debugger, hence the cautious message
        var de: XPathException = new XPathException(
          "Attempt to access a variable while it is being evaluated")
        de.setErrorCode("XTDE0640")
        //de.setXPathContext(context);
        throw de
      // recursive entry: can happen if there is a circularity involving variable and function definitions
      case EMPTY => EmptyIterator.emptyIterator()
      case _ => throw new IllegalStateException("Unknown iterator state")

    }
  }

  def itemAt(n: Int): Item = synchronized {
    if (n < 0) {
      return null
    }
    if (reservoir != null && n < used) {
      reservoir(n)
    }
    if (state == ALL_READ || state == EMPTY) {
      return null
    }
    if (state == UNREAD) {
      val item: Item = inputIterator.next()
      if (item == null) {
        state = EMPTY
        return null
      } else {
        state = MAYBE_MORE
        reservoir = Array.ofDim[Item](50)
        append(item)
        if (n == 0) {
          return item
        }
      }
    }
    // We have read some items from the input sequence but not enough. Read as many more as are needed.
    var diff: Int = n - used + 1
    while ( {
      diff -= 1;
      diff + 1
    } > 0) {
      val i: Item = inputIterator.next()
      if (i == null) {
        state = ALL_READ
        condense()
        return null
      }
      append(i)
      state = MAYBE_MORE
    }
    //noinspection ConstantConditions
    reservoir(n)
  }

  private def append(item: Item): Unit = {
    assert(reservoir != null)
    if (used >= reservoir.length) {
      reservoir = Array.copyOf(reservoir, used * 2)
    }
    reservoir({
      used += 1;
      used - 1
    }) = item
  }

  private def condense(): Unit = {
    if (reservoir != null && reservoir.length - used > 30) {
      reservoir = Array.copyOf(reservoir, used)
    }
  }

  class ProgressiveIterator
    extends SequenceIterator
      with LastPositionFinder
      with GroundedIterator {

    // zero-based position in the reservoir of the
    var position: Int = -1

    def getMemoSequence(): MemoSequence = MemoSequence.this

    /*@Nullable*/

    override def next(): Item = this.synchronized {
      // synchronized for the case where a multi-threaded xsl:for-each is reading the variable
      if (position == -2) {
        // means we've already returned null once, keep doing so if called again.
        return null
      }
      position = position + 1
      if (position < used) {
        assert(reservoir != null)
        return reservoir(position).asInstanceOf[Item]
      } else if (state == ALL_READ) {
        // someone else has read the input to completion in the meantime
        position = -2
        return null
      } else {
        assert(inputIterator != null)
        val i: Item = inputIterator.next()
        if (i == null) {
          state = ALL_READ
          condense()
          position = -2
          return null
        }
        position = used
        append(i)
        state = MAYBE_MORE
        return i.asInstanceOf[Item]
      }
    }

    override def close(): Unit = {}

    def getLength(): Int =
      if (state == ALL_READ) {
        used
      } else if (state == EMPTY) {
        0
      } else {
        // save the current position
        val savePos: Int = position
        // fill the reservoir
        while (next() != null) {}
        // reset the current position
        position = savePos
        // return the total number of items
        used
      }

    /*@Nullable*/

    override def materialize(): GroundedValue =
      if (state == ALL_READ) {
        makeExtent()
      } else if (state == EMPTY) {
        EmptySequence.getInstance
      } else {
        // save the current position
        val savePos: Int = position
        // fill the reservoir
        while (next() != null) {}
        // reset the current position
        position = savePos
        // return all the items
        makeExtent()
      }

    private def makeExtent(): GroundedValue =
      if (used == reservoir.length) {
        if (used == 0) {
          EmptySequence.getInstance
        } else if (used == 1) {
          reservoir(0)
        } else {
          new SequenceExtent(reservoir)
        }
      } else {
        SequenceExtent.makeSequenceExtent {
          Seq(reservoir.toIndexedSeq: _*).slice(0, used).toList.asInstanceOf[util.List[Item]]
        }
      }

    override def getResidue(): GroundedValue =
      if (state == EMPTY || position >= used || position == -2) {
        EmptySequence.getInstance
      } else if (state == ALL_READ) {
        SequenceExtent.makeSequenceExtent(reservoir.slice(position + 1, used).toList.asInstanceOf[util.List[Item]])
      } else {
        // save the current position
        val savePos: Int = position
        // fill the reservoir
        while (next() != null) {}
        // reset the current position
        position = savePos
        // return all the items
        SequenceExtent.makeSequenceExtent(
          Seq(reservoir.toIndexedSeq: _*).slice(position + 1, used).toList.asInstanceOf[util.List[Item]])
      }

    override def getProperties(): Set[Property] = Set(Property.GROUNDED, Property.LAST_POSITION_FINDER)

  }

}