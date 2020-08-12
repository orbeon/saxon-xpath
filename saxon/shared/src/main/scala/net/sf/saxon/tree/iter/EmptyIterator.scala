package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om._
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.EmptySequence
import java.util.EnumSet

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property

object EmptyIterator {

  private val theInstance: EmptyIterator = new EmptyIterator()

  def getInstance(): EmptyIterator = theInstance

  def emptyIterator(): EmptyIterator = theInstance

  def ofNodes(): AxisIterator = OfNodes.THE_INSTANCE

  def ofAtomic[T <: AtomicValue](): AtomicIterator[T] =
    OfAtomic.THE_INSTANCE.asInstanceOf[AtomicIterator[T]]

  object OfNodes {

    val THE_INSTANCE: OfNodes = new OfNodes()

  }

  class OfNodes extends EmptyIterator with AxisIterator {

    override def next(): NodeInfo = null

  }

  object OfAtomic {

    val THE_INSTANCE: OfAtomic[Nothing] = new OfAtomic()

  }

  class OfAtomic[T <: AtomicValue] extends AtomicIterator[T] {

    def next(): T = null.asInstanceOf[T]

  }

}

class EmptyIterator  ()
  extends SequenceIterator
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator
    with UnfailingIterator
    with AtomizedValueIterator {

  def nextAtomizedValue(): AtomicSequence = null

  def next(): Item = null

  def getLength(): Int = 0

  def getReverseIterator(): EmptyIterator = this

  override def getProperties(): Set[Property] =
    Set(Property.LOOKAHEAD,
      Property.GROUNDED,
      Property.LAST_POSITION_FINDER,
      Property.ATOMIZING)

  override def materialize(): GroundedValue = EmptySequence.getInstance

  override def getResidue(): GroundedValue = EmptySequence.getInstance

  def hasNext(): Boolean = false

}