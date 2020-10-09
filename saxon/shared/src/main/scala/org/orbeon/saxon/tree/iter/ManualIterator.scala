package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om.{FocusIterator, GroundedValue, Item}
import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property
import org.orbeon.saxon.trans.XPathException

class ManualIterator
  extends FocusIterator
    with UnfailingIterator
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator {

  private var item: Item = null

  var position: Int = 0

  private var lastPositionFinder: LastPositionFinder = _

  def this(value: Item, position: Int) = {
    this()
    this.item = value
    this.position = position
  }

  def this(value: Item) = {
    this()
    this.item = value
    this.position = 1
    this.lastPositionFinder = new LastPositionFinder {
      def getLength: Int = 1
    }
  }

  def setContextItem(value: Item): Unit = {
    this.item = value
  }

  def setLastPositionFinder(finder: LastPositionFinder): Unit = {
    this.lastPositionFinder = finder
  }

  def incrementPosition(): Unit = {
    position += 1
  }

  def setPosition(position: Int): Unit = {
    this.position = position
  }

  def hasNext: Boolean =
    try position != getLength
    catch {
      case e: XPathException => false

    }

  def next(): Item = null

  def current: Item = item

  def getLength: Int =
    if (lastPositionFinder == null) {
      throw new XPathException(
        "Saxon streaming restriction: last() cannot be used when consuming a sequence of streamed nodes, even if the items being processed are grounded")
    } else {
      lastPositionFinder.getLength
    }

  def getReverseIterator: ManualIterator = new ManualIterator(item)

  override def materialize(): GroundedValue = item

  override def getResidue: GroundedValue = materialize()

  override def getProperties: Set[Property] =
    Set(Property.LOOKAHEAD,
      Property.GROUNDED,
      Property.LAST_POSITION_FINDER)

}
