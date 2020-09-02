package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.FocusIterator
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.trans.XPathException
import java.util.EnumSet

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property

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
    this.lastPositionFinder = () => 1
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
