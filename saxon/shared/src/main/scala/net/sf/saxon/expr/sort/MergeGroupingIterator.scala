package net.sf.saxon.expr.sort

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.model.Type
import net.sf.saxon.om.AtomicArray
import net.sf.saxon.om.AtomicSequence
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.ListIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.ExternalObject
import net.sf.saxon.value.ObjectValue
import java.util._

import SequenceIterator.Property._

import scala.collection.immutable

class MergeGroupingIterator(p1: SequenceIterator,
                            comp: ItemOrderComparer,
                            lpf: LastPositionFinder)
  extends GroupIterator
    with LookaheadIterator
    with LastPositionFinder {

  private var baseItr: SequenceIterator = p1

  private var currenti: ObjectValue[ItemWithMergeKeys] = null

  private var nextItm: ObjectValue[ItemWithMergeKeys] =
    p1.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]

  private var currentMembers: List[Item] = _

  private var currentSourceMembers: Map[String, List[Item]] = _

  private var comparer: ItemOrderComparer = comp

  private var position: Int = 0

  var compositeMergeKey: List[AtomicValue] = _

  private var lastPositionFinder: LastPositionFinder = lpf

  if (nextItm != null) {
    compositeMergeKey = nextItm
      .asInstanceOf[ObjectValue[_]]
      .getObject
      .asInstanceOf[ItemWithMergeKeys]
      .sortKeyValues
  }

  private def advance(): Unit = {
    currentMembers = new ArrayList(20)
    currentSourceMembers = new HashMap(20)
    var currentItem: Item = currenti.getObject.baseItem
    var source: String = currenti.getObject.sourceName
    currentMembers.add(currentItem)
    if (source != null) {
      val list: List[Item] = new ArrayList[Item]()
      list.add(currentItem)
      currentSourceMembers.put(source, list)
    }
    while (true) {
      val nextCandidate: ObjectValue[ItemWithMergeKeys] =
        baseItr.next().asInstanceOf[ObjectValue[ItemWithMergeKeys]]
      if (nextCandidate == null) {
        nextItm = null
        return
      }
      try {
        val c: Int = comparer.compare(currenti, nextCandidate)
        if (c == 0) {
          currentItem = nextCandidate.getObject.baseItem
          source = nextCandidate.getObject.sourceName
          currentMembers.add(currentItem)
          if (source != null) {
            val list: List[Item] = currentSourceMembers.computeIfAbsent(
              source,
              (k) => new ArrayList())
            list.add(currentItem)
          }
        } else if (c > 0) {
          val keys: List[AtomicValue] = nextCandidate.getObject.sortKeyValues
          throw new XPathException(
            "Merge input for source " + source +
              " is not ordered according to merge key, detected at key value: " +
              Arrays.toString(keys.toArray()),
            "XTDE2220")
        } else {
          nextItm = nextCandidate
          return
        }
      } catch {
        case e: ClassCastException => {
          val err: XPathException = new XPathException(
            "Merge key values are of non-comparable types (" + Type
              .displayTypeName(currentItem) +
              " and " +
              Type.displayTypeName(nextCandidate.getObject.baseItem) +
              ')',
            "XTTE2230"
          )
          err.setIsTypeError(true)
          throw err
        }

      }
    }
  }

  def hasNext(): Boolean = nextItm != null

  def next(): Item = {
    if (nextItm == null) {
      currenti = null
      position = -1
      null
    }
    currenti = nextItm
    position += 1
    compositeMergeKey = nextItm
      .asInstanceOf[ExternalObject[_]]
      .getObject
      .asInstanceOf[ItemWithMergeKeys]
      .sortKeyValues
    advance()
    currenti.getObject.baseItem
  }

  override def close(): Unit = {
    baseItr.close()
  }

  override def getLength(): Int = lastPositionFinder.getLength

  override def getProperties(): immutable.Set[Property] = immutable.Set(LOOKAHEAD, LAST_POSITION_FINDER)

  def getCurrentGroupingKey(): AtomicSequence =
    new AtomicArray(compositeMergeKey)

  def iterateCurrentGroup(): SequenceIterator =
    new ListIterator(currentMembers)

  def iterateCurrentGroup(source: String): SequenceIterator = {
    val sourceMembers: List[Item] = currentSourceMembers.get(source)
    if (sourceMembers == null) {
      EmptyIterator.emptyIterator()
    } else {
      new ListIterator(sourceMembers)
    }
  }

}
