package net.sf.saxon.expr.sort

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.Count

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.tree.iter.LookaheadIterator

import net.sf.saxon.value.AtomicValue

import java.util.ArrayList

import java.util.EnumSet

import java.util.List

import scala.jdk.CollectionConverters._

import SequenceIterator.Property._
import scala.util.control.Breaks._

class GroupAdjacentIterator(private var select: Expression,
                            private var keyExpression: Expression,
                            private var baseContext: XPathContext,
                            private var collator: StringCollator,
                            private var composite: Boolean)
  extends GroupIterator
    with LastPositionFinder
    with LookaheadIterator {

  private var population: FocusIterator =
    runningContext.trackFocus(select.iterate(baseContext))

  private var runningContext: XPathContext = baseContext.newMinorContext()

  private var currentComparisonKey: List[AtomicMatchKey] = _

  private var currentKey: AtomicSequence = _

  private var currentMembers: List[Item] = _

  private var nextComparisonKey: List[AtomicMatchKey] = _

  private var nextKey: List[AtomicValue] = null

  private var nextItem: Item = population.next()

  private var current: Item = null

  private var position: Int = 0

  if (nextItem != null) {
    nextKey = getKey(runningContext)
    nextComparisonKey = getComparisonKey(nextKey, baseContext)
  }

  override def getLength(): Int = {
    val another: GroupAdjacentIterator = new GroupAdjacentIterator(
      select,
      keyExpression,
      baseContext,
      collator,
      composite)
    Count.steppingCount(another)
  }

  private def getKey(context: XPathContext): List[AtomicValue] = {
    val key: List[AtomicValue] = new ArrayList[AtomicValue]()
    val iter: SequenceIterator = keyExpression.iterate(context)
    breakable {
      while (true) {
        val `val`: AtomicValue = iter.next().asInstanceOf[AtomicValue]
        if (`val` == null) {
          break
        }
        key.add(`val`)
      }
    }
    key
  }

  private def getComparisonKey(
                                key: List[AtomicValue],
                                keyContext: XPathContext): List[AtomicMatchKey] = {
    val ckey: List[AtomicMatchKey] = new ArrayList[AtomicMatchKey](key.size)
    for (aKey <- key.asScala) {
      var comparisonKey: AtomicMatchKey = null
      comparisonKey =
        if (aKey.isNaN) AtomicMatchKey.NaN_MATCH_KEY
        else
          aKey.getXPathComparable(false,
            collator,
            keyContext.getImplicitTimezone)
      ckey.add(comparisonKey)
    }
    ckey
  }

  private def advance(): Unit = {
    currentMembers = new ArrayList(20)
    currentMembers.add(current)
    breakable {
      while (true) {
        val nextCandidate: Item = population.next()
        if (nextCandidate == null) {
          break
        }
        val newKey: List[AtomicValue] = getKey(runningContext)
        val newComparisonKey: List[AtomicMatchKey] =
          getComparisonKey(newKey, baseContext)
        try if (currentComparisonKey == newComparisonKey) {
          currentMembers.add(nextCandidate)
        } else {
          nextItem = nextCandidate
          nextComparisonKey = newComparisonKey
          nextKey = newKey
          return
        } catch {
          case e: ClassCastException => {
            val message: String =
              "Grouping key values are of non-comparable types"
            val err: XPathException = new XPathException(message)
            err.setIsTypeError(true)
            err.setXPathContext(runningContext)
            throw err
          }

        }
      }
    }
    nextItem = null
    nextKey = null
  }

  def getCurrentGroupingKey(): AtomicSequence = currentKey

  def iterateCurrentGroup(): SequenceIterator =
    new ListIterator(currentMembers)

  def hasNext(): Boolean = nextItem != null

  def next(): Item = {
    if (nextItem == null) {
      current = null
      position = -1
      return null
    }
    current = nextItem
    currentKey = if (nextKey.size == 1) nextKey.get(0) else new AtomicArray(nextKey)
    currentComparisonKey = nextComparisonKey
    position += 1
    advance()
    current
  }

  override def close(): Unit = {
    population.close()
  }

  override def getProperties(): Set[Property] = Set(LOOKAHEAD, LAST_POSITION_FINDER)

}
