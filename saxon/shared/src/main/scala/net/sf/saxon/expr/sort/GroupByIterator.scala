package net.sf.saxon.expr.sort

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om._

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.tree.iter.LookaheadIterator

import net.sf.saxon.value.AtomicValue

import java.util.ArrayList

import java.util.EnumSet

import java.util.HashMap

import java.util.List

import SequenceIterator.Property._
import scala.util.control.Breaks._

class GroupByIterator
  extends GroupIterator
    with LastPositionFinder
    with LookaheadIterator {
  private var population: SequenceIterator = _
  var keyExpression: Expression = _
  private var keyContext: XPathContext = _
  private var collator: StringCollator = _
  var composite: Boolean = _
  private var position: Int = 0

  var groups: List[List[Item]] = new ArrayList(40)

  var groupKeys: List[AtomicSequence] = new ArrayList(40)

  def this(population: SequenceIterator, keyExpression: Expression, keyContext: XPathContext, collator: StringCollator, composite: Boolean) {
    this()
    this.population = population
    this.keyExpression = keyExpression
    this.keyContext = keyContext
    this.collator = collator
    this.composite = composite
  }

  if (composite) {
    buildIndexedGroupsComposite()
  } else {
    buildIndexedGroups()
  }

  private def buildIndexedGroups(): Unit = {
    val index: HashMap[AtomicMatchKey, List[Item]] =
      new HashMap[AtomicMatchKey, List[Item]](40)
    val c2: XPathContext = keyContext.newMinorContext()
    val focus: FocusIterator = c2.trackFocus(population)
    val implicitTimezone: Int = c2.getImplicitTimezone
    var item: Item = null
    while (({
      item = focus.next()
      item
    }) != null) {
      val keys: SequenceIterator = keyExpression.iterate(c2)
      var firstKey: Boolean = true
      breakable {
        while (true) {
          val key: AtomicValue = keys.next().asInstanceOf[AtomicValue]
          if (key == null) {
            break()
          }
          var comparisonKey: AtomicMatchKey = null
          comparisonKey =
            if (key.isNaN) AtomicMatchKey.NaN_MATCH_KEY
            else key.getXPathComparable(ordered = false, collator, implicitTimezone)
          val g: List[Item] = index.get(comparisonKey)
          if (g == null) {
            val newGroup: List[Item] = new ArrayList[Item](20)
            newGroup.add(item)
            groups.add(newGroup)
            groupKeys.add(key)
            index.put(comparisonKey, newGroup)
          } else {
            if (firstKey) {
              g.add(item)
            } else {
              if (g.get(g.size - 1) != item) {
                g.add(item)
              }
            }
          }
          firstKey = false
        }
      }
    }
  }

  private def buildIndexedGroupsComposite(): Unit = {
    val index: HashMap[List[AtomicMatchKey], List[Item]] =
      new HashMap[List[AtomicMatchKey], List[Item]](40)
    val c2: XPathContext = keyContext.newMinorContext()
    val focus: FocusIterator = c2.trackFocus(population)
    val implicitTimezone: Int = c2.getImplicitTimezone
    var item: Item = null
    while (({
      item = focus.next()
      item
    }) != null) {
      val keys: SequenceIterator = keyExpression.iterate(c2)
      val ckList: List[AtomicMatchKey] = new ArrayList[AtomicMatchKey]()
      val compositeKey: List[AtomicValue] = new ArrayList[AtomicValue]()
      breakable {
        while (true) {
          val key: AtomicValue = keys.next().asInstanceOf[AtomicValue]
          if (key == null) {
            break()
          }
          compositeKey.add(key)
          var comparisonKey: AtomicMatchKey = null
          comparisonKey =
            if (key.isNaN) AtomicMatchKey.NaN_MATCH_KEY
            else key.getXPathComparable(ordered = false, collator, implicitTimezone)
          ckList.add(comparisonKey)
        }
      }
      val g: List[Item] = index.get(ckList)
      if (g == null) {
        val newGroup: List[Item] = new ArrayList[Item](20)
        newGroup.add(item)
        groups.add(newGroup)
        groupKeys.add(new AtomicArray(compositeKey))
        index.put(ckList, newGroup)
      } else {
        g.add(item)
      }
    }
  }

  def getCurrentGroupingKey(): AtomicSequence = synchronized {
    val `val`: AtomicSequence = groupKeys.get(position - 1)
    if (`val` == null) {
      EmptyAtomicSequence.getInstance
    } else {
      `val`
    }
  }

  def iterateCurrentGroup(): SequenceIterator =
    new ListIterator(groups.get(position - 1))

  def getCurrentGroup(): List[_] = groups.get(position - 1)

  def hasNext(): Boolean = position < groups.size

  def next(): Item =
    if (position >= 0 && position < groups.size) {
      position += 1
      current()
    } else {
      position = -1
      null
    }

  private def current(): Item = {
    if (position < 1) {
      return null
    }
    groups.get(position - 1).get(0)
  }

  override def getProperties(): Set[Property] = Set(LOOKAHEAD, LAST_POSITION_FINDER)

  def getLength(): Int = groups.size

}
