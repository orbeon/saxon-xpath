package org.orbeon.saxon.expr.sort

import java.util.Arrays

import org.orbeon.saxon.expr.{ErrorIterator, LastPositionFinder, XPathContext}
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.om.{FocusTrackingIterator, Item, SequenceIterator, StandardNames}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.LookaheadIterator

class SortedIterator ()
  extends SequenceIterator
    with LastPositionFinder
    with LookaheadIterator {

   var base: SequenceIterator = _
   var sortKeyEvaluator: SortKeyEvaluator = _
   var comparators: Array[AtomicComparer] = _
   var values: Array[ObjectToBeSorted[Item]] = _
   var count: Int = -1
   var position: Int = 0
   var context: XPathContext = _

  private var hostLanguage: HostLanguage.HostLanguage = _

  def this(context: XPathContext,
           base: SequenceIterator,
           sortKeyEvaluator: SortKeyEvaluator,
           comparators: Array[AtomicComparer],
           createNewContext: Boolean) = {
    this()
    if (createNewContext) {
      this.context = context.newMinorContext()
      this.base = this.context.trackFocus(base)
      this.context.setTemporaryOutputState(StandardNames.XSL_SORT)
    } else {
      this.base = base
      this.context = context
    }
    this.sortKeyEvaluator = sortKeyEvaluator
    this.comparators = Array.ofDim[AtomicComparer](comparators.length)
    for (n <- comparators.indices)
      this.comparators(n) = comparators(n).provideContext(context)
  }

  def setHostLanguage(language: HostLanguage.HostLanguage): Unit = {
    hostLanguage = language
  }

  def hasNext: Boolean = {
    if (position < 0)
      return false
    if (count < 0) {
      base match {
        case iterator: LookaheadIterator =>
          iterator.hasNext
        case _ =>
          try {
            doSort()
            count > 0
          } catch {
            case err: XPathException =>
              count = -1
              base = new FocusTrackingIterator(new ErrorIterator(err))
              true
          }
      }
    } else {
      position < count
    }
  }

  def next(): Item = {
    if (position < 0)
      return null
    if (count < 0)
      doSort()
    if (position < count) {
      values({
        position += 1
        position - 1
      }).value
    } else {
      position = -1
      null
    }
  }

  def getLength: Int = {
    if (count < 0)
      doSort()
    count
  }

  override def getProperties: Set[Property] = Set(LAST_POSITION_FINDER)

   def buildArray(): Unit = {
    var allocated: Int = 0
    allocated =
      if (base.getProperties.contains(LAST_POSITION_FINDER))
        base.asInstanceOf[LastPositionFinder].getLength
      else
        100
    values = Array.ofDim[ObjectToBeSorted[Item]](allocated)
    count = 0
    var item: Item = null
    while ({
      item = base.next()
      item
    } != null) {
      if (count == allocated) {
        allocated *= 2
        val nk2: Array[ObjectToBeSorted[Item]] =
          Array.ofDim[ObjectToBeSorted[Item]](allocated)
        System.arraycopy(values, 0, nk2, 0, count)
        values = nk2
      }
      val itbs: ItemToBeSorted = new ItemToBeSorted(comparators.length)
      values(count) = itbs
      itbs.value = item
      for (n <- comparators.indices)
        itbs.sortKeyValues(n) = sortKeyEvaluator.evaluateSortKey(n, context)
      itbs.originalPosition = count
      count += 1
    }
    if (allocated * 2 < count || (allocated - count) > 2000) {
      val nk2: Array[ObjectToBeSorted[Item]] = Array.ofDim[ObjectToBeSorted[Item]](count)
      System.arraycopy(values, 0, nk2, 0, count)
      values = nk2
    }
  }

  private def doSort(): Unit = {

    buildArray()

    if (count < 2)
      return

    try
      Arrays.sort(values, 0, count, (a: ObjectToBeSorted[Item], b: ObjectToBeSorted[Item]) => {

        val compResultsIt =
          for ((comp, i) <- comparators.iterator.zipWithIndex) yield
            comp.compareAtomicValues(a.sortKeyValues(i), b.sortKeyValues(i))

        compResultsIt.find(_ != 0).getOrElse(a.originalPosition - b.originalPosition)
      })
    catch {
      case e: ClassCastException => {
        val err = new XPathException(
          "Non-comparable types found while sorting: " + e.getMessage)
        if (hostLanguage == HostLanguage.XSLT)
          err.setErrorCode("XTDE1030")
        else
          err.setErrorCode("XPTY0004")
        throw err
      }
    }
  }
}
