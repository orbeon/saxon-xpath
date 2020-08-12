package net.sf.saxon.expr.sort

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.om._

import net.sf.saxon.s9api.HostLanguage

import SequenceIterator.Property._

import java.util.Arrays


class SortedGroupIterator(context: XPathContext,
                          base: GroupIterator,
                          sortKeyEvaluator: SortKeyEvaluator,
                          comparators: Array[AtomicComparer])
  extends SortedIterator(context, base, sortKeyEvaluator, comparators, true)
    with GroupIterator {

  this.setHostLanguage(HostLanguage.XSLT)

 override  def buildArray(): Unit = {
    var allocated: Int = 0
    allocated =
      if (base.getProperties.contains(LAST_POSITION_FINDER))
        base.asInstanceOf[LastPositionFinder].getLength
      else 100
    values = Array.ofDim[ObjectToBeSorted[Item]](allocated)
    count = 0
    val c2: XPathContextMajor = context.newContext()
    c2.setCurrentIterator(base.asInstanceOf[FocusIterator])
    val groupIter: GroupIterator = base
      .asInstanceOf[FocusTrackingIterator]
      .getUnderlyingIterator
      .asInstanceOf[GroupIterator]
    c2.setCurrentGroupIterator(groupIter)
    var item: Item = null
    while ((item = base.next()) != null) {
      if (count == allocated) {
        allocated *= 2
        values = Arrays.copyOf(values, allocated)
      }
      val gtbs: GroupToBeSorted = new GroupToBeSorted(comparators.length)
      values(count) = gtbs
      gtbs.value = item
      for (n <- 0 until comparators.length) {
        gtbs.sortKeyValues(n) = sortKeyEvaluator.evaluateSortKey(n, c2)
      }
      gtbs.originalPosition = { count += 1; count - 1 }
      gtbs.currentGroupingKey = groupIter.getCurrentGroupingKey
      gtbs.currentGroup = new MemoSequence(groupIter.iterateCurrentGroup())
    }
  }

  def getCurrentGroupingKey(): AtomicSequence =
    values(position - 1).asInstanceOf[GroupToBeSorted].currentGroupingKey

  def iterateCurrentGroup(): SequenceIterator =
    values(position - 1).asInstanceOf[GroupToBeSorted].currentGroup.iterate()

}
