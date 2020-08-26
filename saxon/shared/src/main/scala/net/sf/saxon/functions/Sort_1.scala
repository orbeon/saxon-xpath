package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.AtomicSortComparer

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.ma.arrays.ArraySort

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List

import Sort_1._

import scala.jdk.CollectionConverters._

object Sort_1 {

  class ItemToBeSorted {

    var value: Item = _

    var sortKey: GroundedValue = _

    var originalPosition: Int = _

  }

}

class Sort_1 extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val inputList: List[ItemToBeSorted] = getItemsToBeSorted(arguments(0))
    val collation: StringCollator = context.getConfiguration.getCollation(
      getRetainedStaticContext.getDefaultCollationName)
    doSort(inputList, collation, context)
  }

   def getItemsToBeSorted(input: Sequence): List[ItemToBeSorted] = {
    val inputList: List[ItemToBeSorted] = new ArrayList[ItemToBeSorted]()
    var i: Int = 0
    val iterator: SequenceIterator = input.iterate()
    var item: Item = null
    while (({
      item = iterator.next()
      item
    }) != null) {
      val member: ItemToBeSorted = new ItemToBeSorted()
      member.value = item
      i += 1
      member.originalPosition = i
      member.sortKey = item.atomize()
      inputList.add(member)
    }
    inputList
  }

   def doSort(inputList: List[ItemToBeSorted],
                       collation: StringCollator,
                       context: XPathContext): Sequence = {
    val atomicComparer: AtomicComparer = AtomicSortComparer.makeSortComparer(
      collation,
      StandardNames.XS_ANY_ATOMIC_TYPE,
      context)
    try inputList.sort((a, b) => {
      val result: Int =
        ArraySort.compareSortKeys(a.sortKey, b.sortKey, atomicComparer)
      if (result == 0) {
        a.originalPosition - b.originalPosition
      } else {
        result
      }
    })
    catch {
      case e: ClassCastException => {
        val err: XPathException = new XPathException(
          "Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XPTY0004")
        throw err
      }

    }
    val outputList: List[Item] = new ArrayList[Item](inputList.size)
    for (member <- inputList.asScala) {
      outputList.add(member.value)
    }
    new SequenceExtent(outputList)
  }

}
