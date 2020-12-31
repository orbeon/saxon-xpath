package org.orbeon.saxon.functions

import java.util.{ArrayList, List}

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.sort.{AtomicComparer, AtomicSortComparer}
import org.orbeon.saxon.functions.Sort_1._
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.ma.arrays.ArraySort
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.SequenceExtent

//import scala.collection.compat._
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
    val inputList = getItemsToBeSorted(arguments(0))
    val collation: StringCollator = context.getConfiguration.getCollation(
      getRetainedStaticContext.getDefaultCollationName)
    doSort(inputList, collation, context)
  }

   def getItemsToBeSorted(input: Sequence): List[ItemToBeSorted] = {
    val inputList = new ArrayList[ItemToBeSorted]()
    var i = 0
    val iterator = input.iterate()
    var item: Item = null
    while ({
      item = iterator.next()
      item
    } != null) {
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
    try {
      ArraySort.sortList(inputList)((a, b) => {
        val result = ArraySort.compareSortKeys(a.sortKey, b.sortKey, atomicComparer)
        if (result == 0)
          a.originalPosition - b.originalPosition
        else
          result
      })
    } catch {
      case e: ClassCastException =>
        val err = new XPathException(
          "Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XPTY0004")
        throw err
    }
    val outputList = new ArrayList[Item](inputList.size)
    for (member <- inputList.asScala)
      outputList.add(member.value)
    new SequenceExtent(outputList)
  }
}
