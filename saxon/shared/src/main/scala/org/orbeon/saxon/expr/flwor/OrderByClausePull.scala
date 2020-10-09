package org.orbeon.saxon.expr.flwor

import java.util.ArrayList

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.sort.{AtomicComparer, ItemToBeSorted, SortKeyDefinitionList}
import org.orbeon.saxon.trans.XPathException


class OrderByClausePull(private var base: TuplePull,
                        private var tupleExpr: TupleExpression,
                        orderBy: OrderByClause,
                        context: XPathContext)
  extends TuplePull {

  private val orderByClause: OrderByClause = orderBy

  private var currentPosition: Int = -1
  val suppliedComparers: Array[AtomicComparer] = orderBy.getAtomicComparers
  private val comparers: Array[AtomicComparer] =
    new Array[AtomicComparer](suppliedComparers.length)

  private val tupleArray: ArrayList[ItemToBeSorted] =
    new ArrayList[ItemToBeSorted](100)

  for (n <- comparers.indices) {
    this.comparers(n) = suppliedComparers(n).provideContext(context)
  }

  override def nextTuple(context: XPathContext): Boolean = {
    if (currentPosition < 0) {
      currentPosition = 0
      var position: Int = 0
      while (base.nextTuple(context)) {
        val tuple: Tuple = tupleExpr.evaluateItem(context)
        val sortKeyDefinitions: SortKeyDefinitionList =
          orderByClause.getSortKeyDefinitions
        val itbs: ItemToBeSorted = new ItemToBeSorted(sortKeyDefinitions.size)
        itbs.value = tuple
        for (i <- 0 until sortKeyDefinitions.size) {
          itbs.sortKeyValues(i) = orderByClause.evaluateSortKey(i, context)
        }
        position += 1
        itbs.originalPosition = position
        tupleArray.add(itbs)
      }

      // ORBEON: TODO sorting
//      implicit object ItemToBeSortedOrdering extends Ordering[ItemToBeSorted] {
//        def compare(a: ItemToBeSorted, b: ItemToBeSorted): Int = {
//          for (i <- comparers.indices) {
//            val comp = comparers(i).compareAtomicValues(a.sortKeyValues(i), b.sortKeyValues(i))
//            if (comp != 0) {
//              return comp
//            }
//          }
//          a.originalPosition - b.originalPosition
//        }
//      }
//
//      val newArray = tupleArray.toArray(new Array[ItemToBeSorted])
//      try
//        scala.util.Sorting.quickSort(newArray)
//      catch {
//        case e: ClassCastException =>
//          val err = new XPathException("Non-comparable types found while sorting: " + e.getMessage)
//          err.setErrorCode("XPTY0004")
//          throw err
//      }
    }
    if (currentPosition < tupleArray.size) {
      tupleExpr.setCurrentTuple(
        context,
        tupleArray
          .get({
            currentPosition += 1
            currentPosition - 1
          })
          .value
          .asInstanceOf[Tuple])
      true
    } else {
      false
    }
  }

  override def close(): Unit = {
    base.close()
  }

}
