package org.orbeon.saxon.expr.flwor

import java.util.ArrayList

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.sort.{AtomicComparer, ItemToBeSorted, SortKeyDefinitionList}
import org.orbeon.saxon.ma.arrays.ArraySort
import org.orbeon.saxon.trans.XPathException

//remove if not needed
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class OrderByClausePush(outputter: Outputter,
                        private var destination: TuplePush,
                        private var tupleExpr: TupleExpression,
                        orderBy: OrderByClause,
                        private var context: XPathContext)
  extends TuplePush(outputter) {

  val suppliedComparers: Array[AtomicComparer] = orderBy.getAtomicComparers

  private var orderByClause: OrderByClause = orderBy

  private var comparers: Array[AtomicComparer] =
    new Array[AtomicComparer](suppliedComparers.length)

  private var position: Int = 0

  private var tupleArray: ArrayList[ItemToBeSorted] = new ArrayList(100)

  for (n <- 0 until comparers.length) {
    this.comparers(n) = suppliedComparers(n).provideContext(context)
  }

  override def processTuple(context: XPathContext): Unit = {
    val tuple: Tuple = tupleExpr.evaluateItem(context)
    val sortKeyDefinitions: SortKeyDefinitionList =
      orderByClause.getSortKeyDefinitions
    val itbs: ItemToBeSorted = new ItemToBeSorted(sortKeyDefinitions.size)
    itbs.value = tuple
    for (i <- 0 until sortKeyDefinitions.size) {
      itbs.sortKeyValues(i) = orderByClause.evaluateSortKey(i, context)
    }
    position+=1
    itbs.originalPosition = position
      tupleArray.add(itbs)
  }

  override def close(): Unit = {
    try {
      ArraySort.sortList(tupleArray)((a, b) => {

        val compResultsIt =
          for ((comp, i) <- comparers.iterator.zipWithIndex) yield
            comp.compareAtomicValues(a.sortKeyValues(i), b.sortKeyValues(i))

        compResultsIt.find(_ != 0).getOrElse(a.originalPosition - b.originalPosition)
      })
    } catch {
      case e: ClassCastException => {
        val err = new XPathException(
          "Non-comparable types found while sorting: " + e.getMessage)
        err.setErrorCode("XPTY0004")
        throw err
      }

    }
    for (itbs <- tupleArray.asScala) {
      tupleExpr.setCurrentTuple(context, itbs.value.asInstanceOf[Tuple])
      destination.processTuple(context)
    }
    destination.close()
  }

}
