package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.sort.AtomicComparer

import org.orbeon.saxon.expr.sort.ItemToBeSorted

import org.orbeon.saxon.expr.sort.SortKeyDefinitionList

import org.orbeon.saxon.trans.NoDynamicContextException

import org.orbeon.saxon.trans.XPathException

import java.util.ArrayList


class OrderByClausePull(private var base: TuplePull,
                        private var tupleExpr: TupleExpression,
                        orderBy: OrderByClause,
                        context: XPathContext)
  extends TuplePull {

  private var orderByClause: OrderByClause = orderBy

  private var currentPosition: Int = -1
  val suppliedComparers: Array[AtomicComparer] = orderBy.getAtomicComparers
  private var comparers: Array[AtomicComparer] =
    new Array[AtomicComparer](suppliedComparers.length)

  private var tupleArray: ArrayList[ItemToBeSorted] =
    new ArrayList[ItemToBeSorted](100)

  for (n <- 0 until comparers.length) {
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
      try tupleArray.sort((a, b) => {
        for (i <- 0 until comparers.length) {
          val comp: Int = comparers(i).compareAtomicValues(a.sortKeyValues(i),
            b.sortKeyValues(i))
          if (comp != 0) {
            comp
          }
        }
        a.originalPosition - b.originalPosition
      })
      catch {
        case e: ClassCastException => {
          val err = new XPathException(
            "Non-comparable types found while sorting: " + e.getMessage)
          err.setErrorCode("XPTY0004")
          throw err
        }

      }
    }
    if (currentPosition < tupleArray.size) {
      tupleExpr.setCurrentTuple(
        context,
        tupleArray
          .get({
            currentPosition += 1;
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