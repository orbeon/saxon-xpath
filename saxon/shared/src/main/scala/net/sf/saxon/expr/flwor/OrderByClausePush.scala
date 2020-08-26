package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.ItemToBeSorted

import net.sf.saxon.expr.sort.SortKeyDefinitionList

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

//remove if not needed
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
        val err: XPathException = new XPathException(
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
