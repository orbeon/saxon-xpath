////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import java.util.HashMap

import java.util.Iterator

import java.util.List




/**
  * Represents the tuple stream delivered by an "group by" clause. This groups the tuple stream supplied
  * as its input, and outputs a new set of tuples one per group of the input tuples. No groups are output
  * until all the groups have been read.
  */
class GroupByClausePull(private var base: TuplePull,
                        groupBy: GroupByClause,
                        context: XPathContext)
    extends TuplePull {

  private var groupByClause: GroupByClause = groupBy

  /*@Nullable*/

  var groupIterator: Iterator[List[GroupByClause.ObjectToBeGrouped]] = _

  private var comparers: Array[GenericAtomicComparer] =
    new Array[GenericAtomicComparer](groupBy.comparers.length)

  for (i <- 0 until comparers.length) {
    comparers(i) = groupBy.comparers(i).provideContext(context)
  }

  /**
    * Move on to the next tuple. Before returning, this method must set all the variables corresponding
    * to the "returned" tuple in the local stack frame associated with the context object
    *
    * @param context the dynamic evaluation context
    * @return true if another tuple has been generated; false if the tuple stream is exhausted. If the
    *         method returns false, the values of the local variables corresponding to this tuple stream
    *         are undefined.
    */
  override def nextTuple(context: XPathContext): Boolean = {
    if (groupIterator == null) {
      val groupingTupleExpr: TupleExpression =
        groupByClause.getGroupingTupleExpression
      val retainedTupleExpr: TupleExpression =
        groupByClause.getRetainedTupleExpression
      val map: HashMap[Any, List[GroupByClause.ObjectToBeGrouped]] =
        new HashMap[Any, List[GroupByClause.ObjectToBeGrouped]]()
      while (base.nextTuple(context)) {
        val otbg: GroupByClause.ObjectToBeGrouped =
          new GroupByClause.ObjectToBeGrouped()
        val groupingValues: Array[Sequence] =
          groupingTupleExpr.evaluateItem(context).getMembers
        GroupByClausePush.checkGroupingValues(groupingValues)
        otbg.groupingValues = new Tuple(groupingValues)
        otbg.retainedValues = retainedTupleExpr.evaluateItem(context)
        val key: AnyRef =
          groupByClause.getComparisonKey(otbg.groupingValues, comparers)
        val group: List[GroupByClause.ObjectToBeGrouped] = map.get(key)
        GroupByClausePush.addToGroup(key, otbg, group, map)
      }
      groupIterator = map.values.iterator
    }
// First do the grouping
// get an iterator over the groups
// First do the grouping
// get an iterator over the groups
    if (groupIterator.hasNext) {
      val group: List[GroupByClause.ObjectToBeGrouped] = groupIterator.next()
      groupByClause.processGroup(group, context)
      true
    } else {
      false
    }
  }

  override def close(): Unit = {
    base.close()
    groupIterator = null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
