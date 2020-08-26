////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.Atomizer
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.sort.GenericAtomicComparer
import net.sf.saxon.om.{Item, Sequence, SequenceTool}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.EmptySequence
import java.util.ArrayList
import java.util.HashMap
import java.util.List

import GroupByClausePush._

import scala.jdk.CollectionConverters._


object GroupByClausePush {

  def addToGroup(
                  key: AnyRef,
                  objectToBeGrouped: GroupByClause.ObjectToBeGrouped,
                  group: List[GroupByClause.ObjectToBeGrouped],
                  map: HashMap[Any, List[GroupByClause.ObjectToBeGrouped]]): Unit = {
    if (group != null) {
      group.add(objectToBeGrouped)
      map.put(key, group)
    } else {
      val list: List[GroupByClause.ObjectToBeGrouped] =
        new ArrayList[GroupByClause.ObjectToBeGrouped]()
      list.add(objectToBeGrouped)
      map.put(key, list)
    }
  }

  def checkGroupingValues(groupingValues: Array[Sequence]): Unit = {
    for (i <- 0 until groupingValues.length) {
      var v: Sequence = groupingValues(i)
      if (!(v.isInstanceOf[EmptySequence[_ <: Item]] || v.isInstanceOf[AtomicValue])) {
        v = Atomizer.getAtomizingIterator(v.iterate(), oneToOne = false).materialize()
        if (SequenceTool.getLength(v) > 1) {
          throw new XPathException(
            "Grouping key value cannot be a sequence of more than one item",
            "XPTY0004")
        }
        groupingValues(i) = v
      }
    }
  }

}

/**
 * Represents the tuple stream delivered by an "group by" clause. This groups the tuple stream supplied
 * as its input, and outputs a new set of tuples one per group of the input tuples. No groups are output
 * until all the groups have been read.
 */
class GroupByClausePush(outputter: Outputter,
                        private var destination: TuplePush,
                        groupBy: GroupByClause,
                        private var context: XPathContext)
  extends TuplePush(outputter) {

  private var groupByClause: GroupByClause = groupBy

  private var map: HashMap[Any, List[GroupByClause.ObjectToBeGrouped]] =
    new HashMap()

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
   */
  override def processTuple(context: XPathContext): Unit = {
    val groupingTupleExpr: TupleExpression =
      groupByClause.getGroupingTupleExpression
    val retainedTupleExpr: TupleExpression =
      groupByClause.getRetainedTupleExpression
    val otbg: GroupByClause.ObjectToBeGrouped =
      new GroupByClause.ObjectToBeGrouped()
    val groupingValues: Array[Sequence] =
      groupingTupleExpr.evaluateItem(context).getMembers
    checkGroupingValues(groupingValues)
    otbg.groupingValues = new Tuple(groupingValues)
    otbg.retainedValues = retainedTupleExpr.evaluateItem(context)
    val key: AnyRef =
      groupByClause.getComparisonKey(otbg.groupingValues, comparers)
    val group: List[GroupByClause.ObjectToBeGrouped] = map.get(key)
    addToGroup(key, otbg, group, map)
  }

  // Allocate the incoming tuple to a group
  // Allocate the incoming tuple to a group

  override def close(): Unit = {
    for (group <- map.values.asScala) {
      groupByClause.processGroup(group, context)
      destination.processTuple(context)
    }
    destination.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
