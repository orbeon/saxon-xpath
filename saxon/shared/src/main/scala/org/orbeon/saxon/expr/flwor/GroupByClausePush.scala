////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.{Atomizer, XPathContext}
import org.orbeon.saxon.expr.flwor.GroupByClausePush._
import org.orbeon.saxon.expr.sort.GenericAtomicComparer
import org.orbeon.saxon.om.{Sequence, SequenceTool}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, EmptySequence}

import java.util.{ArrayList, HashMap, List}
import scala.jdk.CollectionConverters._


object GroupByClausePush {

  def addToGroup(
    key              : AnyRef,
    objectToBeGrouped: GroupByClause.ObjectToBeGrouped,
    group            : List[GroupByClause.ObjectToBeGrouped],
    map              : HashMap[Any, List[GroupByClause.ObjectToBeGrouped]]
  ): Unit =
    if (group != null) {
      group.add(objectToBeGrouped)
      map.put(key, group)
    } else {
      val list = new ArrayList[GroupByClause.ObjectToBeGrouped]()
      list.add(objectToBeGrouped)
      map.put(key, list)
    }

  def checkGroupingValues(groupingValues: Array[Sequence]): Unit =
    for (i <- groupingValues.indices) {
      var v = groupingValues(i)
      if (! (v.isInstanceOf[EmptySequence.type] || v.isInstanceOf[AtomicValue])) {
        v = Atomizer.getAtomizingIterator(v.iterate(), oneToOne = false).materialize
        if (SequenceTool.getLength(v) > 1) {
          throw new XPathException(
            "Grouping key value cannot be a sequence of more than one item",
            "XPTY0004"
          )
        }
        groupingValues(i) = v
      }
    }
}

/**
 * Represents the tuple stream delivered by an "group by" clause. This groups the tuple stream supplied
 * as its input, and outputs a new set of tuples one per group of the input tuples. No groups are output
 * until all the groups have been read.
 */
class GroupByClausePush(
  outputter              : Outputter,
  private var destination: TuplePush,
  groupBy                : GroupByClause,
  private var context    : XPathContext
) extends TuplePush(outputter) {

  private val groupByClause: GroupByClause = groupBy
  private val map: HashMap[Any, List[GroupByClause.ObjectToBeGrouped]] = new HashMap

  private val comparers: Array[GenericAtomicComparer] =
    new Array[GenericAtomicComparer](groupBy.comparers.length)

  for (i <- comparers.indices)
    comparers(i) = groupBy.comparers(i).provideContext(context)

  /**
   * Move on to the next tuple. Before returning, this method must set all the variables corresponding
   * to the "returned" tuple in the local stack frame associated with the context object
   *
   * @param context the dynamic evaluation context
   */
  def processTuple(context: XPathContext): Unit = {

    val groupingTupleExpr = groupByClause.getGroupingTupleExpression
    val retainedTupleExpr = groupByClause.getRetainedTupleExpression
    val otbg              = new GroupByClause.ObjectToBeGrouped()
    val groupingValues    = groupingTupleExpr.evaluateItem(context).getMembers

    checkGroupingValues(groupingValues)

    otbg.groupingValues = new Tuple(groupingValues)
    otbg.retainedValues = retainedTupleExpr.evaluateItem(context)

    val key   = groupByClause.getComparisonKey(otbg.groupingValues, comparers)
    val group = map.get(key)

    addToGroup(key, otbg, group, map)
  }

  // Allocate the incoming tuple to a group
  override def close(): Unit = {
    for (group <- map.values.asScala) {
      groupByClause.processGroup(group, context)
      destination.processTuple(context)
    }
    destination.close()
  }
}
