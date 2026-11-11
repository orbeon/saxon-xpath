////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import java.util.{Comparator, List, TreeSet}
import org.orbeon.saxon.expr.UnionIterator.Intake
import org.orbeon.saxon.expr.sort.ItemOrderComparer
import org.orbeon.saxon.om.{NodeInfo, SequenceIterator}
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.tree.iter.LookaheadIterator

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object UnionIterator {
  private class Intake(var iter: SequenceIterator, var nextNode: NodeInfo)
}

/**
 * A multi-way union delivering the sorted results obtained from a number
 * of sorted input iterators
 */
class UnionIterator(
  inputs   : List[SequenceIterator],
  comparer : ItemOrderComparer
) extends SequenceIterator with LookaheadIterator {

  private val comp: Comparator[Intake] = (a, b) => comparer.compare(a.nextNode, b.nextNode)

  private val intakes: TreeSet[Intake] = new TreeSet[Intake](comp)

  for (seq <- inputs.asScala) {
    var next = seq.next().asInstanceOf[NodeInfo]
    breakable {
      while (next != null) {
        val added = intakes.add(new Intake(seq, next))
        if (added) {
          break()
        } else {
          next = seq.next().asInstanceOf[NodeInfo]
        }
      }
    }
  }

  override def hasNext: Boolean = ! intakes.isEmpty

  override def next(): NodeInfo = {
    val nextIntake = intakes.pollFirst()
    if (nextIntake != null) {
      val iter = nextIntake.iter
      var nextNode = iter.next().asInstanceOf[NodeInfo]
      breakable {
        while (nextNode != null) {
          var added = false
          if (! nextNode.isSameNodeInfo(nextIntake.nextNode)) {
            val replacement = new Intake(iter, nextNode)
            added = intakes.add(replacement)
          }
          if (added) {
            break()
          } else {
            nextNode = iter.next().asInstanceOf[NodeInfo]
          }
        }
      }
      return nextIntake.nextNode
    }
    null
  }

  override def close(): Unit =
    for (intake <- intakes.asScala)
      intake.iter.close()

  override def getProperties: Set[Property] =
    Set(LOOKAHEAD)
}
