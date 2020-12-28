////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.expr.SingletonIntersectExpression
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.tree.iter.UnfailingIterator

import java.{util => ju}


/**
  * A value that exists in memory and that can be directly addressed
  * @since 9.5.  Generified in 9.9.
  */
trait GroundedValue extends Sequence {

  def iterate(): UnfailingIterator
  def itemAt(n: Int): Item
  def head: Item
  def subsequence(start: Int, length: Int): GroundedValue
  def getLength: Int
  def getStringValue: String
  def getStringValueCS: CharSequence

  def effectiveBooleanValue: Boolean = ExpressionTool.effectiveBooleanValue(iterate())
  def reduce(): GroundedValue = this

  override def materialize: GroundedValue = this

  def toShortString: String = Err.depictSequence(this).toString

  def asIterable(): java.lang.Iterable[_ <: Item] = () => {
    val base = iterate()
    new ju.Iterator[Item]() {
      var pending: Item = null

      def hasNext: Boolean = {
        pending = base.next()
        pending != null
      }

      def next(): Item = pending
    }
  }

  def containsNode(sought: NodeInfo): Boolean =
    SingletonIntersectExpression.containsNode(iterate(), sought)

  def concatenate(others: GroundedValue*): GroundedValue = {
    val c = new ju.ArrayList[GroundedValue]()
    c.add(this)
    ju.Collections.addAll(c, others: _*)
    new Chain(c)
  }
}
