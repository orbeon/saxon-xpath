////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.expr.SingletonIntersectExpression

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.trans.Err

import net.sf.saxon.tree.iter.UnfailingIterator

import java.util.ArrayList

import java.util.Collections

import java.util.Iterator

import java.util.List




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

  def effectiveBooleanValue(): Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  def getStringValue: String

  def getStringValueCS: CharSequence

  def reduce(): GroundedValue = this

  override def materialize(): GroundedValue = this

  def toShortString: String = Err.depictSequence(this).toString

  def asIterable(): java.lang.Iterable[_ <: Item] = () => {
    val base: UnfailingIterator = iterate()
    new Iterator[Item]() {
      var pending: Item = null

      override def hasNext(): Boolean = {
        pending = base.next()
        pending != null
      }

      override def next(): Item = pending
    }
  }

  def containsNode(sought: NodeInfo): Boolean =
    SingletonIntersectExpression.containsNode(iterate(), sought)

  def concatenate(others: GroundedValue*): GroundedValue = {
    val c: List[GroundedValue] = new ArrayList[GroundedValue]()
    c.add(this)
    Collections.addAll(c, others:_*)
    new Chain(c)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
