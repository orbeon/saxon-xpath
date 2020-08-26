////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import StreamingFunctionArgumentPattern._


object StreamingFunctionArgumentPattern {

  private var THE_INSTANCE: StreamingFunctionArgumentPattern =
    new StreamingFunctionArgumentPattern()

  def getInstance(): StreamingFunctionArgumentPattern = THE_INSTANCE

}

/**
 * This is a special pattern that matches the node supplied as the first argument of a call to
 * a streamable stylesheet function; it corresponds to the
 * pattern match="$arg" where $arg is the first argument of the function.
 */
class StreamingFunctionArgumentPattern() extends Pattern {

  /**
   * Get a UType indicating which kinds of items this Pattern can match.
   *
   * @return a UType indicating all the primitive types of item that the pattern can match.
   */
  override def getUType(): UType = UType.ANY_NODE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = this

  def matches(item: Item, context: XPathContext): Boolean = {
    val arg: Sequence = context.getStackFrame().getStackFrameValues()(0)
    val iter: SequenceIterator = arg.iterate()
    var j: Item = null
    while (({
      j = iter.next()
      j
    }) != null) if (j == item) {
      true
    }
    false
  }

  override def getItemType(): ItemType = AnyNodeTest.getInstance

  /*@NotNull*/

  override def toString(): String = "$$"

  override def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.streamingArg")
    presenter.endElement()
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Pattern = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
