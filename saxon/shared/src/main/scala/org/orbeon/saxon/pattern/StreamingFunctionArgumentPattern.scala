////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import StreamingFunctionArgumentPattern._


object StreamingFunctionArgumentPattern {

  private var THE_INSTANCE: StreamingFunctionArgumentPattern =
    new StreamingFunctionArgumentPattern()

  def getInstance: StreamingFunctionArgumentPattern = THE_INSTANCE

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
  override def getUType: UType = UType.ANY_NODE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = this

  def matches(item: Item, context: XPathContext): Boolean = {
    val arg: Sequence = context.getStackFrame.getStackFrameValues(0)
    val iter: SequenceIterator = arg.iterate()
    var j: Item = null
    while (({
      j = iter.next()
      j
    }) != null) if (j == item) {
      return true
    }
    false
  }

  override def getItemType: ItemType = AnyNodeTest.getInstance

  /*@NotNull*/

  override def toString: String = "$$"

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
