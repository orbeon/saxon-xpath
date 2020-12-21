package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.serialize.HTMLURIEscaper

import ScalarSystemFunction._

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

class EscapeHtmlUri extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue = {
    val s: CharSequence = arg.getStringValueCS
    StringValue.makeStringValue(
      HTMLURIEscaper
        .escapeURL(s, normalize = false, getRetainedStaticContext.getConfiguration))
  }

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

}
