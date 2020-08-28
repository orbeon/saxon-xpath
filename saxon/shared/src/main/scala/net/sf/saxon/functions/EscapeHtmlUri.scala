package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.serialize.HTMLURIEscaper

import ScalarSystemFunction._

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

class EscapeHtmlUri extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue = {
    val s: CharSequence = arg.getStringValueCS
    StringValue.makeStringValue(
      HTMLURIEscaper
        .escapeURL(s, normalize = false, getRetainedStaticContext.getConfiguration))
  }

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

}
