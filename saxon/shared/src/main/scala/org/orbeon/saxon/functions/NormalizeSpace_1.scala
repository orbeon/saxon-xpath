package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

import org.orbeon.saxon.value.Whitespace

import NormalizeSpace_1._

object NormalizeSpace_1 {

  def normalizeSpace(sv: StringValue): StringValue = {
    if (sv == null) {
      return StringValue.EMPTY_STRING
    }
    StringValue.makeStringValue(
      Whitespace.collapseWhitespace(sv.getStringValueCS))
  }

}

class NormalizeSpace_1 extends ScalarSystemFunction {

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = One.string("")

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    normalizeSpace(arg.asInstanceOf[StringValue])

  override def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def effectiveBooleanValue(c: XPathContext): Boolean = {
        val sv: AtomicValue = getArg(0).evaluateItem(c).asInstanceOf[AtomicValue]
        if (sv == null) return false
        val cs: CharSequence = sv.getStringValueCS
        !Whitespace.isWhite(cs)
      }
    }

  override def getCompilerName(): String = "NormalizeSpaceCompiler"

}
