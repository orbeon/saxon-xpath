package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.One

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.Whitespace

import NormalizeSpace_1._

object NormalizeSpace_1 {

  def normalizeSpace(sv: StringValue): StringValue = {
    if (sv == null) {
      StringValue.EMPTY_STRING
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
