package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StringLiteral

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.regex.RegularExpression

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import Replace._

object Replace {

  def checkReplacement(rep: CharSequence): String = {
    var itr = 0
    for (i <- 0 until rep.length) {
      itr = i
      val c: Char = rep.charAt(itr)
      if (c == '$') {
        if (itr + 1 < rep.length) {
          itr += 1
          val next: Char = rep.charAt(itr)
          if (next < '0' || next > '9') {
            return "Invalid replacement string in replace(): $ sign must be followed by digit 0-9"
          }
        } else {
          return "Invalid replacement string in replace(): $ sign at end of string"
        }
      } else if (c == '\\') {
        if (itr + 1 < rep.length) {
          itr += 1
          val next: Char = rep.charAt(itr)
          if (next != '\\' && next != '$') {
            return  "Invalid replacement string in replace(): \\ character must be followed by \\ or $"
          }
        } else {
          return "Invalid replacement string in replace(): \\ character at end of string"
        }
      }
    }
    null
  }

}

class Replace extends RegexFunction {

  private var replacementChecked: Boolean = false

   override def allowRegexMatchingEmptyString(): Boolean = false

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val maybeQ: Boolean = arguments.length == 4 &&
      (!(arguments(3).isInstanceOf[StringLiteral]) ||
        arguments(3)
          .asInstanceOf[StringLiteral]
          .getStringValue
          .contains("q"))
    if (arguments(2).isInstanceOf[StringLiteral] && !maybeQ) {
      val rep: String = arguments(2).asInstanceOf[StringLiteral].getStringValue
      if (checkReplacement(rep) == null) {
        replacementChecked = true
      }
    }
    super.makeFunctionCall(arguments: _*)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val arg0: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val input: CharSequence = if (arg0 == null) "" else arg0.getStringValueCS
    val arg2: StringValue = arguments(2).head.asInstanceOf[StringValue]
    val replacement: CharSequence = arg2.getStringValueCS
    val re: RegularExpression = getRegularExpression(arguments)
    if (!re.getFlags.contains("q")) {
      if (!replacementChecked) {
        val msg = checkReplacement(replacement)
        if (msg != null) {
          throw new XPathException(msg, "FORX0004", context)
        }
      }
    }
    val res: CharSequence = re.replace(input, replacement)
    StringValue.makeStringValue(res)
  }

}
