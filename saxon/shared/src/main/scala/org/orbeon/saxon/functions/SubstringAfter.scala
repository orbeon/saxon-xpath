package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.SubstringMatcher

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import SubstringAfter._

object SubstringAfter {

  private def substringAfter(arg1: StringValue,
                             arg2: StringValue,
                             collator: SubstringMatcher): StringValue = {
    var strArg1 = arg1
    var strArg2 = arg2
    if (strArg1 == null) {
      strArg1 = StringValue.EMPTY_STRING
    }
    if (strArg2 == null) {
      strArg2 = StringValue.EMPTY_STRING
    }
    if (strArg2.isZeroLength) {
      return strArg1
    }
    if (strArg1.isZeroLength) {
     return  StringValue.EMPTY_STRING
    }
    val s1: String = strArg1.getStringValue
    val s2: String = strArg2.getStringValue
    val result: String = collator.substringAfter(s1, s2)
    val s: StringValue = StringValue.makeStringValue(result)
    if (strArg1.isKnownToContainNoSurrogates) {
      s.setContainsNoSurrogates()
    }
    s
  }

}

class SubstringAfter extends CollatingFunctionFixed {

  override def isSubstringMatchingFunction(): Boolean = true

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val arg1: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val arg2: StringValue = arguments(1).head.asInstanceOf[StringValue]
    substringAfter(arg1,
      arg2,
      getStringCollator.asInstanceOf[SubstringMatcher])
  }

  override def getCompilerName(): String = "SubstringAfterCompiler"

}
