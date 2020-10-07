package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.sort.CodepointCollator

import org.orbeon.saxon.lib.SubstringMatcher

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.StringValue

import StartsWith._

object StartsWith {

  def startsWith(arg0: StringValue,
                 arg1: StringValue,
                 collator: SubstringMatcher): Boolean = {
    if (arg1 == null || arg1.isZeroLength ||
      collator.comparesEqual(arg1.getPrimitiveStringValue, "")) {
      return true
    }
    if (arg0 == null || arg0.isZeroLength) {
      return false
    }
    val s0: String = arg0.getStringValue
    val s1: String = arg1.getStringValue
    collator.startsWith(s0, s1)
  }

}

class StartsWith extends CollatingFunctionFixed {

  override def isSubstringMatchingFunction(): Boolean = true

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression =
    if (getStringCollator == CodepointCollator.getInstance) {
      new SystemFunctionCall.Optimized(this, arguments.toArray) {
        override def effectiveBooleanValue(context: XPathContext): Boolean = {
          val s0: String = getArg(0).evaluateAsString(context).toString
          val s1: String = getArg(1).evaluateAsString(context).toString
          s0.startsWith(s1)
        }
      }
    } else {
      super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
    }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val s0: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val s1: StringValue = arguments(1).head.asInstanceOf[StringValue]
    BooleanValue.get(
      startsWith(s0, s1, getStringCollator.asInstanceOf[SubstringMatcher]))
  }

  override def getCompilerName(): String = "StartsWithCompiler"

}
