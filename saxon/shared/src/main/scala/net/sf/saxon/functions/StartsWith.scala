package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.sort.CodepointCollator

import net.sf.saxon.lib.SubstringMatcher

import net.sf.saxon.om.Sequence

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.StringValue

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
    val s0: StringValue = arguments(0).head().asInstanceOf[StringValue]
    val s1: StringValue = arguments(1).head().asInstanceOf[StringValue]
    BooleanValue.get(
      startsWith(s0, s1, getStringCollator.asInstanceOf[SubstringMatcher]))
  }

  override def getCompilerName(): String = "StartsWithCompiler"

}
