////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Negatable

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.TypeChecker

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.Sequence

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue


class NotFn extends SystemFunction {

  override def supplyTypeInformation(visitor: ExpressionVisitor,
                                     contextItemType: ContextItemStaticInfo,
                                     arguments: Array[Expression]): Unit = {
    val err = TypeChecker.ebvError(
      arguments(0),
      visitor.getConfiguration.getTypeHierarchy)
    if (err != null) {
      throw err
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(
      !ExpressionTool.effectiveBooleanValue(arguments(0).iterate()))

  /**
   * Make an expression that either calls this function, or that is equivalent to a call
   * on this function
   *
   * @param arguments the supplied arguments to the function call
   * @return either a function call on this function, or an expression that delivers
   *         the same result
   */
  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def effectiveBooleanValue(c: XPathContext): Boolean =
        try !getArg(0).effectiveBooleanValue(c)
        catch {
          case e: XPathException => {
            e.maybeSetLocation(getLocation)
            e.maybeSetContext(c)
            throw e
          }

        }
    }

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression = {
    val th: TypeHierarchy =
      visitor.getStaticContext.getConfiguration.getTypeHierarchy
    if (arguments(0).isInstanceOf[Negatable] &&
      arguments(0).asInstanceOf[Negatable].isNegatable(th)) {
      arguments(0).asInstanceOf[Negatable].negate()
    }
    if (arguments(0).getItemType.isInstanceOf[NodeTest]) {
      val empty: SystemFunction =
        SystemFunction.makeFunction("empty", getRetainedStaticContext, 1)
      empty.makeFunctionCall(arguments(0)).optimize(visitor, contextInfo)
    }
    null
  }

  override def getCompilerName(): String = "NotFnCompiler"

  override def getStreamerName: String = "NotFn"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class supports the XPath functions boolean(), not(), true(), and false()
 */
