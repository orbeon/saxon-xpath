////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.Token

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.LookaheadIterator

import net.sf.saxon.value.BooleanValue

import Empty._


object Empty {

  private def empty(iter: SequenceIterator): Boolean = {
    var result: Boolean = false
    result =
      if (iter.getProperties.contains(SequenceIterator.Property.LOOKAHEAD))
        !iter.asInstanceOf[LookaheadIterator].hasNext
      else iter.next() == null
    iter.close()
    result
  }

}

/**
 * Implementation of the fn:empty function
 */
class Empty extends Aggregate {

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression = {
    // See if we can deduce the answer from the cardinality
    val c: Int = arguments(0).getCardinality
    if (c == StaticProperty.ALLOWS_ONE_OR_MORE) {
      Literal.makeLiteral(BooleanValue.FALSE, arguments(0))
    } else if (c == StaticProperty.ALLOWS_ZERO) {
      Literal.makeLiteral(BooleanValue.TRUE, arguments(0))
    }
    //    empty(A|B) => empty(A) and empty(B)
    if (arguments(0)
      .isInstanceOf[VennExpression] && !visitor.isOptimizeForStreaming) {
      val v: VennExpression = arguments(0).asInstanceOf[VennExpression]
      if (v.getOperator == Token.UNION) {
        val e0: Expression = SystemFunction.makeCall("empty",
          getRetainedStaticContext,
          v.getLhsExpression)
        val e1: Expression = SystemFunction.makeCall("empty",
          getRetainedStaticContext,
          v.getRhsExpression)
        new AndExpression(e0, e1).optimize(visitor, contextInfo)
      }
    }
    null
  }

  // Rewrite
  // Rewrite


  /*Evaluate the expression

   @param context   the dynamic evaluation context
   @param arguments the values of the arguments, supplied as Sequences
   @return the result of the evaluation, in the form of a Sequence
   @throws net.sf.saxon.trans.XPathException
            if a dynamic error occurs during the evaluation of the expression*/
  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(empty(arguments(0).iterate()))

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      /*@NotNull*/

      override def optimize(visitor: ExpressionVisitor,
                            contextInfo: ContextItemStaticInfo): Expression = {
        val e2: Expression = super.optimize(visitor, contextInfo)
        if (e2 != this) {
          e2
        }
        // See if we can deduce the answer from the cardinality
        val c: Int = getArg(0).getCardinality
        if (c == StaticProperty.ALLOWS_ONE_OR_MORE) {
          Literal.makeLiteral(BooleanValue.FALSE, e2)
        } else if (c == StaticProperty.ALLOWS_ZERO) {
          Literal.makeLiteral(BooleanValue.TRUE, e2)
        }
        // Don't sort the argument
        setArg(0, getArg(0).unordered(false, visitor.isOptimizeForStreaming))
        //    empty(A|B) => empty(A) and empty(B)
        if (getArg(0).isInstanceOf[VennExpression]) {
          val v: VennExpression = getArg(0).asInstanceOf[VennExpression]
          if (v.getOperator == Token.UNION && !visitor.isOptimizeForStreaming) {
            val e0: Expression = SystemFunction.makeCall(
              "empty",
              getRetainedStaticContext,
              v.getLhsExpression)
            val e1: Expression = SystemFunction.makeCall(
              "empty",
              getRetainedStaticContext,
              v.getRhsExpression)
            new AndExpression(e0, e1).optimize(visitor, contextInfo)
          }
        }
        this
      }

      // Rewrite
      // Rewrite

      override def evaluateItem(context: XPathContext): BooleanValue =
        BooleanValue.get(effectiveBooleanValue(context))

      override def effectiveBooleanValue(c: XPathContext): Boolean = {
        val iter: SequenceIterator = getArg(0).iterate(c)
        var result: Boolean = false
        result =
          if (iter.getProperties.contains(SequenceIterator.Property.LOOKAHEAD))
            !iter.asInstanceOf[LookaheadIterator].hasNext
          else iter.next() == null
        iter.close()
        result
      }

      override def getNetCost(): Int = 0
    }

  /*
     Perform optimisation of an expression and its subexpressions.
     <p>This method is called after all references to functions and variables have been resolved
     to the declaration of the function or variable, and after all type checking has been done.</p>

     @param visitor     an expression visitor
     @param contextInfo the static type of "." at the point where this expression is invoked.
                        The parameter is set to null if it is known statically that the context item will be undefined.
                        If the type of the context item is not known statically, the argument is set to
                        {@link net.sf.saxon.model.Type#ITEM_TYPE}
     @return the original expression, rewritten if appropriate to optimize execution
     @throws net.sf.saxon.trans.XPathException if an error is discovered during this phase
                                               (typically a type error)
*/
  /**
   * Evaluate the function
   */
  /**
   * Evaluate the function in a boolean context
   */
  override def getCompilerName(): String = "EmptyCompiler"

  override def getStreamerName(): String = "Empty"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
