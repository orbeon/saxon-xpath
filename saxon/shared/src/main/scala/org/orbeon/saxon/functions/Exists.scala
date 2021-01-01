package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.Token

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.LookaheadIterator

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.Cardinality

import Exists._

object Exists {

  private def exists(iter: SequenceIterator): Boolean = {
    var result: Boolean = false
    result =
      if (iter.getProperties.contains(SequenceIterator.Property.LOOKAHEAD))
        iter.asInstanceOf[LookaheadIterator].hasNext
      else iter.next() != null
    iter.close()
    result
  }

}

class Exists extends Aggregate {

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def optimize(visitor: ExpressionVisitor,
                            contextInfo: ContextItemStaticInfo): Expression = {
        val e2: Expression = super.optimize(visitor, contextInfo)
        if (e2 != this) {
          return e2
        }
        val c: Int = getArg(0).getCardinality
        if (!Cardinality.allowsZero(c)) {
          return Literal.makeLiteral(BooleanValue.TRUE, e2)
        } else if (c == StaticProperty.ALLOWS_ZERO) {
         return Literal.makeLiteral(BooleanValue.FALSE, e2)
        }
        setArg(0, getArg(0).unordered(retainAllNodes = false, forStreaming = visitor.isOptimizeForStreaming))
        if (getArg(0)
          .isInstanceOf[VennExpression] && !visitor.isOptimizeForStreaming) {
          val v: VennExpression = getArg(0).asInstanceOf[VennExpression]
          if (v.getOperator == Token.UNION) {
            val e0: Expression = SystemFunction.makeCall(
              "exists",
              getRetainedStaticContext,
              v.getLhsExpression)
            val e1: Expression = SystemFunction.makeCall(
              "exists",
              getRetainedStaticContext,
              v.getRhsExpression)
            return new OrExpression(e0, e1).optimize(visitor, contextInfo)
          }
        }
        this
      }

      override def effectiveBooleanValue(c: XPathContext): Boolean =
        try {
          var result: Boolean = false
          val iter: SequenceIterator = getArg(0).iterate(c)
          result =
            if (iter.getProperties.contains(
              SequenceIterator.Property.LOOKAHEAD))
              iter.asInstanceOf[LookaheadIterator].hasNext
            else iter.next() != null
          iter.close()
          result
        } catch {
          case e: XPathException => {
            e.maybeSetLocation(getLocation)
            e.maybeSetContext(c)
            throw e
          }

        }

      override def evaluateItem(context: XPathContext): BooleanValue =
        BooleanValue.get(effectiveBooleanValue(context))

      override def getNetCost(): Int = 0
    }

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression = {
    var args = arguments.toArray
    val c: Int = args(0).getCardinality
    if (c == StaticProperty.ALLOWS_ONE_OR_MORE) {
     return Literal.makeLiteral(BooleanValue.TRUE, args(0))
    } else if (c == StaticProperty.ALLOWS_ZERO) {
      return Literal.makeLiteral(BooleanValue.FALSE, args(0))
    }
    args(0) = args(0).unordered(retainAllNodes = false, forStreaming = visitor.isOptimizeForStreaming)
    if (args(0).isInstanceOf[VennExpression] && !visitor.isOptimizeForStreaming) {
      val v: VennExpression = args(0).asInstanceOf[VennExpression]
      if (v.getOperator == Token.UNION) {
        val e0: Expression = SystemFunction.makeCall("exists",
          getRetainedStaticContext,
          v.getLhsExpression)
        val e1: Expression = SystemFunction.makeCall("exists",
          getRetainedStaticContext,
          v.getRhsExpression)
        return new OrExpression(e0, e1).optimize(visitor, contextInfo)
      }
    }
    null
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(exists(arguments(0).iterate()))

  override def getCompilerName(): String = "ExistsCompiler"

  override def getStreamerName: String = "Exists"

}
