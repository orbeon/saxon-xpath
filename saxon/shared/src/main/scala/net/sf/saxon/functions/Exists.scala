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

import net.sf.saxon.value.Cardinality

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
          e2
        }
        val c: Int = getArg(0).getCardinality
        if (!Cardinality.allowsZero(c)) {
          Literal.makeLiteral(BooleanValue.TRUE, e2)
        } else if (c == StaticProperty.ALLOWS_ZERO) {
          Literal.makeLiteral(BooleanValue.FALSE, e2)
        }
        setArg(0, getArg(0).unordered(false, visitor.isOptimizeForStreaming))
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
            new OrExpression(e0, e1).optimize(visitor, contextInfo)
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
      Literal.makeLiteral(BooleanValue.TRUE, args(0))
    } else if (c == StaticProperty.ALLOWS_ZERO) {
      Literal.makeLiteral(BooleanValue.FALSE, args(0))
    }
    args(0) = args(0).unordered(false, visitor.isOptimizeForStreaming)
    if (args(0).isInstanceOf[VennExpression] && !visitor.isOptimizeForStreaming) {
      val v: VennExpression = args(0).asInstanceOf[VennExpression]
      if (v.getOperator == Token.UNION) {
        val e0: Expression = SystemFunction.makeCall("exists",
          getRetainedStaticContext,
          v.getLhsExpression)
        val e1: Expression = SystemFunction.makeCall("exists",
          getRetainedStaticContext,
          v.getRhsExpression)
        new OrExpression(e0, e1).optimize(visitor, contextInfo)
      }
    }
    null
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue =
    BooleanValue.get(exists(arguments(0).iterate()))

  override def getCompilerName(): String = "ExistsCompiler"

  override def getStreamerName(): String = "Exists"

}
