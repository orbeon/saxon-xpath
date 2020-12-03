package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.event.ComplexContentOutputter
import org.orbeon.saxon.expr.EvaluationMode._
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.Block
import org.orbeon.saxon.om._
import org.orbeon.saxon.value._

import java.util


object Evaluator {

  val EMPTY_SEQUENCE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      EmptySequence.getInstance

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.RETURN_EMPTY_SEQUENCE
  }

  val LITERAL: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      expr.asInstanceOf[Literal].getValue

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.EVALUATE_LITERAL
  }

  val VARIABLE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      try expr.asInstanceOf[VariableReference].evaluateVariable(context)
      catch {
        case _: ClassCastException =>
          assert(false)
          LAZY_SEQUENCE.evaluate(expr, context)
      }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.EVALUATE_AND_MATERIALIZE_VARIABLE
  }

  val SUPPLIED_PARAMETER: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      try expr
        .asInstanceOf[SuppliedParameterReference]
        .evaluateVariable(context)
      catch {
        case _: ClassCastException =>
          assert(false)
          LAZY_SEQUENCE.evaluate(expr, context)
      }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.EVALUATE_SUPPLIED_PARAMETER
  }

  val SINGLE_ITEM: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Item =
      expr.evaluateItem(context)

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.CALL_EVALUATE_SINGLE_ITEM
  }

  val OPTIONAL_ITEM: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val item = expr.evaluateItem(context)
      if (item == null) EmptySequence.getInstance else item
    }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.CALL_EVALUATE_OPTIONAL_ITEM
  }

  val LAZY_SEQUENCE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter = expr.iterate(context)
      new LazySequence(iter)
    }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.MAKE_CLOSURE
  }

  val MEMO_SEQUENCE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter = expr.iterate(context)
      new MemoSequence(iter)
    }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.MAKE_MEMO_CLOSURE
  }

  val MEMO_CLOSURE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      new MemoClosure(expr, context)

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.MAKE_MEMO_CLOSURE
  }

  val SINGLETON_CLOSURE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      new SingletonClosure(expr, context)

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.MAKE_SINGLETON_CLOSURE
  }

  val EAGER_SEQUENCE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter: SequenceIterator = expr.iterate(context)
      iter.materialize
    }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.ITERATE_AND_MATERIALIZE
  }

  val SHARED_APPEND: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      expr match {
        case block: Block =>
          val children     = block.getOperanda
          val subsequences = new util.ArrayList[GroundedValue](children.length)
          for (o <- children) {
            val child = o.getChildExpression
            if (Cardinality.allowsMany(child.getCardinality)) {
              subsequences.add(child.iterate(context).materialize)
            } else {
              val j = child.evaluateItem(context)
              if (j != null)
                subsequences.add(j)
            }
          }
          new Chain(subsequences)
        case _            =>
          expr.iterate(context).materialize
      }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.SHARED_APPEND_EXPRESSION
  }

  val STREAMING_ARGUMENT: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      context.getConfiguration
        .obtainOptimizer
        .evaluateStreamingArgument(expr, context)

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.STREAMING_ARGUMENT
  }

  val MAKE_INDEXED_VARIABLE: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence =
      context.getConfiguration
        .obtainOptimizer
        .makeIndexedValue(expr.iterate(context))

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.MAKE_INDEXED_VARIABLE
  }

  val PROCESS: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val controller = context.getController
      val seq = controller.allocateSequenceOutputter
      val out = new ComplexContentOutputter(seq)
      out.open()
      expr.process(out, context)
      out.close()
      val `val` = seq.getSequence
      seq.reset()
      `val`
    }

    def getEvaluationMode: EvaluationMode = EvaluationMode.PROCESS
  }

  val LAZY_TAIL: Evaluator = new Evaluator() {
    def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val tail = expr.asInstanceOf[TailExpression]
      val vr = tail.getBaseExpression.asInstanceOf[VariableReference]
      var base = Evaluator.VARIABLE.evaluate(vr, context)
      if (base.isInstanceOf[MemoClosure]) {
        val it = base.iterate()
        base = it.materialize
      }
      base match {
        case range: IntegerRange =>
          val start = range.getStart + tail.getStart - 1
          val end   = range.getEnd
          if (start == end)
            Int64Value.makeIntegerValue(end)
          else if (start > end)
            EmptySequence.getInstance
          else
            new IntegerRange(start, end)
        case baseSeq: GroundedValue =>
          baseSeq.subsequence(
            tail.getStart - 1,
            baseSeq.getLength - tail.getStart + 1
          )
        case _                      =>
          new MemoClosure(tail, context)
      }
    }

    def getEvaluationMode: EvaluationMode =
      EvaluationMode.LAZY_TAIL_EXPRESSION
  }
}

abstract class Evaluator {
  def evaluate(expr: Expression, context: XPathContext): Sequence
  def getEvaluationMode: EvaluationMode
}
