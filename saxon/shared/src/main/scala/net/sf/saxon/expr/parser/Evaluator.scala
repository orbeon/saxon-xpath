package net.sf.saxon.expr.parser

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.ComplexContentOutputter

import net.sf.saxon.event.SequenceCollector

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.Block

import net.sf.saxon.om._

import EvaluationMode._

import net.sf.saxon.value._

import java.util.ArrayList

import java.util.List

object Evaluator {

  val EMPTY_SEQUENCE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      EmptySequence.getInstance

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.RETURN_EMPTY_SEQUENCE
  }

  val LITERAL: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      expr.asInstanceOf[Literal].getValue

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.EVALUATE_LITERAL
  }

  val VARIABLE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      try expr.asInstanceOf[VariableReference].evaluateVariable(context)
      catch {
        case e: ClassCastException => {
          assert(false)
          LAZY_SEQUENCE.evaluate(expr, context)
        }

      }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.EVALUATE_AND_MATERIALIZE_VARIABLE
  }

  val SUPPLIED_PARAMETER: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      try expr
        .asInstanceOf[SuppliedParameterReference]
        .evaluateVariable(context)
      catch {
        case e: ClassCastException => {
          assert(false)
          LAZY_SEQUENCE.evaluate(expr, context)
        }

      }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.EVALUATE_SUPPLIED_PARAMETER
  }

  val SINGLE_ITEM: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Item =
      expr.evaluateItem(context)

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.CALL_EVALUATE_SINGLE_ITEM
  }

  val OPTIONAL_ITEM: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val item: Item = expr.evaluateItem(context)
      if (item == null) EmptySequence.getInstance else item
    }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.CALL_EVALUATE_OPTIONAL_ITEM
  }

  val LAZY_SEQUENCE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter: SequenceIterator = expr.iterate(context)
      new LazySequence(iter)
    }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.MAKE_CLOSURE
  }

  val MEMO_SEQUENCE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter: SequenceIterator = expr.iterate(context)
      new MemoSequence(iter)
    }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.MAKE_MEMO_CLOSURE
  }

  val MEMO_CLOSURE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      new MemoClosure(expr, context)

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.MAKE_MEMO_CLOSURE
  }

  val SINGLETON_CLOSURE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      new SingletonClosure(expr, context)

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.MAKE_SINGLETON_CLOSURE
  }

  val EAGER_SEQUENCE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val iter: SequenceIterator = expr.iterate(context)
      iter.materialize()
    }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.ITERATE_AND_MATERIALIZE
  }

  val SHARED_APPEND: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      if (expr.isInstanceOf[Block]) {
        val block: Block = expr.asInstanceOf[Block]
        val children: Array[Operand] = block.getOperanda
        val subsequences: List[GroundedValue] =
          new ArrayList[GroundedValue](children.length)
        for (o <- children) {
          val child: Expression = o.getChildExpression
          if (Cardinality.allowsMany(child.getCardinality)) {
            subsequences.add(child.iterate(context).materialize())
          } else {
            val j: Item = child.evaluateItem(context)
            if (j != null) {
              subsequences.add(j)
            }
          }
        }
        new Chain(subsequences)
      } else {
        expr.iterate(context).materialize()
      }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.SHARED_APPEND_EXPRESSION
  }

  val STREAMING_ARGUMENT: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      context.getConfiguration
        .obtainOptimizer
        .evaluateStreamingArgument(expr, context)

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.STREAMING_ARGUMENT
  }

  val MAKE_INDEXED_VARIABLE: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence =
      context.getConfiguration
        .obtainOptimizer
        .makeIndexedValue(expr.iterate(context))

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.MAKE_INDEXED_VARIABLE
  }

  val PROCESS: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val controller: Controller = context.getController
      val seq: SequenceCollector = controller.allocateSequenceOutputter
      val out: ComplexContentOutputter = new ComplexContentOutputter(seq)
      out.open()
      expr.process(out, context)
      out.close()
      val `val`: Sequence = seq.getSequence
      seq.reset()
      `val`
    }

    override def getEvaluationMode(): EvaluationMode = EvaluationMode.PROCESS
  }

  val LAZY_TAIL: Evaluator = new Evaluator() {
    override def evaluate(expr: Expression, context: XPathContext): Sequence = {
      val tail: TailExpression = expr.asInstanceOf[TailExpression]
      val vr: VariableReference =
        tail.getBaseExpression.asInstanceOf[VariableReference]
      var base: Sequence = Evaluator.VARIABLE.evaluate(vr, context)
      if (base.isInstanceOf[MemoClosure]) {
        val it: SequenceIterator = base.iterate()
        base = it.materialize()
      }
      if (base.isInstanceOf[IntegerRange]) {
        val start: Long = base
          .asInstanceOf[IntegerRange]
          .getStart + tail.getStart -
          1
        val end: Long = base.asInstanceOf[IntegerRange].getEnd
        if (start == end) {
          Int64Value.makeIntegerValue(end)
        } else if (start > end) {
          EmptySequence.getInstance
        } else {
          new IntegerRange(start, end)
        }
      }
      if (base.isInstanceOf[GroundedValue]) {
        val baseSeq: GroundedValue = base.asInstanceOf[GroundedValue]
        baseSeq.subsequence(tail.getStart - 1,
          baseSeq.getLength - tail.getStart + 1)
      }
      new MemoClosure(tail, context)
    }

    override def getEvaluationMode(): EvaluationMode =
      EvaluationMode.LAZY_TAIL_EXPRESSION
  }

}

abstract class Evaluator {

  def evaluate(expr: Expression, context: XPathContext): Sequence

  def getEvaluationMode(): EvaluationMode

}
