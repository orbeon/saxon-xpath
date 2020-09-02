package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.NumericValue

import Subsequence_2._

object Subsequence_2 {

  def subSequence(seq: SequenceIterator,
                  startVal: NumericValue): SequenceIterator = {
    var startNumVal = startVal
    var lstart: Long = 0L
    if (startNumVal.isInstanceOf[Int64Value]) {
      lstart = startNumVal.longValue()
      if (lstart <= 1) {
        return seq
      }
    } else if (startNumVal.isNaN) {
      return EmptyIterator.emptyIterator
    } else {
      startNumVal = startNumVal.round(0)
      if (startNumVal.compareTo(Int64Value.PLUS_ONE) <= 0) {
        return seq
      } else if (startNumVal.compareTo(Int64Value.MAX_LONG) > 0) {
        return EmptyIterator.emptyIterator
      } else {
        lstart = startNumVal.longValue()
      }
    }
    if (lstart > java.lang.Integer.MAX_VALUE) {
      return EmptyIterator.emptyIterator
    }
    TailIterator.make(seq, lstart.toInt)
  }

}

class Subsequence_2 extends SystemFunction with Callable {

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    arguments(0).getSpecialProperties

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality | StaticProperty.ALLOWS_ZERO_OR_ONE

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(
      subSequence(arguments(0).iterate(),
        arguments(1).head.asInstanceOf[NumericValue]))

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    try if (Literal.isAtomic(arguments(1)) && !(arguments(0)
      .isInstanceOf[ErrorExpression])) {
      var start: NumericValue = arguments(1)
        .asInstanceOf[Literal]
        .value
        .asInstanceOf[NumericValue]
      start = start.round(0)
      val intStart: Long = start.longValue()
      if (intStart > java.lang.Integer.MAX_VALUE) {
        super.makeFunctionCall(arguments.toIndexedSeq: _*)
      }
      if (intStart <= 0) {
        arguments(0)
      }
      new TailExpression(arguments(0), intStart.toInt)
    } catch {
      case e: Exception => {}

    }
    super.makeFunctionCall(arguments.toIndexedSeq: _*)
  }

  override def getStreamerName: String = "Subsequence"

}
