package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.NumericValue

import Subsequence_3._

object Subsequence_3 {

  def subSequence(seq: SequenceIterator,
                  startVal: NumericValue,
                  lengthVal: NumericValue,
                  context: XPathContext): SequenceIterator = {
    var startNumVal = startVal
    var lengthNumVal = lengthVal
    if (startNumVal.isInstanceOf[Int64Value] && lengthNumVal
      .isInstanceOf[Int64Value]) {
      val lstart: Long = startNumVal.longValue()
      if (lstart > java.lang.Integer.MAX_VALUE) {
        EmptyIterator.emptyIterator
      }
      var llength: Long = lengthNumVal.longValue()
      if (llength > java.lang.Integer.MAX_VALUE) {
        llength = java.lang.Integer.MAX_VALUE
      }
      if (llength < 1) {
        EmptyIterator.emptyIterator
      }
      val lend: Long = lstart + llength - 1
      if (lend < 1) {
        EmptyIterator.emptyIterator
      }
      val start: Int = if (lstart < 1) 1 else lstart.toInt
      SubsequenceIterator.make(seq, start, lend.toInt)
    } else {
      if (startNumVal.isNaN) {
        EmptyIterator.emptyIterator
      }
      if (startNumVal.compareTo(Int64Value.MAX_LONG) > 0) {
        EmptyIterator.emptyIterator
      }
      startNumVal = startNumVal.round(0)
      if (lengthNumVal.isNaN) {
        EmptyIterator.emptyIterator
      }
      lengthNumVal = lengthNumVal.round(0)
      if (lengthNumVal.compareTo(Int64Value.ZERO) <= 0) {
        EmptyIterator.emptyIterator
      }
      var rend: NumericValue = ArithmeticExpression
        .compute(startNumVal, Calculator.PLUS, lengthNumVal, context)
        .asInstanceOf[NumericValue]
      if (rend.isNaN) {
        EmptyIterator.emptyIterator
      }
      rend = ArithmeticExpression
        .compute(rend, Calculator.MINUS, Int64Value.PLUS_ONE, context)
        .asInstanceOf[NumericValue]
      if (rend.compareTo(Int64Value.ZERO) <= 0) {
        EmptyIterator.emptyIterator
      }
      var lstart: Long = 0L
      lstart =
        if (startNumVal.compareTo(Int64Value.PLUS_ONE) <= 0) 1
        else startNumVal.longValue()
      if (lstart > java.lang.Integer.MAX_VALUE) {
        EmptyIterator.emptyIterator
      }
      var lend: Long = 0L
      lend =
        if (rend.compareTo(Int64Value.MAX_LONG) >= 0)
          java.lang.Integer.MAX_VALUE
        else rend.longValue()
      SubsequenceIterator.make(seq, lstart.toInt, lend.toInt)
    }
  }

}

class Subsequence_3 extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    arguments(0).getSpecialProperties

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(
      subSequence(arguments(0).iterate(),
        arguments(1).head.asInstanceOf[NumericValue],
        arguments(2).head.asInstanceOf[NumericValue],
        context))

  override def getStreamerName: String = "Subsequence"

}
