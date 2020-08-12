package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.NumericValue

class Round extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NumericValue] = {
    val val0: NumericValue = arguments(0).head().asInstanceOf[NumericValue]
    if (val0 == null) {
      ZeroOrOne.empty()
    }
    var scaleRnd: Int = 0
    if (arguments.length == 2) {
      val scaleVal: NumericValue =
        arguments(1).head().asInstanceOf[NumericValue]
      scaleRnd = scaleVal.longValue().toInt
    }
    new ZeroOrOne(val0.round(scaleRnd))
  }

  override def getCompilerName(): String = "RoundingCompiler"

}
