package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.NumericValue

class Round extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NumericValue] = {
    val val0: NumericValue = arguments(0).head.asInstanceOf[NumericValue]
    if (val0 == null) {
      ZeroOrOne.empty()
    }
    var scaleRnd: Int = 0
    if (arguments.length == 2) {
      val scaleVal: NumericValue =
        arguments(1).head.asInstanceOf[NumericValue]
      scaleRnd = scaleVal.longValue().toInt
    }
    new ZeroOrOne(val0.round(scaleRnd))
  }

  override def getCompilerName(): String = "RoundingCompiler"

}
