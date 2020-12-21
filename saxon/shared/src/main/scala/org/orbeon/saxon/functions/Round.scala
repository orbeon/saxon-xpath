package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, XPathContext}
import org.orbeon.saxon.om.{Sequence, ZeroOrOne}
import org.orbeon.saxon.value.NumericValue

class Round extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NumericValue] = {
    val val0 = arguments(0).head.asInstanceOf[NumericValue]
    if (val0 == null) {
      ZeroOrOne.empty
    } else {
      var scaleRnd = 0
      if (arguments.length == 2) {
        val scaleVal = arguments(1).head.asInstanceOf[NumericValue]
        scaleRnd = scaleVal.longValue().toInt
      }
      new ZeroOrOne(val0.round(scaleRnd))
    }
  }

  override def getCompilerName: String = "RoundingCompiler"
}
