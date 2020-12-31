package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, XPathContext}
import org.orbeon.saxon.om.{Item, One, Sequence, ZeroOrOne}
import org.orbeon.saxon.value.NumericValue

class RoundHalfToEven extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val val0 = arguments(0).head.asInstanceOf[NumericValue]
    if (val0 == null) {
      return ZeroOrOne.empty
    } else {
      var scale = 0
      if (arguments.length == 2) {
        val scaleVal = arguments(1).head.asInstanceOf[NumericValue]
        if (scaleVal.compareTo(java.lang.Integer.MAX_VALUE) > 0) {
          return new ZeroOrOne(val0)
        } else
          scale =
            if (scaleVal.compareTo(java.lang.Integer.MIN_VALUE) < 0)
              java.lang.Integer.MIN_VALUE
            else
              scaleVal.longValue().toInt
      }
      new One(val0.roundHalfToEven(scale))
    }
  }

  override def getCompilerName: String = "RoundingCompiler"
}
