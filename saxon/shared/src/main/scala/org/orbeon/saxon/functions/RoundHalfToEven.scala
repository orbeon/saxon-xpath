package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, One, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.NumericValue

class RoundHalfToEven extends SystemFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val val0: NumericValue = arguments(0).head.asInstanceOf[NumericValue]
    if (val0 == null) {
      ZeroOrOne.empty()
    }
    var scale: Int = 0
    if (arguments.length == 2) {
      val scaleVal: NumericValue =
        arguments(1).head.asInstanceOf[NumericValue]
      if (scaleVal.compareTo(java.lang.Integer.MAX_VALUE) > 0) {
        new ZeroOrOne(val0)
      } else
        scale =
          if (scaleVal.compareTo(java.lang.Integer.MIN_VALUE) < 0)
            java.lang.Integer.MIN_VALUE
          else scaleVal.longValue().toInt
    }
    new One(val0.roundHalfToEven(scale))
  }

  override def getCompilerName(): String = "RoundingCompiler"

}
