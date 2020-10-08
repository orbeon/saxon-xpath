package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, Literal, XPathContext}
import org.orbeon.saxon.functions.MathFunctionSet._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.{Item, One, Sequence, ZeroOrOne}
import org.orbeon.saxon.value.{DoubleValue, NumericValue}

object MathFunctionSet {

  private var THE_INSTANCE: MathFunctionSet = new MathFunctionSet()

  def getInstance: MathFunctionSet = THE_INSTANCE

  class PiFn extends SystemFunction {

    override def makeFunctionCall(arguments: Expression*): Expression =
      Literal.makeLiteral(new DoubleValue(Math.PI))

    def call(context: XPathContext, arguments: Array[Sequence]): DoubleValue =
      new DoubleValue(Math.PI)
  }

  abstract class TrigFn1 extends SystemFunction {

    def compute(input: Double): Double

    def call(context: XPathContext, args: Array[Sequence]): ZeroOrOne[_ <: Item] = {
      val in: DoubleValue = args(0).head.asInstanceOf[DoubleValue]
      if (in == null) {
        ZeroOrOne.empty()
      } else {
        One.dbl(compute(in.getDoubleValue))
      }
    }
  }

  class SinFn extends TrigFn1 {
     def compute(input: Double): Double = Math.sin(input)
  }

  class CosFn extends TrigFn1 {
     def compute(input: Double): Double = Math.cos(input)
  }

  class TanFn extends TrigFn1 {
    def compute(input: Double): Double = Math.tan(input)
  }

  class AsinFn extends TrigFn1 {
    def compute(input: Double): Double = Math.asin(input)
  }

  class AcosFn extends TrigFn1 {

     def compute(input: Double): Double = Math.acos(input)

  }

  class AtanFn extends TrigFn1 {

     def compute(input: Double): Double = Math.atan(input)

  }

  class SqrtFn extends TrigFn1 {

     def compute(input: Double): Double = Math.sqrt(input)

  }

  class LogFn extends TrigFn1 {

     def compute(input: Double): Double = Math.log(input)

  }

  class Log10Fn extends TrigFn1 {

     def compute(input: Double): Double = Math.log10(input)

  }

  class ExpFn extends TrigFn1 {

     def compute(input: Double): Double = Math.exp(input)

  }

  class Exp10Fn extends TrigFn1 {

     def compute(input: Double): Double = Math.pow(10, input)

  }

  class PowFn extends SystemFunction {

    override def call(context: XPathContext,
                      args: Array[Sequence]): ZeroOrOne[DoubleValue] = {
      val x: DoubleValue = args(0).head.asInstanceOf[DoubleValue]
      var result: DoubleValue = null
      if (x == null) {
        result = null
      } else {
        val dx: Double = x.getDoubleValue
        if (dx == 1) {
          result = x
        } else {
          val y: NumericValue = args(1).head.asInstanceOf[NumericValue]
          assert(y != null)
          val dy: Double = y.getDoubleValue
          result =
            if (dx == -1 && java.lang.Double.isInfinite(dy))
              new DoubleValue(1.0e0)
            else new DoubleValue(Math.pow(dx, dy))
        }
      }
      new ZeroOrOne(result)
    }

  }

  class Atan2Fn extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): DoubleValue = {
      val y: DoubleValue = arguments(0).head.asInstanceOf[DoubleValue]
      assert(y != null)
      val x: DoubleValue = arguments(1).head.asInstanceOf[DoubleValue]
      assert(x != null)
      val result: Double = Math.atan2(y.getDoubleValue, x.getDoubleValue)
      new DoubleValue(result)
    }

  }

}

class MathFunctionSet private () extends BuiltInFunctionSet {

  init()

  private def reg1(name: String, make: () => SystemFunction): Unit =
    register(name, 1, make, BuiltInAtomicType.DOUBLE, OPT, CARD0)
      .arg(0, BuiltInAtomicType.DOUBLE, OPT, EMPTY)

  private def init(): Unit = {
    register("pi", 0,  () => new PiFn, BuiltInAtomicType.DOUBLE, ONE, 0)
    reg1    ("sin",    () => new SinFn)
    reg1    ("cos",    () => new CosFn)
    reg1    ("tan",    () => new TanFn)
    reg1    ("asin",   () => new AsinFn)
    reg1    ("acos",   () => new AcosFn)
    reg1    ("atan",   () => new AtanFn)
    reg1    ("sqrt",   () => new SqrtFn)
    reg1    ("log",    () => new LogFn)
    reg1    ("log10",  () => new Log10Fn)
    reg1    ("exp",    () => new ExpFn)
    reg1    ("exp10",  () => new Exp10Fn)
    register("pow", 2, () => new PowFn, BuiltInAtomicType.DOUBLE, OPT, CARD0)
      .arg(0, BuiltInAtomicType.DOUBLE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DOUBLE, ONE, null)
    register("atan2", 2, () => new Atan2Fn, BuiltInAtomicType.DOUBLE, ONE, 0)
      .arg(0, BuiltInAtomicType.DOUBLE, ONE, null)
      .arg(1, BuiltInAtomicType.DOUBLE, ONE, null)
  }

  override def getNamespace(): String = NamespaceConstant.MATH

  override def getConventionalPrefix(): String = "math"

}
