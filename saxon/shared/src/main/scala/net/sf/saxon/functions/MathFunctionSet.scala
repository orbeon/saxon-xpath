package net.sf.saxon.functions

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.Literal
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.registry.BuiltInFunctionSet
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.om.{Item, One, Sequence, ZeroOrOne}
import BuiltInFunctionSet._
import net.sf.saxon.value.DoubleValue
import net.sf.saxon.value.NumericValue
import MathFunctionSet._

object MathFunctionSet {

  private var THE_INSTANCE: MathFunctionSet = new MathFunctionSet()

  def getInstance(): MathFunctionSet = THE_INSTANCE

  class PiFn extends SystemFunction {

    override def makeFunctionCall(arguments: Expression*): Expression =
      Literal.makeLiteral(new DoubleValue(Math.PI))

    def call(context: XPathContext, arguments: Array[Sequence]): DoubleValue =
      new DoubleValue(Math.PI)

  }

  abstract class TrigFn1 extends SystemFunction {

     def compute(input: Double): Double

    def call(context: XPathContext, args: Array[Sequence]): ZeroOrOne[_ <: Item] = {
      val in: DoubleValue = args(0).head().asInstanceOf[DoubleValue]
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
      val x: DoubleValue = args(0).head().asInstanceOf[DoubleValue]
      var result: DoubleValue = null
      if (x == null) {
        result = null
      } else {
        val dx: Double = x.getDoubleValue
        if (dx == 1) {
          result = x
        } else {
          val y: NumericValue = args(1).head().asInstanceOf[NumericValue]
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
      val y: DoubleValue = arguments(0).head().asInstanceOf[DoubleValue]
      assert(y != null)
      val x: DoubleValue = arguments(1).head().asInstanceOf[DoubleValue]
      assert(x != null)
      val result: Double = Math.atan2(y.getDoubleValue, x.getDoubleValue)
      new DoubleValue(result)
    }

  }

}

class MathFunctionSet private () extends BuiltInFunctionSet {

  init()

  private def reg1(name: String,
                   implementation: Class[_ <: SystemFunction]): Unit = {
    register(name, 1, implementation, BuiltInAtomicType.DOUBLE, OPT, CARD0)
      .arg(0, BuiltInAtomicType.DOUBLE, OPT, EMPTY)
  }

  private def init(): Unit = {
    register("pi", 0, classOf[PiFn], BuiltInAtomicType.DOUBLE, ONE, 0)
    reg1("sin", classOf[SinFn])
    reg1("cos", classOf[CosFn])
    reg1("tan", classOf[TanFn])
    reg1("asin", classOf[AsinFn])
    reg1("acos", classOf[AcosFn])
    reg1("atan", classOf[AtanFn])
    reg1("sqrt", classOf[SqrtFn])
    reg1("log", classOf[LogFn])
    reg1("log10", classOf[Log10Fn])
    reg1("exp", classOf[ExpFn])
    reg1("exp10", classOf[Exp10Fn])
    register("pow", 2, classOf[PowFn], BuiltInAtomicType.DOUBLE, OPT, CARD0)
      .arg(0, BuiltInAtomicType.DOUBLE, OPT, EMPTY)
      .arg(1, BuiltInAtomicType.DOUBLE, ONE, null)
    register("atan2", 2, classOf[Atan2Fn], BuiltInAtomicType.DOUBLE, ONE, 0)
      .arg(0, BuiltInAtomicType.DOUBLE, ONE, null)
      .arg(1, BuiltInAtomicType.DOUBLE, ONE, null)
  }

  override def getNamespace(): String = NamespaceConstant.MATH

  override def getConventionalPrefix(): String = "math"

}
