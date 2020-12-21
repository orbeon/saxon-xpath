package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.FunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.model.ConversionResult

import org.orbeon.saxon.model.Converter

import org.orbeon.saxon.model.ValidationFailure

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value._

import Number_1._

object Number_1 {

  def toNumber(arg0: AtomicValue): DoubleValue =
    if (arg0.isInstanceOf[BooleanValue]) {
      Converter.BooleanToDouble.INSTANCE
        .convert(arg0.asInstanceOf[BooleanValue])
    } else if (arg0.isInstanceOf[NumericValue]) {
      Converter.NumericToDouble.INSTANCE
        .convert(arg0.asInstanceOf[NumericValue])
        .asAtomic()
        .asInstanceOf[DoubleValue]
    } else if (arg0.isInstanceOf[StringValue] && !(arg0
      .isInstanceOf[AnyURIValue])) {
      val cr: ConversionResult =
        StringToDouble11.getInstance.convert(arg0.asInstanceOf[StringValue])
      if (cr.isInstanceOf[ValidationFailure]) {
        DoubleValue.NaN
      } else {
        cr.asInstanceOf[DoubleValue]
      }
    } else {
      DoubleValue.NaN
    }

  def convert(value: AtomicValue, config: Configuration): DoubleValue =
    try {
      if (value == null) {
        DoubleValue.NaN
      }
      if (value.isInstanceOf[BooleanValue]) {
        new DoubleValue(
          if (value.asInstanceOf[BooleanValue].getBooleanValue) 1.0e0
          else 0.0e0)
      }
      if (value.isInstanceOf[DoubleValue]) {
        value.asInstanceOf[DoubleValue]
      }
      if (value.isInstanceOf[NumericValue]) {
        new DoubleValue(value.asInstanceOf[NumericValue].getDoubleValue)
      }
      if (value
        .isInstanceOf[StringValue] && !(value.isInstanceOf[AnyURIValue])) {
        val d: Double = config.getConversionRules.getStringToDoubleConverter
          .stringToNumber(value.getStringValueCS)
        new DoubleValue(d)
      }
      DoubleValue.NaN
    } catch {
      case e: NumberFormatException => DoubleValue.NaN

    }

}

class Number_1 extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    toNumber(arg.asInstanceOf[AtomicValue])

  override def resultWhenEmpty(): ZeroOrOne[AtomicValue] = new One(DoubleValue.NaN)

  override def typeCheckCaller(caller: FunctionCall,
                               visitor: ExpressionVisitor,
                               contextInfo: ContextItemStaticInfo): Expression = {
    if (caller.getArg(0).isCallOn(classOf[Number_1])) {
      caller.setArg(0, caller.getArg(0).asInstanceOf[FunctionCall].getArg(0))
    }
    caller
  }

  override def getCompilerName(): String = "NumberFnCompiler"

}
