package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.StringConverter

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value._


class Average extends FoldingFunction {

  override def getCardinality(arguments: Array[Expression]): Int =
    if (!Cardinality.allowsZero(arguments(0).getCardinality)) {
      StaticProperty.EXACTLY_ONE
    } else {
      super.getCardinality(arguments)
    }

  override def getFold(context: XPathContext,
                       additionalArguments: Sequence*): Fold =
    new AverageFold(context)

  private class AverageFold(private var context: XPathContext) extends Fold {

    private var data: AtomicValue = _

    private var atStart: Boolean = true

    private var rules: ConversionRules =
      context.getConfiguration.getConversionRules

    private var toDouble: StringConverter =
      BuiltInAtomicType.DOUBLE.getStringConverter(rules)

    private var count: Int = 0

    def processItem(item: Item): Unit = {
      var next: AtomicValue = item.asInstanceOf[AtomicValue]
      if (next.isInstanceOf[UntypedAtomicValue]) {
        next =
          toDouble.convert(next.asInstanceOf[UntypedAtomicValue]).asAtomic
      }
      count += 1
      if (atStart) {
        if (next.isInstanceOf[NumericValue] || next
          .isInstanceOf[DayTimeDurationValue] ||
          next.isInstanceOf[YearMonthDurationValue]) {
          data = next
          atStart = false
        } else if (next.isInstanceOf[DurationValue]) {
          throw new XPathException(
            "Input to avg() contains a duration (" + Err.depict(next) +
              ") that is neither an xs:dayTimeDuration nor an xs:yearMonthDuration",
            "FORG0006")
        } else {
          throw new XPathException(
            "Input to avg() contains a value (" + Err.depict(next) +
              ") that is neither numeric, nor a duration",
            "FORG0006")
        }
      } else {
        if (data.isInstanceOf[NumericValue]) {
          if (!(next.isInstanceOf[NumericValue])) {
            throw new XPathException(
              "Input to avg() contains a mix of numeric and non-numeric values",
              "FORG0006")
          }
          data =
            ArithmeticExpression.compute(data, Calculator.PLUS, next, context)
        } else if (data.isInstanceOf[DurationValue]) {
          if (!(next.isInstanceOf[DurationValue])) {
            throw new XPathException(
              "Input to avg() contains a mix of duration and non-duration values",
              "FORG0006")
          }
          try data = data
            .asInstanceOf[DurationValue]
            .add(next.asInstanceOf[DurationValue])
          catch {
            case e: XPathException => {
              if ("XPTY0004" == e.getErrorCodeLocalPart) {
                e.setErrorCode("FORG0006")
              }
              throw e
            }

          }
        } else {
          throw new XPathException(
            "Input to avg() contains a value (" + Err.depict(data) +
              ") that is neither numeric, nor a duration",
            "FORG0006")
        }
      }
    }

    def isFinished(): Boolean = data.isInstanceOf[DoubleValue] && data.isNaN

    def result(): Sequence =
      if (atStart) {
        EmptySequence.getInstance
      } else if (data.isInstanceOf[NumericValue]) {
        ArithmeticExpression.compute(data,
          Calculator.DIV,
          new Int64Value(count),
          context)
      } else {
        data.asInstanceOf[DurationValue].divide(count)
      }

  }

  override def call(context: XPathContext, args: Array[Sequence]): Sequence = call(context, args)
}