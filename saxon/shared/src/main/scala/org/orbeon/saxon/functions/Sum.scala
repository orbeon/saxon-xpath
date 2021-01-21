package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._
import org.orbeon.saxon.functions.Sum._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Item, Sequence, SequenceIterator}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._


object Sum {

  def total(
    in      : SequenceIterator,
    context : XPathContext,
    locator : Location
  ): AtomicValue =
    try {
      val fold = new SumFold(context, null)
      in.forEachOrFail(fold.processItem)
      fold.result().head.asInstanceOf[AtomicValue]
    } catch {
      case e: XPathException =>
        e.maybeSetLocation(locator)
        throw e
    }

  private class SumFold(
    private var context   : XPathContext,
    private var zeroValue : AtomicValue
  ) extends Fold {

    private var data: AtomicValue = _
    private var atStart: Boolean = true

    private val rules = context.getConfiguration.getConversionRules
    private val toDouble = BuiltInAtomicType.DOUBLE.getStringConverter(rules)

    def processItem(item: Item): Unit = {
      var next = item.asInstanceOf[AtomicValue]
      if (atStart) {
        atStart = false
        if (next.isInstanceOf[UntypedAtomicValue]) {
          data = toDouble.convert(next).asAtomic
          return
        } else if (next.isInstanceOf[NumericValue] || next.isInstanceOf[DayTimeDurationValue] || next.isInstanceOf[YearMonthDurationValue]) {
          data = next
          return
        } else {
          val err = new XPathException(
            "Input to sum() contains a value of type " + next.getPrimitiveType.getDisplayName +
              " which is neither numeric, nor a duration")
          err.setXPathContext(context)
          err.setErrorCode("FORG0006")
          throw err
        }
      }
      data match {
        case _: NumericValue      =>
          if (next.isInstanceOf[UntypedAtomicValue]) {
            next = toDouble.convert(next).asAtomic
          } else if (! next.isInstanceOf[NumericValue]) {
            val err = new XPathException(
              "Input to sum() contains a mix of numeric and non-numeric values"
            )
            err.setXPathContext(context)
            err.setErrorCode("FORG0006")
            throw err
          }
          data = ArithmeticExpression.compute(data, Calculator.PLUS, next, context)
        case durationValue: DurationValue =>
          if (!( data.isInstanceOf[DayTimeDurationValue] || data.isInstanceOf[YearMonthDurationValue])) {
            val err = new XPathException(
              "Input to sum() contains a duration that is neither a dayTimeDuration nor a yearMonthDuration"
            )
            err.setXPathContext(context)
            err.setErrorCode("FORG0006")
            throw err
          }
          if (! next.isInstanceOf[DurationValue]) {
            val err = new XPathException(
              "Input to sum() contains a mix of duration and non-duration values"
            )
            err.setXPathContext(context)
            err.setErrorCode("FORG0006")
            throw err
          }
          data = durationValue.add(next.asInstanceOf[DurationValue])
        case _                    =>
          val err = new XPathException(
            "Input to sum() contains a value of type " + data.getPrimitiveType.getDisplayName +
              " which is neither numeric, nor a duration"
          )
          err.setXPathContext(context)
          err.setErrorCode("FORG0006")
          throw err
      }
    }

    def isFinished: Boolean = data.isInstanceOf[DoubleValue] && data.isNaN

    def result(): Sequence =
      if (atStart)
        if (zeroValue == null) EmptySequence.getInstance else zeroValue
      else
        data
  }
}

class Sum extends FoldingFunction {

  override def getResultItemType(args: Array[Expression]): ItemType = {
    val th = getRetainedStaticContext.getConfiguration.getTypeHierarchy
    var base = Atomizer.getAtomizedItemType(args(0), alwaysUntyped = false, th)
    if (base == BuiltInAtomicType.UNTYPED_ATOMIC)
      base = BuiltInAtomicType.DOUBLE
    if (Cardinality.allowsZero(args(0).getCardinality)) {
      if (getArity == 1)
        Type.getCommonSuperType(base, BuiltInAtomicType.INTEGER, th)
      else
        Type.getCommonSuperType(base, args(1).getItemType, th)
    } else {
      base.getPrimitiveItemType
    }
  }

  override def getCardinality(arguments: Array[Expression]): Int =
    if (getArity == 1 || arguments(1).getCardinality == 1)
      StaticProperty.EXACTLY_ONE
    else
      StaticProperty.ALLOWS_ZERO_OR_ONE

  def getFold(context: XPathContext, additionalArguments: Sequence*): Fold =
    if (additionalArguments.nonEmpty) {
      val z = additionalArguments(0).head.asInstanceOf[AtomicValue]
      new SumFold(context, z)
    } else {
      new SumFold(context, Int64Value.ZERO)
    }

  override def getCompilerName: String = "SumCompiler"
}
