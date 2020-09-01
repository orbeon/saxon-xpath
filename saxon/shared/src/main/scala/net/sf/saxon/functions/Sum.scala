package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

import Sum._

object Sum {

  def total(in: SequenceIterator,
            context: XPathContext,
            locator: Location): AtomicValue =
    try {
      val fold: SumFold = new SumFold(context, null)
      in.forEachOrFail(fold.processItem)
      fold.result().head.asInstanceOf[AtomicValue]
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(locator)
        throw e
      }

    }

  private class SumFold(private var context: XPathContext,
                        private var zeroValue: AtomicValue)
    extends Fold {

    private var data: AtomicValue = _

    private var atStart: Boolean = true

    private var rules: ConversionRules =
      context.getConfiguration.getConversionRules

    private var toDouble: StringConverter =
      BuiltInAtomicType.DOUBLE.getStringConverter(rules)

    def processItem(item: Item): Unit = {
      var next: AtomicValue = item.asInstanceOf[AtomicValue]
      if (atStart) {
        atStart = false
        if (next.isInstanceOf[UntypedAtomicValue]) {
          data = toDouble.convert(next).asAtomic()
          return
        } else if (next.isInstanceOf[NumericValue] || next
          .isInstanceOf[DayTimeDurationValue] ||
          next.isInstanceOf[YearMonthDurationValue]) {
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
      if (data.isInstanceOf[NumericValue]) {
        if (next.isInstanceOf[UntypedAtomicValue]) {
          next = toDouble.convert(next).asAtomic()
        } else if (!(next.isInstanceOf[NumericValue])) {
          val err = new XPathException(
            "Input to sum() contains a mix of numeric and non-numeric values")
          err.setXPathContext(context)
          err.setErrorCode("FORG0006")
          throw err
        }
        data =
          ArithmeticExpression.compute(data, Calculator.PLUS, next, context)
      } else if (data.isInstanceOf[DurationValue]) {
        if (!((data.isInstanceOf[DayTimeDurationValue]) || (data
          .isInstanceOf[YearMonthDurationValue]))) {
          val err = new XPathException(
            "Input to sum() contains a duration that is neither a dayTimeDuration nor a yearMonthDuration")
          err.setXPathContext(context)
          err.setErrorCode("FORG0006")
          throw err
        }
        if (!(next.isInstanceOf[DurationValue])) {
          val err = new XPathException(
            "Input to sum() contains a mix of duration and non-duration values")
          err.setXPathContext(context)
          err.setErrorCode("FORG0006")
          throw err
        }
        data = data
          .asInstanceOf[DurationValue]
          .add(next.asInstanceOf[DurationValue])
      } else {
        val err = new XPathException(
          "Input to sum() contains a value of type " + data.getPrimitiveType.getDisplayName +
            " which is neither numeric, nor a duration")
        err.setXPathContext(context)
        err.setErrorCode("FORG0006")
        throw err
      }
    }

    def isFinished(): Boolean = data.isInstanceOf[DoubleValue] && data.isNaN

    def result(): Sequence =
      if (atStart) {
        if (zeroValue == null) EmptySequence.getInstance else zeroValue
      } else {
        data
      }

  }

}

class Sum extends FoldingFunction {

  override def getResultItemType(args: Array[Expression]): ItemType = {
    val th: TypeHierarchy =
      getRetainedStaticContext.getConfiguration.getTypeHierarchy
    var base: ItemType = Atomizer.getAtomizedItemType(args(0), alwaysUntyped = false, th)
    if (base == BuiltInAtomicType.UNTYPED_ATOMIC) {
      base = BuiltInAtomicType.DOUBLE
    }
    if (Cardinality.allowsZero(args(0).getCardinality)) {
      if (getArity == 1) {
        Type.getCommonSuperType(base, BuiltInAtomicType.INTEGER, th)
      } else {
        Type.getCommonSuperType(base, args(1).getItemType, th)
      }
    } else {
      base.getPrimitiveItemType
    }
  }

  override def getCardinality(arguments: Array[Expression]): Int =
    if (getArity == 1 || arguments(1).getCardinality == 1) {
      StaticProperty.EXACTLY_ONE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }

  override def getFold(context: XPathContext,
                       additionalArguments: Sequence*): Fold =
    if (additionalArguments.length > 0) {
      val z: AtomicValue =
        additionalArguments(0).head.asInstanceOf[AtomicValue]
      new SumFold(context, z)
    } else {
      new SumFold(context, Int64Value.ZERO)
    }

  override def getCompilerName(): String = "SumCompiler"

  override def call(context: XPathContext, args: Array[Sequence]): Sequence = call(context,args)
}
