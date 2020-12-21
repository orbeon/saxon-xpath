package org.orbeon.saxon.model

import java.math.BigDecimal

import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.om.NamespaceResolver
import org.orbeon.saxon.value._

import scala.beans.BeanProperty


object Converter {

  def convert(value: AtomicValue, targetType: AtomicType, rules: ConversionRules): AtomicValue = {
    val converter = rules.getConverter(value.getPrimitiveType, targetType)
    if (converter eq null) {
      val ve = new ValidationFailure("Cannot convert value from " + value.getPrimitiveType + " to " + targetType)
      ve.setErrorCode("FORG0001")
      throw ve.makeException()
    }
    converter.convert(value).asAtomic
  }

  abstract class UnfailingConverter extends Converter {
    override def convert(input: AtomicValue): AtomicValue
    override def isAlwaysSuccessful: Boolean = true
  }

  object IdentityConverter {
    val INSTANCE: IdentityConverter = new IdentityConverter
  }

  class IdentityConverter extends Converter {
    def convert(input: AtomicValue): ConversionResult = input
    override def isAlwaysSuccessful: Boolean = true
  }

  class UpCastingConverter(annotation: AtomicType) extends UnfailingConverter {
    private val newTypeAnnotation: AtomicType = annotation
    def convert(input: AtomicValue): AtomicValue = input.copyAsSubType(newTypeAnnotation)
  }

  class DownCastingConverter(annotation: AtomicType, rules: ConversionRules)
    extends Converter {

    private val newType: AtomicType = annotation

    this.conversionRules = rules

    def getTargetType: AtomicType = newType

    def convert(input: AtomicValue): ConversionResult =
      convert(input, input.getCanonicalLexicalRepresentation)

    def convert(input: AtomicValue,
                lexicalForm: CharSequence): ConversionResult = {
      val f = newType.validate(input, lexicalForm, getConversionRules)
      if (f == null)
        input.copyAsSubType(newType)
      else
        f
    }

    def validate(input: AtomicValue, lexicalForm: CharSequence): ValidationFailure =
      newType.validate(input, lexicalForm, getConversionRules)
  }

  object TwoPhaseConverter {

    def makeTwoPhaseConverter(inputType: AtomicType,
                              viaType: AtomicType,
                              outputType: AtomicType,
                              rules: ConversionRules): TwoPhaseConverter =
      new TwoPhaseConverter(rules.getConverter(inputType, viaType),
        rules.getConverter(viaType, outputType))
  }

  class TwoPhaseConverter(private val phaseOne: Converter,
                          private val phaseTwo: Converter)
    extends Converter {

    override def setNamespaceResolver(resolver: NamespaceResolver): Converter =
      new TwoPhaseConverter(phaseOne.setNamespaceResolver(resolver),
        phaseTwo.setNamespaceResolver(resolver))

    def convert(input: AtomicValue): ConversionResult = {
      val temp = phaseOne.convert(input)
      if (temp.isInstanceOf[ValidationFailure])
        return temp
      val aTemp = temp.asInstanceOf[AtomicValue]
      phaseTwo match {
        case downCastingConverter: DownCastingConverter =>
          downCastingConverter.convert(aTemp, aTemp.getCanonicalLexicalRepresentation)
        case _ =>
          phaseTwo.convert(aTemp)
      }
    }
  }

  object ToUntypedAtomicConverter {
    val INSTANCE: ToUntypedAtomicConverter = new ToUntypedAtomicConverter
  }

  class ToUntypedAtomicConverter extends UnfailingConverter {
    def convert(input: AtomicValue): UntypedAtomicValue =
      new UntypedAtomicValue(input.getStringValueCS)
  }

  object ToStringConverter {
    val INSTANCE: ToStringConverter = new ToStringConverter
  }

  class ToStringConverter extends UnfailingConverter {
    def convert(input: AtomicValue): StringValue =
      new StringValue(input.getStringValueCS)
  }

  object NumericToFloat {
    val INSTANCE: NumericToFloat = new NumericToFloat
  }

  class NumericToFloat extends UnfailingConverter {
    def convert(input: AtomicValue): FloatValue =
      new FloatValue(input.asInstanceOf[NumericValue].getFloatValue)
  }

  object BooleanToFloat {
    val INSTANCE: BooleanToFloat = new BooleanToFloat
  }

  class BooleanToFloat extends UnfailingConverter {
    def convert(input: AtomicValue): FloatValue =
      new FloatValue(if (input.asInstanceOf[BooleanValue].getBooleanValue) 1.0f else 0.0f)
  }

  object NumericToDouble {
    val INSTANCE: NumericToDouble = new NumericToDouble
  }

  class NumericToDouble extends UnfailingConverter {
    def convert(input: AtomicValue): DoubleValue =
      input match {
        case doubleValue: DoubleValue =>
          doubleValue
        case _ =>
          new DoubleValue(input.asInstanceOf[NumericValue].getDoubleValue)
      }
  }

  object BooleanToDouble {
    val INSTANCE: BooleanToDouble = new BooleanToDouble
  }

  class BooleanToDouble extends UnfailingConverter {
    def convert(input: AtomicValue): DoubleValue =
      new DoubleValue(if (input.asInstanceOf[BooleanValue].getBooleanValue) 1.0e0 else 0.0e0)
  }

  object DoubleToDecimal {
    val INSTANCE: DoubleToDecimal = new DoubleToDecimal
  }

  class DoubleToDecimal extends Converter {
    def convert(input: AtomicValue): ConversionResult =
      try new BigDecimalValue(input.asInstanceOf[DoubleValue].getDoubleValue)
      catch {
        case e: ValidationException => e.getValidationFailure
      }
  }

  object FloatToDecimal {
    val INSTANCE: FloatToDecimal = new FloatToDecimal
  }

  class FloatToDecimal extends Converter {
    def convert(input: AtomicValue): ConversionResult =
      try new BigDecimalValue(input.asInstanceOf[FloatValue].getFloatValue)
      catch {
        case e: ValidationException => e.getValidationFailure
      }
  }

  object IntegerToDecimal {
    val INSTANCE: IntegerToDecimal = new IntegerToDecimal
  }

  class IntegerToDecimal extends UnfailingConverter {
    def convert(input: AtomicValue): BigDecimalValue =
      input match {
        case int64Value: Int64Value =>
          new BigDecimalValue(int64Value.longValue())
        case _ =>
          new BigDecimalValue(input.asInstanceOf[BigIntegerValue].asDecimal())
      }
  }

  object NumericToDecimal {
    val INSTANCE: NumericToDecimal = new NumericToDecimal
  }

  class NumericToDecimal extends Converter {
    def convert(input: AtomicValue): ConversionResult =
      try {
        val decimal: BigDecimal =
          input.asInstanceOf[NumericValue].getDecimalValue
        new BigDecimalValue(decimal)
      } catch {
        case e: ValidationException => e.getValidationFailure
      }
  }

  object BooleanToDecimal {
    val INSTANCE: BooleanToDecimal = new BooleanToDecimal
  }

  class BooleanToDecimal extends UnfailingConverter {
    def convert(input: AtomicValue): BigDecimalValue =
      if (input.asInstanceOf[BooleanValue].getBooleanValue)
        BigDecimalValue.ONE
      else
        BigDecimalValue.ZERO
  }

  object DoubleToInteger {
    val INSTANCE: DoubleToInteger = new DoubleToInteger
  }

  class DoubleToInteger extends Converter {
    def convert(input: AtomicValue): ConversionResult =
      IntegerValue.makeIntegerValue(input.asInstanceOf[DoubleValue])
  }

  object FloatToInteger {
    val INSTANCE: FloatToInteger = new FloatToInteger
  }

  class FloatToInteger extends Converter {
    def convert(input: AtomicValue): ConversionResult =
      IntegerValue.makeIntegerValue(
        new DoubleValue(input.asInstanceOf[FloatValue].getDoubleValue))
  }

  object DecimalToInteger {
    val INSTANCE: DecimalToInteger = new DecimalToInteger
  }

  class DecimalToInteger extends UnfailingConverter {
    def convert(input: AtomicValue): IntegerValue =
      input match {
        case integerValue: IntegerValue => integerValue
        case _ => IntegerValue.makeIntegerValue(input.asInstanceOf[BigDecimalValue].getDecimalValue.toBigInteger)
      }
  }

  object NumericToInteger {
    val INSTANCE: NumericToInteger = new NumericToInteger
  }

  class NumericToInteger extends Converter {

    def convert(input: AtomicValue): ConversionResult = {
      val in = input.asInstanceOf[NumericValue]
      try in match {
        case _: IntegerValue =>
          in
        case doubleValue: DoubleValue =>
          IntegerValue.makeIntegerValue(doubleValue)
        case _: FloatValue =>
          IntegerValue.makeIntegerValue(new DoubleValue(in.getDoubleValue))
        case _ =>
          IntegerValue.makeIntegerValue(in.getDecimalValue.toBigInteger)
      } catch {
        case e: ValidationException => e.getValidationFailure
      }
    }
  }

  object BooleanToInteger {
    val INSTANCE: BooleanToInteger = new BooleanToInteger
  }

  class BooleanToInteger extends UnfailingConverter {
    def convert(input: AtomicValue): Int64Value =
      if (input.asInstanceOf[BooleanValue].getBooleanValue)
        Int64Value.PLUS_ONE
      else
        Int64Value.ZERO
  }

  object DurationToDayTimeDuration {
    val INSTANCE: DurationToDayTimeDuration = new DurationToDayTimeDuration
  }

  class DurationToDayTimeDuration extends UnfailingConverter {

    def convert(duration: AtomicValue): DayTimeDurationValue = {
      val d = duration.asInstanceOf[DurationValue]
      if (d.signum() < 0) {
        new DayTimeDurationValue(-d.getDays,
          -d.getHours,
          -d.getMinutes,
          -d.getSeconds,
          -d.getNanoseconds)
      } else {
        new DayTimeDurationValue(d.getDays,
          d.getHours,
          d.getMinutes,
          d.getSeconds,
          d.getNanoseconds)
      }
    }
  }

  object DurationToYearMonthDuration {
    val INSTANCE: DurationToYearMonthDuration = new DurationToYearMonthDuration
  }

  class DurationToYearMonthDuration extends UnfailingConverter {
    def convert(input: AtomicValue): YearMonthDurationValue = YearMonthDurationValue.fromMonths(
        input.asInstanceOf[DurationValue].getTotalMonths)
  }

  object DateToDateTime {
    val INSTANCE: DateToDateTime = new DateToDateTime
  }

  class DateToDateTime extends UnfailingConverter {
    def convert(input: AtomicValue): DateTimeValue = input.asInstanceOf[DateValue].toDateTime
  }

  object DateTimeToDate {
    val INSTANCE: DateTimeToDate = new DateTimeToDate
  }

  class DateTimeToDate extends UnfailingConverter {
    def convert(input: AtomicValue): DateValue = {
      val dt: DateTimeValue = input.asInstanceOf[DateTimeValue]
      new DateValue(dt.getYear,
        dt.getMonth,
        dt.getDay,
        dt.getTimezoneInMinutes,
        dt.isXsd10Rules)
    }
  }

  object DateTimeToGMonth {
    val INSTANCE: DateTimeToGMonth = new DateTimeToGMonth
  }

  class DateTimeToGMonth extends UnfailingConverter {
    def convert(input: AtomicValue): GMonthValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new GMonthValue(dt.getMonth, dt.getTimezoneInMinutes)
    }
  }

  object DateTimeToGYearMonth {
    val INSTANCE: DateTimeToGYearMonth = new DateTimeToGYearMonth
  }

  class DateTimeToGYearMonth extends UnfailingConverter {
    def convert(input: AtomicValue): GYearMonthValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new GYearMonthValue(dt.getYear,
        dt.getMonth,
        dt.getTimezoneInMinutes,
        dt.isXsd10Rules)
    }
  }

  object DateTimeToGYear {
    val INSTANCE: DateTimeToGYear = new DateTimeToGYear
  }

  class DateTimeToGYear extends UnfailingConverter {
    def convert(input: AtomicValue): GYearValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new GYearValue(dt.getYear, dt.getTimezoneInMinutes, dt.isXsd10Rules)
    }
  }

  object DateTimeToGMonthDay {
    val INSTANCE: DateTimeToGMonthDay = new DateTimeToGMonthDay
  }

  class DateTimeToGMonthDay extends UnfailingConverter {
    def convert(input: AtomicValue): GMonthDayValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new GMonthDayValue(dt.getMonth, dt.getDay, dt.getTimezoneInMinutes)
    }
  }

  object DateTimeToGDay {
    val INSTANCE: DateTimeToGDay = new DateTimeToGDay
  }

  class DateTimeToGDay extends UnfailingConverter {
    def convert(input: AtomicValue): GDayValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new GDayValue(dt.getDay, dt.getTimezoneInMinutes)
    }
  }

  object DateTimeToTime {
    val INSTANCE: DateTimeToTime = new DateTimeToTime
  }

  class DateTimeToTime extends UnfailingConverter {
    def convert(input: AtomicValue): TimeValue = {
      val dt = input.asInstanceOf[DateTimeValue]
      new TimeValue(dt.getHour,
        dt.getMinute,
        dt.getSecond,
        dt.getNanosecond,
        dt.getTimezoneInMinutes,
        "")
    }
  }

  object NumericToBoolean {
    val INSTANCE: NumericToBoolean = new NumericToBoolean
  }

  class NumericToBoolean extends UnfailingConverter {
    def convert(input: AtomicValue): BooleanValue =
      BooleanValue.get(input.asInstanceOf[NumericValue].effectiveBooleanValue)
  }

  object Base64BinaryToHexBinary {
    val INSTANCE: Base64BinaryToHexBinary = new Base64BinaryToHexBinary
  }

  class Base64BinaryToHexBinary extends UnfailingConverter {
    def convert(input: AtomicValue): HexBinaryValue =
      new HexBinaryValue(input.asInstanceOf[Base64BinaryValue].getBinaryValue)
  }

  object HexBinaryToBase64Binary {
    val INSTANCE: HexBinaryToBase64Binary = new HexBinaryToBase64Binary
  }

  class HexBinaryToBase64Binary extends UnfailingConverter {
    def convert(input: AtomicValue): Base64BinaryValue =
      new Base64BinaryValue(input.asInstanceOf[HexBinaryValue].getBinaryValue)
  }

  object NotationToQName {
    val INSTANCE: NotationToQName = new NotationToQName
  }

  class NotationToQName extends UnfailingConverter {
    def convert(input: AtomicValue): QNameValue =
      new QNameValue(input.asInstanceOf[NotationValue].getStructuredQName, BuiltInAtomicType.QNAME)
  }

  object QNameToNotation {
    val INSTANCE: QNameToNotation = new QNameToNotation
  }

  class QNameToNotation extends UnfailingConverter {
    def convert(input: AtomicValue): NotationValue =
      new NotationValue(input.asInstanceOf[QNameValue].getStructuredQName, BuiltInAtomicType.NOTATION)
  }

  class PromoterToDouble extends Converter {

    private var stringToDouble: StringConverter = null

    override def convert(input: AtomicValue): ConversionResult =
      input match {
        case _: DoubleValue =>
          input
        case numericValue: NumericValue =>
          new DoubleValue(numericValue.getDoubleValue)
        case _: UntypedAtomicValue =>
          if (stringToDouble == null)
            stringToDouble = BuiltInAtomicType.DOUBLE.getStringConverter(getConversionRules)
          stringToDouble.convert(input)
        case _ =>
          val err = new ValidationFailure(
            "Cannot promote non-numeric value to xs:double")
          err.setErrorCode("XPTY0004")
          err
      }
  }

  class PromoterToFloat extends Converter {

    private var stringToFloat: StringConverter = null

    override def convert(input: AtomicValue): ConversionResult =
      input match {
        case _: FloatValue =>
          input
        case _: DoubleValue =>
          val err: ValidationFailure = new ValidationFailure(
            "Cannot promote from xs:double to xs:float")
          err.setErrorCode("XPTY0004")
          err
        case numericValue: NumericValue =>
          new FloatValue(numericValue.getDoubleValue.toFloat)
        case _: UntypedAtomicValue =>
          if (stringToFloat == null)
            stringToFloat = BuiltInAtomicType.FLOAT.getStringConverter(getConversionRules)
          stringToFloat.convert(input)
        case _ =>
          val err = new ValidationFailure(
            "Cannot promote non-numeric value to xs:double")
          err.setErrorCode("XPTY0004")
          err
      }
  }
}

abstract class Converter {

  @BeanProperty
  var conversionRules: ConversionRules = _

  def this(rules: ConversionRules) = {
    this
    this.conversionRules = rules
  }

  def convert(input: AtomicValue): ConversionResult
  def isAlwaysSuccessful: Boolean = false
  def setNamespaceResolver(resolver: NamespaceResolver): Converter = this
  def getNamespaceResolver: NamespaceResolver = null
}
