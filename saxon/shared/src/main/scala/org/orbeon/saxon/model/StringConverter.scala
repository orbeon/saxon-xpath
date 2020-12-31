////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.model

import java.util.regex.Pattern

import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.model.Converter.DownCastingConverter
import org.orbeon.saxon.om.{NameChecker, NamespaceResolver, QNameException}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value._

//remove if not needed

object StringConverter {

  class StringToNonStringDerivedType(
                                      private var phaseOne: StringConverter,
                                      private var phaseTwo: DownCastingConverter)
    extends StringConverter {

    override def setNamespaceResolver(
                                       resolver: NamespaceResolver): StringToNonStringDerivedType =
      new StringToNonStringDerivedType(
        phaseOne.setNamespaceResolver(resolver).asInstanceOf[StringConverter],
        phaseTwo
          .setNamespaceResolver(resolver)
          .asInstanceOf[DownCastingConverter])

    def convert(input: StringValue): ConversionResult = {
      var in: CharSequence = input.getStringValueCS
      try in = phaseTwo.getTargetType.preprocess(in)
      catch {
        case err: ValidationException => return err.getValidationFailure

      }
      val temp: ConversionResult = phaseOne.convertString(in)
      if (temp.isInstanceOf[ValidationFailure]) {
        return temp
      }
      phaseTwo.convert(temp.asInstanceOf[AtomicValue], in)
    }

    def convertString(input: CharSequence): ConversionResult = {
      var inputChar = input
      try {
        inputChar = phaseTwo.getTargetType.preprocess(inputChar)
      } catch {
        case err: ValidationException => return err.getValidationFailure

      }
      val temp: ConversionResult = phaseOne.convertString(inputChar)
      if (temp.isInstanceOf[ValidationFailure]) {
        return temp
      }
      phaseTwo.convert(temp.asInstanceOf[AtomicValue], inputChar)
    }

    /**
      * Validate a string for conformance to the target type, without actually performing
      * the conversion
      *
      * @param input the string to be validated
      * @return null if validation is successful, or a ValidationFailure indicating the reasons for failure
      * if unsuccessful
      */
    override def validate(input: CharSequence): ValidationFailure = {
      var inputChar = input
      try {
         inputChar = phaseTwo.getTargetType.preprocess(inputChar)
      }
      catch {
        case err: ValidationException => return err.getValidationFailure

      }
      val temp: ConversionResult = phaseOne.convertString(inputChar)
      if (temp.isInstanceOf[ValidationFailure]) {
        return temp.asInstanceOf[ValidationFailure]
      }
      phaseTwo.validate(temp.asInstanceOf[AtomicValue], inputChar)
    }

  }

  object StringToString {

    val INSTANCE: StringToString = new StringToString()

  }

  class StringToString extends StringConverter {

    override def convert(input: AtomicValue): ConversionResult =
      new StringValue(input.getStringValueCS)

    def convertString(input: CharSequence): ConversionResult =
      new StringValue(input)

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure = null

    override def isAlwaysSuccessful(): Boolean = true

  }

  object StringToUntypedAtomic {

    val INSTANCE: StringToUntypedAtomic = new StringToUntypedAtomic()

  }

  class StringToUntypedAtomic extends StringConverter {

    override def convert(input: AtomicValue): UntypedAtomicValue =
      new UntypedAtomicValue(input.getStringValueCS)

    def convertString(input: CharSequence): ConversionResult =
      new UntypedAtomicValue(input)

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure = null

    override def isAlwaysSuccessful(): Boolean = true

  }

  object StringToNormalizedString {

    val INSTANCE: StringToNormalizedString = new StringToNormalizedString()

  }

  class StringToNormalizedString extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      new StringValue(Whitespace.normalizeWhitespace(input),
        BuiltInAtomicType.NORMALIZED_STRING)

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure = null

    override def isAlwaysSuccessful(): Boolean = true

  }

  object StringToToken {

    val INSTANCE: StringToToken = new StringToToken()

  }

  class StringToToken extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      new StringValue(Whitespace.collapseWhitespace(input),
        BuiltInAtomicType.TOKEN)

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure = null

    override def isAlwaysSuccessful(): Boolean = true

  }

  object StringToLanguage {

    private val regex: Pattern =
      Pattern.compile("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")

    val INSTANCE: StringToLanguage = new StringToLanguage()

  }

  class StringToLanguage extends StringConverter {

    def convertString(input: CharSequence): ConversionResult = {
      val trimmed: CharSequence = Whitespace.trimWhitespace(input)
      if (!StringToLanguage.regex.matcher(trimmed).matches()) {
        return new ValidationFailure(
          "The value '" + input + "' is not a valid xs:language")
      }
      new StringValue(trimmed, BuiltInAtomicType.LANGUAGE)
    }

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure =
      if (StringToLanguage.regex.matcher(Whitespace.trimWhitespace(input)).matches()) {
        null
      } else {
        new ValidationFailure(
          "The value '" + input + "' is not a valid xs:language")
      }

  }

  object StringToNCName {

    val TO_ID: StringToNCName = new StringToNCName(BuiltInAtomicType.ID)

    val TO_ENTITY: StringToNCName = new StringToNCName(
      BuiltInAtomicType.ENTITY)

    val TO_NCNAME: StringToNCName = new StringToNCName(
      BuiltInAtomicType.NCNAME)

    val TO_IDREF: StringToNCName = new StringToNCName(BuiltInAtomicType.IDREF)

  }

  class StringToNCName(var targetType: AtomicType) extends StringConverter {

    def convertString(input: CharSequence): ConversionResult = {
      val trimmed: CharSequence = Whitespace.trimWhitespace(input)
      if (NameChecker.isValidNCName(trimmed)) {
        new StringValue(trimmed, targetType)
      } else {
        new ValidationFailure(
          "The value '" + input + "' is not a valid " + targetType.getDisplayName)
      }
    }

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure =
      if (NameChecker.isValidNCName(Whitespace.trimWhitespace(input))) {
        null
      } else {
        new ValidationFailure(
          "The value '" + input + "' is not a valid " + targetType.getDisplayName)
      }

  }

  object StringToNMTOKEN {

    val INSTANCE: StringToNMTOKEN = new StringToNMTOKEN()

  }

  class StringToNMTOKEN extends StringConverter {

    def convertString(input: CharSequence): ConversionResult = {
      val trimmed: CharSequence = Whitespace.trimWhitespace(input)
      if (NameChecker.isValidNmtoken(trimmed)) {
        new StringValue(trimmed, BuiltInAtomicType.NMTOKEN)
      } else {
        new ValidationFailure(
          "The value '" + input + "' is not a valid xs:NMTOKEN")
      }
    }

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure =
      if (NameChecker.isValidNmtoken(Whitespace.trimWhitespace(input))) {
        null
      } else {
        new ValidationFailure(
          "The value '" + input + "' is not a valid xs:NMTOKEN")
      }

  }

  object StringToName {

    val INSTANCE: StringToName = new StringToName()

  }

  class StringToName extends StringToNCName(BuiltInAtomicType.NAME) {

    override def convertString(input: CharSequence): ConversionResult = {
      val vf: ValidationFailure = validate(input)
      if (vf == null) {
        new StringValue(Whitespace.trimWhitespace(input),
          BuiltInAtomicType.NAME)
      } else {
        vf
      }
    }

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure = {
      val trimmed: CharSequence = Whitespace.trimWhitespace(input)
      if (NameChecker.isValidNCName(trimmed)) {
        return null
      }
      val buff = new FastStringBuffer(trimmed.length)
      buff.cat(trimmed)
      for (i <- 0 until buff.length if buff.charAt(i) == ':') {
        buff.setCharAt(i, '_')
      }
      if (NameChecker.isValidNCName(buff)) {
        null
      } else {
        new ValidationFailure(
          "The value '" + trimmed + "' is not a valid xs:Name")
      }
    }

  }

  class StringToStringSubtype(rules: ConversionRules,
                              var targetType: AtomicType)
    extends StringConverter(rules) {

    var whitespaceAction: Int = targetType.getWhitespaceAction

    def convertString(input: CharSequence): ConversionResult = {
      var cs: CharSequence =
        Whitespace.applyWhitespaceNormalization(whitespaceAction, input)
      try cs = targetType.preprocess(cs)
      catch {
        case err: ValidationException => return err.getValidationFailure

      }
      val sv: StringValue = new StringValue(cs)
      val f: ValidationFailure =
        targetType.validate(sv, cs, getConversionRules)
      if (f == null) {
        sv.setTypeLabel(targetType)
        sv
      } else {
        f
      }
    }

    override def validate(input: CharSequence): ValidationFailure = {
      var cs: CharSequence =
        Whitespace.applyWhitespaceNormalization(whitespaceAction, input)
      try cs = targetType.preprocess(cs)
      catch {
        case err: ValidationException => return err.getValidationFailure

      }
      targetType.validate(new StringValue(cs), cs, getConversionRules)
    }

  }

  class StringToDerivedStringSubtype(rules: ConversionRules,
                                     var targetType: AtomicType)
    extends StringConverter(rules) {

    var builtInValidator: StringConverter = targetType.getBuiltInBaseType
      .asInstanceOf[AtomicType]
      .getStringConverter(rules)

    var whitespaceAction: Int = targetType.getWhitespaceAction

    def convertString(input: CharSequence): ConversionResult = {
      var cs: CharSequence =
        Whitespace.applyWhitespaceNormalization(whitespaceAction, input)
      var f: ValidationFailure = builtInValidator.validate(cs)
      if (f != null) {
        return f
      }
      try cs = targetType.preprocess(cs)
      catch {
        case err: ValidationException => return err.getValidationFailure

      }
      val sv: StringValue = new StringValue(cs)
      f = targetType.validate(sv, cs, getConversionRules)
      if (f == null) {
        sv.setTypeLabel(targetType)
        sv
      } else {
        f
      }
    }

  }

  class StringToFloat(rules: ConversionRules) extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      try {
        val flt: Float = getConversionRules.getStringToDoubleConverter
          .stringToNumber(input)
          .toFloat
        new FloatValue(flt)
      } catch {
        case err: NumberFormatException => {
          val ve: ValidationFailure = new ValidationFailure(
            "Cannot convert string to float: " + input)
          ve.setErrorCode("FORG0001")
          ve
        }

      }

  }

  object StringToDecimal {

    val INSTANCE: StringToDecimal = new StringToDecimal()

  }

  class StringToDecimal extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      BigDecimalValue.makeDecimalValue(input, validate = true)

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure =
      if (BigDecimalValue.castableAsDecimal(input)) {
        null
      } else {
        new ValidationFailure("Cannot convert string to decimal: " + input)
      }

  }

  object StringToInteger {

    val INSTANCE: StringToInteger = new StringToInteger()

  }

  class StringToInteger extends StringConverter {

    def convert(input: StringValue): ConversionResult =
      IntegerValue.stringToInteger(input.getStringValueCS)

    def convertString(input: CharSequence): ConversionResult =
      IntegerValue.stringToInteger(input)

    override def validate(input: CharSequence): ValidationFailure =
      IntegerValue.castableAsInteger(input)

  }

  class StringToIntegerSubtype(var targetType: BuiltInAtomicType)
    extends StringConverter {

    def convertString(input: CharSequence): ConversionResult = {
      val iv: ConversionResult = IntegerValue.stringToInteger(input)
      if (iv.isInstanceOf[Int64Value]) {
        val ok: Boolean = IntegerValue.checkRange(
          iv.asInstanceOf[Int64Value].longValue,
          targetType)
        if (ok) {
          iv.asInstanceOf[Int64Value].copyAsSubType(targetType)
        } else {
          new ValidationFailure(
            "Integer value is out of range for type " + targetType)
        }
      } else if (iv.isInstanceOf[BigIntegerValue]) {
        val ok: Boolean = IntegerValue.checkBigRange(
          iv.asInstanceOf[BigIntegerValue].asBigInteger(),
          targetType)
        if (ok) {
          iv.asInstanceOf[BigIntegerValue].setTypeLabel(targetType)
          iv
        } else {
          new ValidationFailure(
            "Integer value is out of range for type " + targetType)
        }
      } else {
        assert(iv.isInstanceOf[ValidationFailure])
        iv
      }
    }

  }

  object StringToDuration {

    val INSTANCE: StringToDuration = new StringToDuration()

  }

  class StringToDuration extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      DurationValue.makeDuration(input)

  }

  object StringToDayTimeDuration {

    val INSTANCE: StringToDayTimeDuration = new StringToDayTimeDuration()

  }

  class StringToDayTimeDuration extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      DayTimeDurationValue.makeDayTimeDurationValue(input)

  }

  object StringToYearMonthDuration {

    val INSTANCE: StringToYearMonthDuration = new StringToYearMonthDuration()

  }

  class StringToYearMonthDuration extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      YearMonthDurationValue.makeYearMonthDurationValue(input)

  }

  class StringToDateTime(rules: ConversionRules)
    extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      DateTimeValue.makeDateTimeValue(input, getConversionRules)

  }

  class StringToDateTimeStamp(rules: ConversionRules)
    extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult = {
      val `val`: ConversionResult =
        DateTimeValue.makeDateTimeValue(input, getConversionRules)
      if (`val`.isInstanceOf[DateTimeValue]) {
        if (!`val`.asInstanceOf[DateTimeValue].hasTimezone ){
          return new ValidationFailure(
            "Supplied DateTimeStamp value " + input + " has no time zone")
        } else {
           `val`
            .asInstanceOf[DateTimeValue]
            .setTypeLabel(BuiltInAtomicType.DATE_TIME_STAMP)
        }
      }
      `val`
    }

  }

  class StringToDate(rules: ConversionRules) extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      DateValue.makeDateValue(input, getConversionRules)

  }

  object StringToGMonth {

    val INSTANCE: StringToGMonth = new StringToGMonth()

  }

  class StringToGMonth extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      GMonthValue.makeGMonthValue(input)

  }

  class StringToGYearMonth(rules: ConversionRules)
    extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      GYearMonthValue.makeGYearMonthValue(input, getConversionRules)

  }

  class StringToGYear(rules: ConversionRules) extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      GYearValue.makeGYearValue(input, getConversionRules)

  }

  object StringToGMonthDay {

    val INSTANCE: StringToGMonthDay = new StringToGMonthDay()

  }

  class StringToGMonthDay extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      GMonthDayValue.makeGMonthDayValue(input)

  }

  object StringToGDay {

    val INSTANCE: StringToGDay = new StringToGDay()

  }

  class StringToGDay extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      GDayValue.makeGDayValue(input)

  }

  object StringToTime {

    val INSTANCE: StringToTime = new StringToTime()

  }

  class StringToTime extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      TimeValue.makeTimeValue(input)

  }

  object StringToBoolean {

    val INSTANCE: StringToBoolean = new StringToBoolean()

  }

  class StringToBoolean extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      BooleanValue.fromString(input)

  }

  object StringToHexBinary {

    val INSTANCE: StringToHexBinary = new StringToHexBinary()

  }

  class StringToHexBinary extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      try new HexBinaryValue(input)
      catch {
        case e: XPathException => ValidationFailure.fromException(e)

      }

  }

  object StringToBase64Binary {

    val INSTANCE: StringToBase64Binary = new StringToBase64Binary()

  }

  class StringToBase64Binary extends StringConverter {

    def convertString(input: CharSequence): ConversionResult =
      try new Base64BinaryValue(input)
      catch {
        case e: XPathException => ValidationFailure.fromException(e)

      }

  }

  class StringToQName(rules: ConversionRules) extends StringConverter(rules) {

    private var nsResolver: NamespaceResolver = _

    override def setNamespaceResolver(resolver: NamespaceResolver): StringToQName = {
      val c: StringToQName = new StringToQName(getConversionRules)
      c.nsResolver = resolver
      c
    }

    override def getNamespaceResolver(): NamespaceResolver = nsResolver

    def convertString(input: CharSequence): ConversionResult = {
      if (nsResolver == null) {
        throw new UnsupportedOperationException(
          "Cannot validate a QName without a namespace resolver")
      }
      try {
        val parts: Array[String] =
          NameChecker.getQNameParts(Whitespace.trimWhitespace(input))
        val uri: String = nsResolver.getURIForPrefix(parts(0), useDefault = true)
        if (uri == null) {
          val failure: ValidationFailure = new ValidationFailure(
            "Namespace prefix " + Err
              .wrap(parts(0)) + " has not been declared")
          failure.setErrorCode("FONS0004")
          return failure
        }
        new QNameValue(parts(0), uri, parts(1), BuiltInAtomicType.QNAME, false)
      } catch {
        case err: QNameException =>
          new ValidationFailure("Invalid lexical QName " + Err.wrap(input))

        case err: XPathException => new ValidationFailure(err.getMessage)

      }
    }

  }

  class StringToNotation(rules: ConversionRules)
    extends StringConverter(rules) {

    private var nsResolver: NamespaceResolver = _

    override def setNamespaceResolver(
                                       resolver: NamespaceResolver): StringToNotation = {
      val c: StringToNotation = new StringToNotation(getConversionRules)
      c.nsResolver = resolver
      c
    }

    override def getNamespaceResolver(): NamespaceResolver = nsResolver

    def convertString(input: CharSequence): ConversionResult = {
      if (getNamespaceResolver == null) {
        throw new UnsupportedOperationException(
          "Cannot validate a NOTATION without a namespace resolver")
      }
      try {
        val parts: Array[String] =
          NameChecker.getQNameParts(Whitespace.trimWhitespace(input))
        val uri: String = getNamespaceResolver.getURIForPrefix(parts(0), useDefault = true)
        if (uri == null) {
          return new ValidationFailure(
            "Namespace prefix " + Err
              .wrap(parts(0)) + " has not been declared")
        }
        if (!getConversionRules.isDeclaredNotation(uri, parts(1))) {
         return new ValidationFailure(
            "Notation {" + uri + "}" + parts(1) + " is not declared in the schema")
        }
        new NotationValue(parts(0), uri, parts(1), false)
      } catch {
        case err: QNameException =>
          new ValidationFailure("Invalid lexical QName " + Err.wrap(input))

        case err: XPathException => new ValidationFailure(err.getMessage)

      }
    }

  }

  class StringToAnyURI(rules: ConversionRules) extends StringConverter(rules) {

    def convertString(input: CharSequence): ConversionResult =
      if (getConversionRules.isValidURI(input)) {
        new AnyURIValue(input)
      } else {
        new ValidationFailure("Invalid URI: " + input)
      }

    /*@Nullable*/

    override def validate(input: CharSequence): ValidationFailure =
      if (getConversionRules.isValidURI(input)) {
        null
      } else {
        new ValidationFailure("Invalid URI: " + input)
      }

  }

  class StringToUnionConverter(var targetType: PlainType,
                               var rules: ConversionRules)
    extends StringConverter {

    if (!targetType.isPlainType) {
      throw new IllegalArgumentException
    }

    if (targetType.isNamespaceSensitive) {
      throw new IllegalArgumentException
    }

    override def convertString(input: CharSequence): ConversionResult =
      try targetType
        .asInstanceOf[UnionType]
        .getTypedValue(input, null, rules)
        .head
      catch {
        case err: ValidationException => err.getValidationFailure

      }

  }

}

/**
  * A {@link Converter} that accepts a string as input. This subclass of Converter is provided
  * to avoid having to wrap the string into a StringValue prior to conversion. Every Converter whose
  * source type is xs:string must be an instance of this subclass.
  * <p>The input to a StringConverter can also be an xs:untypedAtomic value, since the conversion semantics
  * are always the same as from a string.</p>
  * <p>A StringConverter also provides a method to validate that a string is valid against the target type,
  * without actually performing the conversion.</p>
  */
abstract class StringConverter  () extends Converter {

  def this(rules: ConversionRules) = {
    this()
    this.setConversionRules(rules)
  }

  def convertString(input: CharSequence): ConversionResult

  /*@Nullable*/

  def validate(input: CharSequence): ValidationFailure = {
    val result: ConversionResult = convertString(input)
    if (result.isInstanceOf[ValidationFailure])
      result.asInstanceOf[ValidationFailure]
    else null
  }

  override def convert(input: AtomicValue): ConversionResult =
    convertString(input.getStringValueCS)

}

