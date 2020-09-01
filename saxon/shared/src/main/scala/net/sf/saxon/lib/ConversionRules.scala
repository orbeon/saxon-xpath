////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.expr.sort.LRUCache
import net.sf.saxon.model._
import net.sf.saxon.om.{NotationSet, StandardNames}
import net.sf.saxon.value.StringToDouble11

import scala.beans.BooleanBeanProperty

//remove if not needed

object ConversionRules {

  /**
   * Default conversion rules. Changed in Saxon 9.9 so these are the XSD 1.1 rules (year zero allowed in dates,
   * {@code -INF} allowed in {@code xs:double}). Modifying the default conversion rules is inadvisable,
   * but it could potentially be done in order to retain compatibility with earlier Saxon releases.
   */
  /**
   * Default conversion rules. Changed in Saxon 9.9 so these are the XSD 1.1 rules (year zero allowed in dates,
   * {@code -INF} allowed in {@code xs:double}). Modifying the default conversion rules is inadvisable,
   * but it could potentially be done in order to retain compatibility with earlier Saxon releases.
   */
  val DEFAULT: ConversionRules = new ConversionRules()

}

class ConversionRules {

  private var stringToDouble: StringToDouble = StringToDouble11.getInstance

  // may be null
  private var notationSet: NotationSet = _

  private var uriChecker: URIChecker = _

  @BooleanBeanProperty
  var allowYearZero: Boolean = true

  // may be null
  private var typeHierarchy: TypeHierarchy = _

  // These two tables need to be synchronised to make the caching thread-safe
  private var converterCache: LRUCache[Integer, Converter] =
    new LRUCache(100, true)

  def copy(): ConversionRules = {
    val cr: ConversionRules = new ConversionRules()
    copyTo(cr)
    cr
  }

  def copyTo(cr: ConversionRules): Unit = {
    cr.stringToDouble = stringToDouble
    cr.notationSet = notationSet
    cr.uriChecker = uriChecker
    cr.allowYearZero = allowYearZero
    cr.typeHierarchy = typeHierarchy
    cr.converterCache.clear()
  }

  def setTypeHierarchy(typeHierarchy: TypeHierarchy): Unit = {
    this.typeHierarchy = typeHierarchy
  }

  def setStringToDoubleConverter(converter: StringToDouble): Unit = {
    this.stringToDouble = converter
  }

  def getStringToDoubleConverter: StringToDouble = stringToDouble

  def setNotationSet(notations: NotationSet): Unit = {
    this.notationSet = notations
  }

  def isDeclaredNotation(uri: String, local: String): Boolean = //noinspection SimplifiableIfStatement
    if (notationSet == null) {
      // in the absence of a known configuration, treat all notations as valid
      true
    } else {
      notationSet.isDeclaredNotation(uri, local)
    }

  def setURIChecker(checker: URIChecker): Unit = {
    this.uriChecker = checker
  }

  def isValidURI(string: CharSequence): Boolean =
    uriChecker == null || uriChecker.isValidURI(string)

  /*@Nullable*/

  def getConverter(source: AtomicType, target: AtomicType): Converter = {
    // Handle some common cases before looking in the cache
    if (source == target) {
      Converter.IdentityConverter.INSTANCE
    } else if (source == BuiltInAtomicType.STRING || source == BuiltInAtomicType.UNTYPED_ATOMIC) {
      target.getStringConverter(this)
    } else if (target == BuiltInAtomicType.STRING) {
      Converter.ToStringConverter.INSTANCE
    } else if (target == BuiltInAtomicType.UNTYPED_ATOMIC) {
      Converter.ToUntypedAtomicConverter.INSTANCE
    }
    // fingerprint of the target type (20 bits)
    val key: Int = (source.getPrimitiveType << 20) | target.getFingerprint
    var converter: Converter = converterCache.get(key)
    if (converter == null) {
      converter = makeConverter(source, target)
      if (converter != null) {
        converterCache.put(key, converter)
      } else {
        return null
      }
    }
    converter
  }

  // For a lookup key, use the primitive type of the source type (always 10 bits) and the
  // For a lookup key, use the primitive type of the source type (always 10 bits) and the

  /*@Nullable*/

  private def makeConverter(sourceType: AtomicType,
                            targetType: AtomicType): Converter = {
    if (sourceType == targetType) {
      Converter.IdentityConverter.INSTANCE
    }
    val tt: Int = targetType.getFingerprint
    val tp: Int = targetType.getPrimitiveType
    val st: Int = sourceType.getPrimitiveType
    if ((st == StandardNames.XS_STRING || st == StandardNames.XS_UNTYPED_ATOMIC) &&
      (tp == StandardNames.XS_STRING || tp == StandardNames.XS_UNTYPED_ATOMIC)) {
      makeStringConverter(targetType)
    }
    if (!targetType.isPrimitiveType) {
      val primTarget: AtomicType = targetType.getPrimitiveItemType
      if (sourceType == primTarget) {
        new Converter.DownCastingConverter(targetType, this)
      } else if (st == StandardNames.XS_STRING || st == StandardNames.XS_UNTYPED_ATOMIC) {
        makeStringConverter(targetType)
      } else {
        val stageOne: Converter = makeConverter(sourceType, primTarget)
        if (stageOne == null) {
          return null
        }
        val stageTwo: Converter =
          new Converter.DownCastingConverter(targetType, this)
        new Converter.TwoPhaseConverter(stageOne, stageTwo)
      }
    }
    if (st == tt) {
      // we are casting between subtypes of the same primitive type.
      if (typeHierarchy != null && typeHierarchy.isSubType(sourceType,
        targetType)) {
        new Converter.UpCastingConverter(targetType)
      }
      val upcast: Converter =
        new Converter.UpCastingConverter(sourceType.getPrimitiveItemType)
      val downcast: Converter =
        new Converter.DownCastingConverter(targetType, this)
      new Converter.TwoPhaseConverter(upcast, downcast)
    }
    tt match {
      case StandardNames.XS_UNTYPED_ATOMIC =>
        Converter.ToUntypedAtomicConverter.INSTANCE
      case StandardNames.XS_STRING => Converter.ToStringConverter.INSTANCE
      case StandardNames.XS_FLOAT =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToFloat(this)
          case StandardNames.XS_DOUBLE | StandardNames.XS_DECIMAL |
               StandardNames.XS_INTEGER | StandardNames.XS_NUMERIC =>
            Converter.NumericToFloat.INSTANCE
          case StandardNames.XS_BOOLEAN => Converter.BooleanToFloat.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DOUBLE | StandardNames.XS_NUMERIC =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            stringToDouble
          case StandardNames.XS_FLOAT | StandardNames.XS_DECIMAL |
               StandardNames.XS_INTEGER | StandardNames.XS_NUMERIC =>
            Converter.NumericToDouble.INSTANCE
          case StandardNames.XS_BOOLEAN => Converter.BooleanToDouble.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DECIMAL =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToDecimal.INSTANCE
          case StandardNames.XS_FLOAT => Converter.FloatToDecimal.INSTANCE
          case StandardNames.XS_DOUBLE => Converter.DoubleToDecimal.INSTANCE
          case StandardNames.XS_INTEGER => Converter.IntegerToDecimal.INSTANCE
          case StandardNames.XS_NUMERIC => Converter.NumericToDecimal.INSTANCE
          case StandardNames.XS_BOOLEAN => Converter.BooleanToDecimal.INSTANCE
          case _ => null

        }
      case StandardNames.XS_INTEGER =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToInteger.INSTANCE
          case StandardNames.XS_FLOAT => Converter.FloatToInteger.INSTANCE
          case StandardNames.XS_DOUBLE => Converter.DoubleToInteger.INSTANCE
          case StandardNames.XS_DECIMAL => Converter.DecimalToInteger.INSTANCE
          case StandardNames.XS_NUMERIC => Converter.NumericToInteger.INSTANCE
          case StandardNames.XS_BOOLEAN => Converter.BooleanToInteger.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DURATION =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToDuration.INSTANCE
          case StandardNames.XS_DAY_TIME_DURATION |
               StandardNames.XS_YEAR_MONTH_DURATION =>
            new Converter.UpCastingConverter(BuiltInAtomicType.DURATION)
          case _ => null

        }
      case StandardNames.XS_YEAR_MONTH_DURATION =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToYearMonthDuration.INSTANCE
          case StandardNames.XS_DURATION |
               StandardNames.XS_DAY_TIME_DURATION =>
            Converter.DurationToYearMonthDuration.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DAY_TIME_DURATION =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToDayTimeDuration.INSTANCE
          case StandardNames.XS_DURATION |
               StandardNames.XS_YEAR_MONTH_DURATION =>
            Converter.DurationToDayTimeDuration.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DATE_TIME =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToDateTime(this)
          case StandardNames.XS_DATE => Converter.DateToDateTime.INSTANCE
          case _ => null

        }
      case StandardNames.XS_TIME =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToTime.INSTANCE
          case StandardNames.XS_DATE_TIME => Converter.DateTimeToTime.INSTANCE
          case _ => null

        }
      case StandardNames.XS_DATE =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToDate(this)
          case StandardNames.XS_DATE_TIME => Converter.DateTimeToDate.INSTANCE
          case _ => null

        }
      case StandardNames.XS_G_YEAR_MONTH =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToGYearMonth(this)
          case StandardNames.XS_DATE =>
            Converter.TwoPhaseConverter.makeTwoPhaseConverter(
              BuiltInAtomicType.DATE,
              BuiltInAtomicType.DATE_TIME,
              BuiltInAtomicType.G_YEAR_MONTH,
              this)
          case StandardNames.XS_DATE_TIME =>
            Converter.DateTimeToGYearMonth.INSTANCE
          case _ => null

        }
      case StandardNames.XS_G_YEAR =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToGYear(this)
          case StandardNames.XS_DATE =>
            Converter.TwoPhaseConverter.makeTwoPhaseConverter(
              BuiltInAtomicType.DATE,
              BuiltInAtomicType.DATE_TIME,
              BuiltInAtomicType.G_YEAR,
              this)
          case StandardNames.XS_DATE_TIME => Converter.DateTimeToGYear.INSTANCE
          case _ => null

        }
      case StandardNames.XS_G_MONTH_DAY =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToGMonthDay.INSTANCE
          case StandardNames.XS_DATE =>
            Converter.TwoPhaseConverter.makeTwoPhaseConverter(
              BuiltInAtomicType.DATE,
              BuiltInAtomicType.DATE_TIME,
              BuiltInAtomicType.G_MONTH_DAY,
              this)
          case StandardNames.XS_DATE_TIME =>
            Converter.DateTimeToGMonthDay.INSTANCE
          case _ => null

        }
      case StandardNames.XS_G_DAY =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToGDay.INSTANCE
          case StandardNames.XS_DATE =>
            Converter.TwoPhaseConverter.makeTwoPhaseConverter(
              BuiltInAtomicType.DATE,
              BuiltInAtomicType.DATE_TIME,
              BuiltInAtomicType.G_DAY,
              this)
          case StandardNames.XS_DATE_TIME => Converter.DateTimeToGDay.INSTANCE
          case _ => null

        }
      case StandardNames.XS_G_MONTH =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToGMonth.INSTANCE
          case StandardNames.XS_DATE =>
            Converter.TwoPhaseConverter.makeTwoPhaseConverter(
              BuiltInAtomicType.DATE,
              BuiltInAtomicType.DATE_TIME,
              BuiltInAtomicType.G_MONTH,
              this)
          case StandardNames.XS_DATE_TIME =>
            Converter.DateTimeToGMonth.INSTANCE
          case _ => null

        }
      case StandardNames.XS_BOOLEAN =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToBoolean.INSTANCE
          case StandardNames.XS_FLOAT | StandardNames.XS_DOUBLE |
               StandardNames.XS_DECIMAL | StandardNames.XS_INTEGER |
               StandardNames.XS_NUMERIC =>
            Converter.NumericToBoolean.INSTANCE
          case _ => null

        }
      case StandardNames.XS_BASE64_BINARY =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToBase64Binary.INSTANCE
          case StandardNames.XS_HEX_BINARY =>
            Converter.HexBinaryToBase64Binary.INSTANCE
          case _ => null

        }
      case StandardNames.XS_HEX_BINARY =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            StringConverter.StringToHexBinary.INSTANCE
          case StandardNames.XS_BASE64_BINARY =>
            Converter.Base64BinaryToHexBinary.INSTANCE
          case _ => null

        }
      case StandardNames.XS_ANY_URI =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToAnyURI(this)
          case _ => null

        }
      case StandardNames.XS_QNAME =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToQName(this)
          case StandardNames.XS_NOTATION => Converter.NotationToQName.INSTANCE
          case _ => null

        }
      case StandardNames.XS_NOTATION =>
        st match {
          case StandardNames.XS_UNTYPED_ATOMIC | StandardNames.XS_STRING =>
            new StringConverter.StringToNotation(this)
          case StandardNames.XS_QNAME => Converter.QNameToNotation.INSTANCE
          case _ => null

        }
      case StandardNames.XS_ANY_ATOMIC_TYPE =>
        Converter.IdentityConverter.INSTANCE
      case _ =>
        throw new IllegalArgumentException("Unknown primitive type " + tt)

    }
  }

  /*@NotNull*/

  def makeStringConverter(targetType: AtomicType): StringConverter = {
    val tt: Int = targetType.getPrimitiveType
    if (targetType.isBuiltInType) {
      if (tt == StandardNames.XS_STRING) {
        targetType.getFingerprint match {
          case StandardNames.XS_STRING =>
            StringConverter.StringToString.INSTANCE
          case StandardNames.XS_NORMALIZED_STRING =>
            StringConverter.StringToNormalizedString.INSTANCE
          case StandardNames.XS_TOKEN => StringConverter.StringToToken.INSTANCE
          case StandardNames.XS_LANGUAGE =>
            StringConverter.StringToLanguage.INSTANCE
          case StandardNames.XS_NAME => StringConverter.StringToName.INSTANCE
          case StandardNames.XS_NCNAME =>
            StringConverter.StringToNCName.TO_NCNAME
          case StandardNames.XS_ID => StringConverter.StringToNCName.TO_ID
          case StandardNames.XS_IDREF =>
            StringConverter.StringToNCName.TO_IDREF
          case StandardNames.XS_ENTITY =>
            StringConverter.StringToNCName.TO_ENTITY
          case StandardNames.XS_NMTOKEN =>
            StringConverter.StringToNMTOKEN.INSTANCE
          case _ =>
            throw new AssertionError("Unknown built-in subtype of xs:string")

        }
      } else if (tt == StandardNames.XS_UNTYPED_ATOMIC) {
        StringConverter.StringToUntypedAtomic.INSTANCE
      } else if (targetType.isPrimitiveType) {
        // converter to built-in types unrelated to xs:string
        val converter: Converter =
          getConverter(BuiltInAtomicType.STRING, targetType)
        assert(converter != null)
        converter.asInstanceOf[StringConverter]
      } else if (tt == StandardNames.XS_INTEGER) {
        new StringConverter.StringToIntegerSubtype(
          targetType.asInstanceOf[BuiltInAtomicType])
      } else {
        targetType.getFingerprint match {
          case StandardNames.XS_DAY_TIME_DURATION =>
            StringConverter.StringToDayTimeDuration.INSTANCE
          case StandardNames.XS_YEAR_MONTH_DURATION =>
            StringConverter.StringToYearMonthDuration.INSTANCE
          case StandardNames.XS_DATE_TIME_STAMP =>
            var first: StringConverter =
              new StringConverter.StringToDateTime(this)
            var second: Converter.DownCastingConverter =
              new Converter.DownCastingConverter(targetType, this)
            new StringConverter.StringToNonStringDerivedType(first, second)
          case _ =>
            throw new AssertionError("Unknown built in type " + targetType)

        }
      }
    } else {
      if (tt == StandardNames.XS_STRING) {
        if (targetType.getBuiltInBaseType == BuiltInAtomicType.STRING) {
          // converter to user-defined subtypes of xs:string
          new StringConverter.StringToStringSubtype(this, targetType)
        } else {
          // converter to user-defined subtypes of built-in subtypes of xs:string
          new StringConverter.StringToDerivedStringSubtype(this, targetType)
        }
      } else {
        // converter to user-defined types derived from types other than xs:string
        val first: StringConverter =
          targetType.getPrimitiveItemType.getStringConverter(this)
        val second: Converter.DownCastingConverter =
          new Converter.DownCastingConverter(targetType, this)
        new StringConverter.StringToNonStringDerivedType(first, second)
      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class defines a set of rules for converting between different atomic types. It handles the variations
 * that arise between different versions of the W3C specifications, for example the changes in Name syntax
 * between XML 1.0 and XML 1.1, the introduction of "+INF" as a permitted xs:double value in XSD 1.1, and so on.
 * <p>It is possible to nominate a customized <code>ConversionRules</code> object at the level of the
 * {@link net.sf.saxon.Configuration}, either by instantiating this class and changing the properties, or
 * by subclassing.</p>
 *
 * @see net.sf.saxon.Configuration#setConversionRules(ConversionRules)
 * @since 9.3
 */
