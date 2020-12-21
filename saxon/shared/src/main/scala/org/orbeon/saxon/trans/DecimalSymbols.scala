package org.orbeon.saxon.trans

import java.util.Arrays

import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.DecimalSymbols._
import org.orbeon.saxon.z.IntHashMap

object DecimalSymbols {

  val DECIMAL_SEPARATOR: Int = 0
  val GROUPING_SEPARATOR: Int = 1
  val DIGIT: Int = 2
  val MINUS_SIGN: Int = 3
  val PERCENT: Int = 4
  val PER_MILLE: Int = 5
  val ZERO_DIGIT: Int = 6
  val EXPONENT_SEPARATOR: Int = 7
  val PATTERN_SEPARATOR: Int = 8
  val INFINITY: Int = 9
  val NAN: Int = 10

  private val ERR_NOT_SINGLE_CHAR: Int = 0
  private val ERR_NOT_UNICODE_DIGIT: Int = 1
  private val ERR_SAME_CHAR_IN_TWO_ROLES: Int = 2
  private val ERR_TWO_VALUES_FOR_SAME_PROPERTY: Int = 3

  private var XSLT_CODES: Array[String] =
    Array("XTSE0020", "XTSE1295", "XTSE1300", "XTSE1290")

  private var XQUERY_CODES: Array[String] =
    Array("XQST0097", "XQST0097", "XQST0098", "XQST0114")

  val propertyNames: Array[String] = Array(
    "decimal-separator",
    "grouping-separator",
    "digit",
    "minus-sign",
    "percent",
    "per-mille",
    "zero-digit",
    "exponent-separator",
    "pattern-separator",
    "infinity",
    "NaN"
  )

  def isValidZeroDigit(zeroDigit: Int): Boolean =
    Arrays.binarySearch(zeroDigits, zeroDigit) >= 0

  var zeroDigits: Array[Int] = Array(0x0030, 0x0660, 0x06f0, 0x0966, 0x09e6,
    0x0a66, 0x0ae6, 0x0b66, 0x0be6, 0x0c66, 0x0ce6, 0x0d66, 0x0e50, 0x0ed0,
    0x0f20, 0x1040, 0x17e0, 0x1810, 0x1946, 0x19d0, 0xff10, 0x104a0, 0x1d7ce,
    0x1d7d8, 0x1d7e2, 0x1d7ec, 0x1d7f6)

}

class DecimalSymbols(language: HostLanguage.HostLanguage, languageLevel: Int) {

  private var errorCodes: Array[String] = XSLT_CODES

  private var infinityValue: String = "Infinity"

  private var NaNValue: String = "NaN"

  private var intValues: Array[Int] = new Array[Int](propertyNames.length - 2)

  private var precedences: Array[Int] = new Array[Int](propertyNames.length)

  private var inconsistent: Array[Boolean] =
    new Array[Boolean](propertyNames.length)

  intValues(DECIMAL_SEPARATOR) = '.'
  intValues(GROUPING_SEPARATOR) = ','
  intValues(DIGIT) = '#'
  intValues(MINUS_SIGN) = '-'
  intValues(PERCENT) = '%'
  intValues(PER_MILLE) = '‰'
  intValues(ZERO_DIGIT) = '0'
  intValues(EXPONENT_SEPARATOR) = 'e'
  intValues(PATTERN_SEPARATOR) = ';'

  Arrays.fill(precedences, java.lang.Integer.MIN_VALUE)

  setHostLanguage(language, languageLevel)

  def setHostLanguage(language: HostLanguage.HostLanguage, languageLevel: Int): Unit = {
    errorCodes =
      if (language == HostLanguage.XQUERY) XQUERY_CODES else XSLT_CODES
  }

  def getDecimalSeparator: Int = intValues(DECIMAL_SEPARATOR)

  def getGroupingSeparator: Int = intValues(GROUPING_SEPARATOR)

  def getDigit: Int = intValues(DIGIT)

  def getMinusSign: Int = intValues(MINUS_SIGN)

  def getPercent: Int = intValues(PERCENT)

  def getPerMille: Int = intValues(PER_MILLE)

  def getZeroDigit: Int = intValues(ZERO_DIGIT)

  def getExponentSeparator: Int = intValues(EXPONENT_SEPARATOR)

  def getPatternSeparator: Int = intValues(PATTERN_SEPARATOR)

  def getInfinity: String = infinityValue

  def getNaN: String = NaNValue

  def setDecimalSeparator(value: String): Unit = {
    setProperty(DECIMAL_SEPARATOR, value, 0)
  }

  def setGroupingSeparator(value: String): Unit = {
    setProperty(GROUPING_SEPARATOR, value, 0)
  }

  def setDigit(value: String): Unit = {
    setProperty(DIGIT, value, 0)
  }

  def setMinusSign(value: String): Unit = {
    setProperty(MINUS_SIGN, value, 0)
  }

  def setPercent(value: String): Unit = {
    setProperty(PERCENT, value, 0)
  }

  def setPerMille(value: String): Unit = {
    setProperty(PER_MILLE, value, 0)
  }

  def setZeroDigit(value: String): Unit = {
    setProperty(ZERO_DIGIT, value, 0)
  }

  def setExponentSeparator(value: String): Unit = {
    setProperty(EXPONENT_SEPARATOR, value, 0)
  }

  def setPatternSeparator(value: String): Unit = {
    setProperty(PATTERN_SEPARATOR, value, 0)
  }

  def setInfinity(value: String): Unit = {
    setProperty(INFINITY, value, 0)
  }

  def setNaN(value: String): Unit = {
    setProperty(NAN, value, 0)
  }

  def setProperty(key: Int, value: String, precedence: Int): Unit = {
    val name: String = propertyNames(key)
    if (key <= PATTERN_SEPARATOR) {
      val intValue: Int = singleChar(name, value)
      if (precedence > precedences(key)) {
        intValues(key) = intValue
        precedences(key) = precedence
        inconsistent(key) = false
      } else if (precedence == precedences(key)) {
        if (intValue != intValues(key)) {
          inconsistent(key) = true
        }
      } else {}
      if (key == ZERO_DIGIT && !isValidZeroDigit(intValue)) {
        throw new XPathException(
          "The value of the zero-digit attribute must be a Unicode digit with value zero",
          errorCodes(ERR_NOT_UNICODE_DIGIT))
      }
    } else if (key == INFINITY) {
      if (precedence > precedences(key)) {
        infinityValue = value
        precedences(key) = precedence
        inconsistent(key) = false
      } else if (precedence == precedences(key)) {
        if (infinityValue != value) {
          inconsistent(key) = true
        }
      }
    } else if (key == NAN) {
      if (precedence > precedences(key)) {
        NaNValue = value
        precedences(key) = precedence
        inconsistent(key) = false
      } else if (precedence == precedences(key)) {
        if (NaNValue != value) {
          inconsistent(key) = false
        }
      }
    } else {
      throw new IllegalArgumentException()
    }
  }

  def setIntProperty(name: String, value: Int): Unit = {
    for (i <- 0 until propertyNames.length if propertyNames(i) == name) {
      intValues(i) = value
    }
  }

  def export(name: StructuredQName, out: ExpressionPresenter): Unit = {
    val defaultSymbols: DecimalSymbols =
      new DecimalSymbols(HostLanguage.XSLT, 31)
    out.startElement("decimalFormat")
    if (name != null) {
      out.emitAttribute("name", name)
    }
    for (i <- 0 until intValues.length) {
      val propValue: Int = intValues(i)
      if (propValue != defaultSymbols.intValues(i)) {
        out.emitAttribute(propertyNames(i), propValue.toString)
      }
    }
    if ("Infinity" != getInfinity) {
      out.emitAttribute("infinity", getInfinity)
    }
    if ("NaN" != getNaN) {
      out.emitAttribute("NaN", getNaN)
    }
    out.endElement()
  }

  private def singleChar(name: String, value: String): Int = {
    val us: UnicodeString = UnicodeString.makeUnicodeString(value)
    if (us.uLength != 1) {
      val err = new XPathException(
        "Attribute " + name + " should be a single character",
        errorCodes(ERR_NOT_SINGLE_CHAR))
      err.setIsStaticError(true)
      throw err
    }
    us.uCharAt(0)
  }

  def checkConsistency(name: StructuredQName): Unit = {
    for (i <- 0.until(10) if inconsistent(i)) {
      val err = new XPathException(
        "Inconsistency in " +
          (if (name == null) "unnamed decimal format. "
          else "decimal format " + name.getDisplayName + ". ") +
          "There are two inconsistent values for decimal-format property " +
          propertyNames(i) +
          " at the same import precedence")
      err.setErrorCode(errorCodes(ERR_TWO_VALUES_FOR_SAME_PROPERTY))
      err.setIsStaticError(true)
      throw err
    }
    val map: IntHashMap[String] = new IntHashMap[String](20)
    map.put(getDecimalSeparator, "decimal-separator")
    if (map.get(getGroupingSeparator) != null) {
      duplicate("grouping-separator", map.get(getGroupingSeparator), name)
    }
    map.put(getGroupingSeparator, "grouping-separator")
    if (map.get(getPercent) != null) {
      duplicate("percent", map.get(getPercent), name)
    }
    map.put(getPercent, "percent")
    if (map.get(getPerMille) != null) {
      duplicate("per-mille", map.get(getPerMille), name)
    }
    map.put(getPerMille, "per-mille")
    if (map.get(getDigit) != null) {
      duplicate("digit", map.get(getDigit), name)
    }
    map.put(getDigit, "digit")
    if (map.get(getPatternSeparator) != null) {
      duplicate("pattern-separator", map.get(getPatternSeparator), name)
    }
    map.put(getPatternSeparator, "pattern-separator")
    if (map.get(getExponentSeparator) != null) {
      duplicate("exponent-separator", map.get(getExponentSeparator), name)
    }
    map.put(getExponentSeparator, "exponent-separator")
    val zero: Int = getZeroDigit
    for (i <- zero until zero + 10 if map.get(i) != null) {
      val err = new XPathException(
        "Inconsistent properties in " +
          (if (name == null) "unnamed decimal format. "
          else "decimal format " + name.getDisplayName + ". ") +
          "The same character is used as digit " +
          (i - zero) +
          " in the chosen digit family, and as the " +
          map.get(i))
      err.setErrorCode(errorCodes(ERR_SAME_CHAR_IN_TWO_ROLES))
      throw err
    }
  }

  private def duplicate(role1: String,
                        role2: String,
                        name: StructuredQName): Unit = {
    val err = new XPathException(
      "Inconsistent properties in " +
        (if (name == null) "unnamed decimal format. "
        else "decimal format " + name.getDisplayName + ". ") +
        "The same character is used as the " +
        role1 +
        " and as the " +
        role2)
    err.setErrorCode(errorCodes(ERR_SAME_CHAR_IN_TWO_ROLES))
    throw err
  }

  override def equals(obj: Any): Boolean = {

    if (! obj.isInstanceOf[DecimalSymbols])
      return false

    val o: DecimalSymbols = obj.asInstanceOf[DecimalSymbols]
    getDecimalSeparator == o.getDecimalSeparator && getGroupingSeparator == o.getGroupingSeparator &&
      getDigit == o.getDigit &&
      getMinusSign == o.getMinusSign &&
      getPercent == o.getPercent &&
      getPerMille == o.getPerMille &&
      getZeroDigit == o.getZeroDigit &&
      getPatternSeparator == o.getPatternSeparator &&
      getInfinity == o.getInfinity &&
      getNaN == o.getNaN
  }

  override def hashCode: Int =
    getDecimalSeparator + (37 * getGroupingSeparator) + (41 * getDigit)
}
