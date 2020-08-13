////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.model._
import net.sf.saxon.om.SequenceTool
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode
import java.util.regex.Pattern
import scala.util.control.Breaks._

/**
 * An implementation class for decimal values other than integers
 *
 * @since 9.8. This class was previously named "DecimalValue". In 9.8 a new DecimalValue
 *        class is introduced, to more faithfully reflect the XDM type hierarchy, so that every
 *        instance of xs:decimal is now implemented as an instance of DecimalValue.
 */
object BigDecimalValue {
  val DIVIDE_PRECISION = 18
  val BIG_DECIMAL_ONE_MILLION = BigDecimal.valueOf(1000000)
  val BIG_DECIMAL_ONE_BILLION = BigDecimal.valueOf(1000000000)
  val ZERO = new BigDecimalValue(BigDecimal.valueOf(0))
  val ONE = new BigDecimalValue(BigDecimal.valueOf(1))
  val TWO = new BigDecimalValue(BigDecimal.valueOf(2))
  val THREE = new BigDecimalValue(BigDecimal.valueOf(3))
  val MAX_INT = BigDecimal.valueOf(Integer.MAX_VALUE)
  private val decimalPattern = Pattern.compile("(\\-|\\+)?((\\.[0-9]+)|([0-9]+(\\.[0-9]*)?))")

  /**
   * Factory method to construct a DecimalValue from a string
   *
   * @param in       the value of the DecimalValue
   * @param validate true if validation is required; false if the caller knows that the value is valid
   * @return the required DecimalValue if the input is valid, or a ValidationFailure encapsulating the error
   *         message if not.
   */
  def makeDecimalValue(in: CharSequence, validate: Boolean) =
    try parse(in)
    catch {
      case err: NumberFormatException =>
        val e = new ValidationFailure("Cannot convert string " + Err.wrap(Whitespace.trim(in), Err.VALUE) + " to xs:decimal: " + err.getMessage)
        e.setErrorCode("FORG0001")
        e
    }

  /**
   * Factory method to construct a DecimalValue from a string, throwing an unchecked exception
   * if the value is invalid
   *
   * @param in the lexical representation of the DecimalValue
   * @return the required DecimalValue
   * @throws NumberFormatException if the value is invalid
   */
  @throws[NumberFormatException]
  def parse(in: CharSequence): BigDecimalValue = {
    val digits = new FastStringBuffer(in.length)
    var scale = 0
    var state = 0
    // 0 - in initial whitespace; 1 - after sign
    // 3 - after decimal point; 5 - in final whitespace
    var foundDigit = false
    val len = in.length

    for (i <- 0 until len) {
      val c = in.charAt(i)
      c match {
        case ' ' =>
        case '\t' =>
        case '\r' =>
        case '\n' =>
          if (state != 0) state = 5
        case '+' =>
          if (state != 0) throw new NumberFormatException("unexpected sign")
          state = 1
        case '-' =>
          if (state != 0) throw new NumberFormatException("unexpected sign")
          state = 1
          digits.cat(c)
        case '0' =>
        case '1' =>
        case '2' =>
        case '3' =>
        case '4' =>
        case '5' =>
        case '6' =>
        case '7' =>
        case '8' =>
        case '9' =>
          if (state == 0) state = 1
          else if (state >= 3) scale += 1
          if (state == 5) throw new NumberFormatException("contains embedded whitespace")
          digits.cat(c)
          foundDigit = true
        case '.' =>
          if (state == 5) throw new NumberFormatException("contains embedded whitespace")
          if (state >= 3) throw new NumberFormatException("more than one decimal point")
          state = 3
        case _ =>
          throw new NumberFormatException("invalid character '" + c + "'")
      }
    }
    if (!foundDigit) throw new NumberFormatException("no digits in value")

    breakable {
      while (scale > 0) if (digits.charAt(digits.length - 1) == '0') {
        digits.setLength(digits.length - 1)
        scale -= 1
      }
      else break
    }
    if (digits.isEmpty || (digits.length == 1 && digits.charAt(0) == '-')) return BigDecimalValue.ZERO
    val bigInt = new BigInteger(digits.toString)
    val bigDec = new BigDecimal(bigInt, scale)
    new BigDecimalValue(bigDec)
  }

  /**
   * Test whether a string is castable to a decimal value
   *
   * @param in the string to be tested
   * @return true if the string has the correct format for a decimal
   */
  def castableAsDecimal(in: CharSequence) = {
    val trimmed = Whitespace.trimWhitespace(in)
    decimalPattern.matcher(trimmed).matches
  }

  /**
   * Convert a decimal value to a string, using the XPath rules for formatting
   *
   * @param value the decimal value to be converted
   * @param fsb   the FastStringBuffer to which the value is to be appended
   * @return the supplied FastStringBuffer, suitably populated
   */
  def decimalToString(value: BigDecimal, fsb: FastStringBuffer): FastStringBuffer = { // Can't use BigDecimal#toString() under JDK 1.5 because this produces values like "1E-5".
    // Can't use BigDecimal#toPlainString() because it retains trailing zeroes to represent the scale
    val scale = value.scale
    if (scale == 0) {
      fsb.append(value.toString)
      fsb
    }
    else if (scale < 0) {
      val s = value.abs.unscaledValue.toString
      if (s == "0") {
        fsb.cat('0')
        return fsb
      }
      //FastStringBuffer sb = new FastStringBuffer(s.length() + (-scale) + 2);
      if (value.signum < 0) fsb.cat('-')
      fsb.append(s)
      for (i <- 0 until -scale) {
        fsb.cat('0')
      }
      fsb
    }
    else {
      val s = value.abs.unscaledValue.toString
      if (s == "0") {
        fsb.cat('0')
        return fsb
      }
      val len = s.length
      //FastStringBuffer sb = new FastStringBuffer(len+1);
      if (value.signum < 0) fsb.cat('-')
      if (scale >= len) {
        fsb.append("0.")
        for (i <- len until scale) {
          fsb.cat('0')
        }
        fsb.append(s)
      }
      else {
        fsb.append(s.substring(0, len - scale))
        fsb.cat('.')
        fsb.append(s.substring(len - scale))
      }
      fsb
    }
  }

  class DecimalComparable(var value: BigDecimalValue) extends Comparable[AnyRef] {
    def asBigDecimal: BigDecimal = value.getDecimalValue

    override def compareTo(o: AnyRef): Int = if (o.isInstanceOf[BigDecimalValue.DecimalComparable])
      asBigDecimal.compareTo(o.asInstanceOf[BigDecimalValue.DecimalComparable].asBigDecimal)
    else if (o.isInstanceOf[Int64Value.Int64Comparable])
      asBigDecimal.compareTo(BigDecimal.valueOf(o.asInstanceOf[Int64Value.Int64Comparable].asLong))
    else if (o.isInstanceOf[BigIntegerValue.BigIntegerComparable])
      asBigDecimal.compareTo(new BigDecimal(o.asInstanceOf[BigIntegerValue.BigIntegerComparable].asBigInteger))
    else SequenceTool.INDETERMINATE_ORDERING

    override def equals(o: Any) = compareTo(o.asInstanceOf[AnyRef]) == 0

    override def hashCode: Int = {
      if (value.isWholeNumber) {
        val iv = Converter.DecimalToInteger.INSTANCE.convert(value)
        return iv.getSchemaComparable.hashCode
      }
      value.hashCode
    }
  }

}

final class BigDecimalValue extends DecimalValue {
  private var value: BigDecimal = null
  private var doubleValue = .0

  /**
   * Constructor supplying a BigDecimal
   *
   * @param value the value of the DecimalValue
   */
  def this(value: BigDecimal) {
    this()
    this.value = value.stripTrailingZeros
    typeLabel = BuiltInAtomicType.DECIMAL
  }

  /**
   * Constructor supplying a double
   *
   * @param in the value of the DecimalValue
   * @throws ValidationException if the supplied value cannot be converted, typically because it is INF or NaN.
   */
  def this(in: Double) = {
    this()
    try {
      val d = new BigDecimal(in)
      value = d.stripTrailingZeros
    } catch {
      case err: NumberFormatException =>
        // Must be a special value such as NaN or infinity
        val e = new ValidationFailure("Cannot convert double " + Err.wrap(in + "", Err.VALUE) + " to decimal")
        e.setErrorCode("FOCA0002")
        throw e.makeException
    }
    typeLabel = BuiltInAtomicType.DECIMAL
  }

  /**
   * Constructor supplying a long integer
   *
   * @param in the value of the DecimalValue
   */
  def this(in: Long) = {
    this()
    value = BigDecimal.valueOf(in)
    typeLabel = BuiltInAtomicType.DECIMAL
  }

  /**
   * Create a copy of this atomic value, with a different type label
   *
   * @param typeLabel the type label of the new copy. The caller is responsible for checking that
   *                  the value actually conforms to this type.
   */
  override def copyAsSubType(typeLabel: AtomicType) = {
    val v = new BigDecimalValue(value)
    v.typeLabel = typeLabel
    v
  }

  /**
   * Determine the primitive type of the value. This delivers the same answer as
   * getItemType().getPrimitiveItemType(). The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration,
   * and xs:untypedAtomic. For external objects, the result is AnyAtomicType.
   */
  override def getPrimitiveType = BuiltInAtomicType.DECIMAL

  /**
   * Get the numeric value as a double
   *
   * @return A double representing this numeric value; NaN if it cannot be
   *         converted
   */
  override def getDoubleValue = if (doubleValue == null) {
    val d = value.doubleValue
    doubleValue = d
    d
  }
  else doubleValue

  /**
   * Get the numeric value converted to a float
   *
   * @return a float representing this numeric value; NaN if it cannot be converted
   */
  override def getFloatValue = value.doubleValue.toFloat

  @throws[XPathException]
  override def longValue = value.doubleValue.toLong

  /**
   * Get the value
   */
  override def getDecimalValue: BigDecimal = value

  /**
   * Get the hashCode. This must conform to the rules for other NumericValue hashcodes
   *
   * @see NumericValue#hashCode
   */
  override def hashCode: Int = {
    val round = value.setScale(0, RoundingMode.DOWN)
    val longValue = round.longValue
    if (longValue > Integer.MIN_VALUE && longValue < Integer.MAX_VALUE) longValue.toInt
    else getDoubleValue.hashCode
  }

  override def effectiveBooleanValue = value.signum != 0

  /**
   * Get the canonical lexical representation as defined in XML Schema. This is not always the same
   * as the result of casting to a string according to the XPath rules. For xs:decimal, the canonical
   * representation always contains a decimal point.
   */
  override def getCanonicalLexicalRepresentation = {
    var s = getStringValue
    if (s.indexOf('.') < 0) s += ".0"
    s
  }

  /**
   * Get the value as a String
   *
   * @return a String representation of the value
   */
  /*@NotNull*/ override def getPrimitiveStringValue = BigDecimalValue.decimalToString(value, new FastStringBuffer(FastStringBuffer.C16))

  /**
   * Negate the value
   */
  override def negate = new BigDecimalValue(value.negate)

  /**
   * Implement the XPath floor() function
   */
  override def floor = new BigDecimalValue(value.setScale(0, RoundingMode.FLOOR))

  /**
   * Implement the XPath ceiling() function
   */
  override def ceiling = new BigDecimalValue(value.setScale(0, RoundingMode.CEILING))

  /**
   * Implement the XPath round() function
   */
  override def round(scale: Int): NumericValue = {
    if (scale >= value.scale) {
      return this
    }

    value.signum.toString match {
      case "-1" => new BigDecimalValue(value.setScale(scale, RoundingMode.HALF_DOWN))
      case "-0" => this
      case "+1" => new BigDecimalValue(value.setScale(scale, RoundingMode.HALF_UP))
      case _ => this
    }
  }

  /**
   * Implement the XPath round-half-to-even() function
   */
  override def roundHalfToEven(scale: Int): NumericValue = {
    if (scale >= value.scale) return this
    val scaledValue = value.setScale(scale, RoundingMode.HALF_EVEN)
    new BigDecimalValue(scaledValue.stripTrailingZeros)
  }

  /**
   * Determine whether the value is negative, zero, or positive
   *
   * @return -1 if negative, 0 if zero, +1 if positive, NaN if NaN
   */
  override def signum = value.signum

  /**
   * Determine whether the value is a whole number, that is, whether it compares
   * equal to some integer
   */
  override def isWholeNumber: Boolean = value.scale == 0 || value.compareTo(value.setScale(0, RoundingMode.DOWN)) == 0

  /**
   * Test whether a number is a possible subscript into a sequence, that is,
   * a whole number greater than zero and less than 2^31
   *
   *
   * @return the number as an int if it is a possible subscript, or -1 otherwise
   */
  override def asSubscript = if (isWholeNumber && value.compareTo(BigDecimal.ZERO) > 0 && value.compareTo(BigDecimalValue.MAX_INT) <= 0) try longValue.toInt
  catch {
    case e: XPathException =>
      -1
  }
  else -1

  /**
   * Get the absolute value as defined by the XPath abs() function
   *
   * @return the absolute value
   * @since 9.2
   */
  override def abs = if (value.signum > 0) this
  else new BigDecimalValue(value.negate)

  /**
   * Compare the value to another numeric value
   */
  override def compareTo(other: NumericValue): Int = if (NumericValue.isInteger(other)) { // deliberately triggers a ClassCastException if other value is the wrong type
    try value.compareTo(other.getDecimalValue)
    catch {
      case err: XPathException =>
        throw new AssertionError("Conversion of integer to decimal should never fail")
    }
  }
  else if (other.isInstanceOf[BigDecimalValue]) value.compareTo(other.asInstanceOf[BigDecimalValue].value)
  else if (other.isInstanceOf[FloatValue]) -other.compareTo(this)
  else super.compareTo(other)

  /**
   * Compare the value to a long
   *
   * @param other the value to be compared with
   * @return -1 if this is less, 0 if this is equal, +1 if this is greater or if this is NaN
   */
  override def compareTo(other: Long): Int = {
    if (other == 0) return value.signum
    value.compareTo(BigDecimal.valueOf(other))
  }

  /**
   * Get an object that implements XML Schema comparison semantics
   */
  override def getSchemaComparable: Comparable[AnyRef] = new BigDecimalValue.DecimalComparable(this)

  /**
   * Determine whether two atomic values are identical, as determined by XML Schema rules. This is a stronger
   * test than equality (even schema-equality); for example two dateTime values are not identical unless
   * they are in the same timezone.
   * <p>Note that even this check ignores the type annotation of the value. The integer 3 and the short 3
   * are considered identical, even though they are not fully interchangeable. "Identical" means the
   * same point in the value space, regardless of type annotation.</p>
   * <p>NaN is identical to itself.</p>
   *
   * @param v the other value to be compared with this one
   * @return true if the two values are identical, false otherwise.
   */
  override def isIdentical(v: AtomicValue) = v.isInstanceOf[DecimalValue] && this == v
}