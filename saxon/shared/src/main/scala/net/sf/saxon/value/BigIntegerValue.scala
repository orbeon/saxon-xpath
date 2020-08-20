////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr.Calculator

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trans.XPathException

import java.math.BigDecimal

import java.math.BigInteger

import BigIntegerValue._




object BigIntegerValue {

   val MAX_INT: BigInteger =
    BigInteger.valueOf(java.lang.Integer.MAX_VALUE)

   val MIN_INT: BigInteger =
    BigInteger.valueOf(java.lang.Integer.MIN_VALUE)

  val MAX_LONG: BigInteger = BigInteger.valueOf(java.lang.Long.MAX_VALUE)

  val MIN_LONG: BigInteger = BigInteger.valueOf(java.lang.Long.MIN_VALUE)

  /*@NotNull*/

  val MAX_UNSIGNED_LONG: BigInteger = new BigInteger("18446744073709551615")

  /*@NotNull*/

  val ZERO: BigIntegerValue = new BigIntegerValue(BigInteger.ZERO)

  class BigIntegerComparable( var value: BigIntegerValue)
      extends Comparable[AnyRef] {

    def asBigInteger(): BigInteger = value.asBigInteger()

    def compareTo(o: AnyRef): Int =
      if (o.isInstanceOf[Int64Value.Int64Comparable]) {
        asBigInteger().compareTo(
          BigInteger.valueOf(
            o.asInstanceOf[Int64Value.Int64Comparable].asLong))
      } else if (o.isInstanceOf[BigIntegerComparable]) {
        asBigInteger().compareTo(
          o.asInstanceOf[BigIntegerComparable].asBigInteger)
      } else if (o.isInstanceOf[BigDecimalValue.DecimalComparable]) {
        value.getDecimalValue.compareTo(
          o.asInstanceOf[BigDecimalValue.DecimalComparable].asBigDecimal)
      } else {
        SequenceTool.INDETERMINATE_ORDERING
      }

    def equals(o: BigIntegerComparable): Boolean = compareTo(o) == 0

    override def hashCode(): Int = {
// Must align with hashCodes for other subtypes of xs:decimal
      val big: BigInteger = value.asBigInteger()
      if (big.compareTo(MAX_LONG) < 0 && big.compareTo(MIN_LONG) > 0) {
        val iv: Int64Value = new Int64Value(big.longValue())
        iv.hashCode
      }
      big.hashCode
    }

  }

}

class BigIntegerValue() extends IntegerValue {

  typeLabel = BuiltInAtomicType.INTEGER
  var value: BigInteger = _

  def this(value: BigInteger) = {
    this()
    this.value = value
  }

  def this(value: BigInteger, typeLabel: AtomicType) = {
    this()
    this.value = value
    this.typeLabel = typeLabel
  }

  def this(value: Long) = {
    this()
    this.value = BigInteger.valueOf(value)
    typeLabel = BuiltInAtomicType.INTEGER
  }

  /*@NotNull*/

  def copyAsSubType(typeLabel: AtomicType): AtomicValue =
    if (typeLabel.getPrimitiveType == StandardNames.XS_INTEGER) {
      val v: BigIntegerValue = new BigIntegerValue(value)
      v.typeLabel = typeLabel
      v
    } else {
      new BigDecimalValue(new BigDecimal(value))
    }

  /*@Nullable*/

  def convertToSubType(`type`: BuiltInAtomicType,
                       validate: Boolean): ValidationFailure = {
    if (!validate) {
      typeLabel = `type`
      return null
    }
    if (IntegerValue.checkBigRange(value, `type`)) {
      typeLabel = `type`
      null
    } else {
      val err: ValidationFailure = new ValidationFailure(
        "Integer value is out of range for subtype " + `type`.getDisplayName)
      err.setErrorCode("FORG0001")
      err
    }
  }

  /*@Nullable*/

  def validateAgainstSubType(`type`: BuiltInAtomicType): ValidationFailure =
    if (IntegerValue.checkBigRange(value, `type`)) {
      typeLabel = `type`
      null
    } else {
      val err: ValidationFailure = new ValidationFailure(
        "Integer value is out of range for subtype " + `type`.getDisplayName)
      err.setErrorCode("FORG0001")
      err
    }

  override def hashCode(): Int =
    if (value.compareTo(MIN_INT) >= 0 && value.compareTo(MAX_INT) <= 0) {
      value.intValue()
    } else {
      java.lang.Double.valueOf(getDoubleValue).hashCode
    }

  def longValue(): Long = value.longValue()

  def asBigInteger(): BigInteger = value

  def isWithinLongRange(): Boolean =
    value.compareTo(MIN_LONG) >= 0 && value.compareTo(MAX_LONG) <= 0

  /*@NotNull*/

  def asDecimal(): BigDecimal = new BigDecimal(value)

  /**
    * Return the effective boolean value of this integer
    *
    * @return false if the integer is zero, otherwise true
    */
  override def effectiveBooleanValue(): Boolean = value.compareTo(BigInteger.ZERO) != 0

  override def compareTo(other: NumericValue): Int =
    if (other.isInstanceOf[BigIntegerValue]) {
      value.compareTo(other.asInstanceOf[BigIntegerValue].value)
    } else if (other.isInstanceOf[Int64Value]) {
      value.compareTo(
        BigInteger.valueOf(other.asInstanceOf[Int64Value].longValue()))
    } else if (other.isInstanceOf[BigDecimalValue]) {
      asDecimal().compareTo(
        other.asInstanceOf[BigDecimalValue].getDecimalValue)
    } else {
      super.compareTo(other)
    }

  def compareTo(other: Long): Int = {
    if (other == 0) {
      return value.signum()
    }
    value.compareTo(BigInteger.valueOf(other))
  }

  def getPrimitiveStringValue(): String = value.toString

  /**
    * Get the numeric value as a double
    *
    * @return A double representing this numeric value; NaN if it cannot be
    *         converted
    */
  def getDoubleValue(): Double = value.doubleValue()

  /*@NotNull*/

  def getDecimalValue(): BigDecimal = new BigDecimal(value)

  /**
    * Get the numeric value converted to a float
    *
    * @return a float representing this numeric value; NaN if it cannot be converted
    */
  override def getFloatValue(): Float = getDoubleValue.toFloat

  /*@NotNull*/

  def negate(): NumericValue = new BigIntegerValue(value.negate())

  /*@NotNull*/

  def floor(): NumericValue = this

  /*@NotNull*/

  def ceiling(): NumericValue = this

  def round(scale: Int): NumericValue =
    if (scale >= 0) {
      this
    } else {
      val factor: BigInteger = BigInteger.valueOf(10).pow(-scale)
      val pair: Array[BigInteger] = value.divideAndRemainder(factor)
      val up: Int = pair(1).compareTo(factor.divide(BigInteger.valueOf(2)))
      if (up >= 0) {
// remainder is > .5
        pair(0) = pair(0).add(BigInteger.valueOf(1))
      }
      IntegerValue.makeIntegerValue(pair(0).multiply(factor))
    }

  def roundHalfToEven(scale: Int): NumericValue =
    if (scale >= 0) {
      this
    } else {
      val factor: BigInteger = BigInteger.valueOf(10).pow(-scale)
      val pair: Array[BigInteger] = value.divideAndRemainder(factor)
      val up: Int = pair(1).compareTo(factor.divide(BigInteger.valueOf(2)))
      if (up > 0) {
// remainder is > .5
        pair(0) = pair(0).add(BigInteger.valueOf(1))
      } else if (up == 0) {
// remainder == .5
        if (pair(0).mod(BigInteger.valueOf(2)).signum() != 0) {
// last digit of quotient is odd: make it even
          pair(0) = pair(0).add(BigInteger.valueOf(1))
        }
      }
      IntegerValue.makeIntegerValue(pair(0).multiply(factor))
    }

  def signum(): Int = value.signum()

  /*@NotNull*/

  def abs(): NumericValue =
    if (value.signum() >= 0) {
      this
    } else {
      new BigIntegerValue(value.abs())
    }

  override def isWholeNumber(): Boolean = true

  /**
    * Test whether a number is a possible subscript into a sequence, that is,
    * a whole number greater than zero and less than 2^31
    *
    * @return the number as an int if it is a possible subscript, or -1 otherwise
    */
  override def asSubscript(): Int =
    if (value.compareTo(BigInteger.ZERO) > 0 && value.compareTo(MAX_INT) <= 0) {
      longValue().toInt
    } else {
      -1
    }

  def plus(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[BigIntegerValue]) {
      IntegerValue.makeIntegerValue(value.add(other.asInstanceOf[BigIntegerValue].value))
    } else {
//noinspection RedundantCast
      IntegerValue.makeIntegerValue(
        value.add(
          BigInteger.valueOf(other.asInstanceOf[Int64Value].longValue())))
    }

  def minus(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[BigIntegerValue]) {
      IntegerValue.makeIntegerValue(
        value.subtract(other.asInstanceOf[BigIntegerValue].value))
    } else {
//noinspection RedundantCast
      IntegerValue.makeIntegerValue(
        value.subtract(
          BigInteger.valueOf(other.asInstanceOf[Int64Value].longValue())))
    }

  def times(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[BigIntegerValue]) {
      IntegerValue.makeIntegerValue(
        value.multiply(other.asInstanceOf[BigIntegerValue].value))
    } else {
//noinspection RedundantCast
      IntegerValue.makeIntegerValue(
        value.multiply(
          BigInteger.valueOf(other.asInstanceOf[Int64Value].longValue())))
    }

  def div(other: IntegerValue): NumericValue = {
    var oi: BigInteger = null
    oi =
      if (other.isInstanceOf[BigIntegerValue])
        other.asInstanceOf[BigIntegerValue].value
      else BigInteger.valueOf(other.longValue())
    val a: BigDecimalValue = new BigDecimalValue(new BigDecimal(value))
    val b: BigDecimalValue = new BigDecimalValue(new BigDecimal(oi))
    Calculator.decimalDivide(a, b)
  }

  def mod(other: IntegerValue): IntegerValue =
    try if (other.isInstanceOf[BigIntegerValue]) {
      IntegerValue.makeIntegerValue(
        value.remainder(other.asInstanceOf[BigIntegerValue].value))
    } else {
      IntegerValue.makeIntegerValue(value.remainder(BigInteger.valueOf(other.longValue())))
    } catch {
      case err: ArithmeticException => {
        var e: XPathException = null
        e =
          if (BigInteger.valueOf(other.longValue()).signum() == 0)
            new XPathException("Integer modulo zero", "FOAR0001")
          else new XPathException("Integer mod operation failure", err)
        throw e
      }

    }

  def idiv(other: IntegerValue): IntegerValue = {
    var oi: BigInteger = null
    oi =
      if (other.isInstanceOf[BigIntegerValue])
        other.asInstanceOf[BigIntegerValue].value
      else BigInteger.valueOf(other.longValue())
    try IntegerValue.makeIntegerValue(value.divide(oi))
    catch {
      case err: ArithmeticException => {
        var e: XPathException = null
        e =
          if ("/ by zero" == err.getMessage)
            new XPathException("Integer division by zero", "FOAR0001")
          else new XPathException("Integer division failure", err)
        throw e
      }

    }
  }

  /*@NotNull*/

  def getSchemaComparable(): Comparable[AnyRef] = new BigIntegerComparable(this)

  /*@NotNull*/

  override def reduce(): IntegerValue = {
    if (compareTo(java.lang.Long.MAX_VALUE) < 0 && compareTo(
          java.lang.Long.MIN_VALUE) > 0) {
      val iv: Int64Value = new Int64Value(longValue())
      iv.setTypeLabel(typeLabel)
      return iv
    }
    this
  }

  override def asAtomic(): BigIntegerValue = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An integer value: note this is a subtype of decimal in XML Schema, not a primitive type.
  * The abstract class IntegerValue is used to represent any xs:integer value; this implementation
  * is used for values that do not fit comfortably in a Java long; including the built-in subtype xs:unsignedLong
  */
