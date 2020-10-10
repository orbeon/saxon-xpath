package org.orbeon.saxon.value

import org.orbeon.saxon.expr.Calculator
import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.model.ValidationFailure
import org.orbeon.saxon.om.{Item, SequenceTool, StandardNames}
import org.orbeon.saxon.trans.XPathException
import java.math.BigDecimal
import java.math.BigInteger

import Int64Value._


object Int64Value {

  val MINUS_ONE: Int64Value = new Int64Value(-1)

  val ZERO: Int64Value = new Int64Value(0)

  val PLUS_ONE: Int64Value = new Int64Value(+1)

  val MAX_LONG: Int64Value = new Int64Value(java.lang.Long.MAX_VALUE)

  val MIN_LONG: Int64Value = new Int64Value(java.lang.Long.MIN_VALUE)

  private val SMALL_INTEGERS: Array[Int64Value] = Array(
    new Int64Value(0),
    new Int64Value(1),
    new Int64Value(2),
    new Int64Value(3),
    new Int64Value(4),
    new Int64Value(5),
    new Int64Value(6),
    new Int64Value(7),
    new Int64Value(8),
    new Int64Value(9),
    new Int64Value(10),
    new Int64Value(11),
    new Int64Value(12),
    new Int64Value(13),
    new Int64Value(14),
    new Int64Value(15),
    new Int64Value(16),
    new Int64Value(17),
    new Int64Value(18),
    new Int64Value(19),
    new Int64Value(20)
  )

  def makeIntegerValue(value: Long): Int64Value =
    if (value <= 20 && value >= 0) {
      SMALL_INTEGERS(value.toInt)
    } else {
      new Int64Value(value)
    }

  def makeDerived(`val`: Long, `type`: AtomicType): Int64Value = {
    val v = new Int64Value(`val`)
    v.typeLabel = `type`
    v
  }

  def signum(`val`: Long): Int64Value =
    if (`val` == 0) {
      ZERO
    } else {
      if (`val` < 0) MINUS_ONE else PLUS_ONE
    }

  class Int64Comparable(var value: Int64Value) extends Comparable[AnyRef] {

    def asLong(): Long = value.longValue()

    def compareTo(o: AnyRef): Int =
      if (o.isInstanceOf[Int64Comparable]) {
        val long0: Long = value.longValue()
        val long1: Long = o.asInstanceOf[Int64Comparable].value.longValue()
        if (long0 <= long1) {
          if (long0 == long1) {
            0
          } else {
            -1
          }
        } else {
          1
        }
      } else if (o.isInstanceOf[BigIntegerValue.BigIntegerComparable]) {
        value
          .asBigInteger()
          .compareTo(
            o.asInstanceOf[BigIntegerValue.BigIntegerComparable]
              .asBigInteger())
      } else if (o.isInstanceOf[BigDecimalValue.DecimalComparable]) {
        value.getDecimalValue.compareTo(
          o.asInstanceOf[BigDecimalValue.DecimalComparable].asBigDecimal)
      } else {
        SequenceTool.INDETERMINATE_ORDERING
      }

    override def equals(o: Any): Boolean =
      if (o.isInstanceOf[Int64Comparable]) {
        asLong() == o.asInstanceOf[Int64Comparable].asLong()
      } else {
        compareTo(o.asInstanceOf) == 0
      }

    override def hashCode: Int = asLong().toInt

  }

}

class Int64Value extends IntegerValue {

  private var value: Long = _

  def this(value: Long) = {
    this()
    this.value = value
    typeLabel = BuiltInAtomicType.INTEGER
  }

  def this(`val`: Long, `type`: BuiltInAtomicType, check: Boolean) = {
    this()
    value = `val`
    typeLabel = `type`
    if (check && !IntegerValue.checkRange(value, `type`)) {
      val err = new XPathException(
        "Integer value " + `val` + " is out of range for the requested type " +
          `type`.getDescription)
      err.setErrorCode("XPTY0004")
      err.setIsTypeError(true)
      throw err
    }
  }

  override def asSubscript(): Int =
    if (value > 0 && value <= java.lang.Integer.MAX_VALUE) {
      value.toInt
    } else {
      -1
    }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue =
    if (typeLabel.getPrimitiveType == StandardNames.XS_INTEGER) {
      val v: Int64Value = new Int64Value(value)
      v.typeLabel = typeLabel
      v
    } else {
      new BigDecimalValue(value)
    }

  def convertToSubType(subtype: BuiltInAtomicType,
                       validate: Boolean): ValidationFailure =
    if (!validate) {
      this.setSubType(subtype)
      null
    } else if (checkRange(subtype)) {
      null
    } else {
      val err: ValidationFailure = new ValidationFailure(
        "String " + value + " cannot be converted to integer subtype " +
          subtype.getDescription)
      err.setErrorCode("FORG0001")
      err
    }

  def validateAgainstSubType(`type`: BuiltInAtomicType): ValidationFailure =
    if (IntegerValue.checkRange(value, `type`)) {
      null
    } else {
      val err: ValidationFailure = new ValidationFailure(
        "Value " + value + " cannot be converted to integer subtype " +
          `type`.getDescription)
      err.setErrorCode("FORG0001")
      err
    }

  def setSubType(`type`: AtomicType): Unit = {
    typeLabel = `type`
  }

  def checkRange(`type`: BuiltInAtomicType): Boolean = {
    typeLabel = `type`
    IntegerValue.checkRange(value, `type`)
  }

  def getSchemaComparable(): Comparable[AnyRef] = new Int64Comparable(this)

  override def hashCode: Int =
    if (value > java.lang.Integer.MIN_VALUE && value < java.lang.Integer.MAX_VALUE) {
      value.toInt
    } else {
      java.lang.Double.valueOf(getDoubleValue).hashCode
    }

  def longValue(): Long = value

  override def effectiveBooleanValue: Boolean = value != 0

  override def compareTo(other: NumericValue): Int =
    if (other.isInstanceOf[Int64Value]) {
      java.lang.Long.compare(value, other.asInstanceOf[Int64Value].value)
    } else if (other.isInstanceOf[BigIntegerValue]) {
      BigInteger
        .valueOf(value)
        .compareTo(other.asInstanceOf[BigIntegerValue].asBigInteger())
    } else if (other.isInstanceOf[BigDecimalValue]) {
      new BigDecimal(value)
        .compareTo(other.asInstanceOf[BigDecimalValue].getDecimalValue)
    } else {
      super.compareTo(other)
    }

  def compareTo(other: Long): Int = java.lang.Long.compare(value, other)

  def getPrimitiveStringValue(): String = java.lang.Long.toString(value)

  def getDoubleValue(): Double = value.toDouble

  def getFloatValue(): Float = value.toFloat

  def getDecimalValue(): BigDecimal = BigDecimal.valueOf(value)

  def negate(): NumericValue =
    if (value == java.lang.Long.MIN_VALUE) {
      IntegerValue.makeIntegerValue(BigInteger.valueOf(value)).negate()
    } else {
      new Int64Value(-value)
    }

  def floor(): NumericValue = this

  def ceiling(): NumericValue = this

  def round(scale: Int): NumericValue =
    if (scale >= 0 || value == 0) {
      this
    } else {
      if (scale < -15) {
        new BigIntegerValue(value).round(scale)
      }
      val absolute: Long = Math.abs(value)
      var factor: Long = 1
      var i: Long = 1
      while (i <= -scale) {
        factor *= 10
        i = i + 1
      }
      val modulus: Long = absolute % factor
      var rval: Long = absolute - modulus
      val d: Long = modulus * 2
      if (value > 0) {
        if (d >= factor) {
          rval += factor
        }
      } else {
        if (d > factor) {
          rval += factor
        }
        rval = -rval
      }
      new Int64Value(rval)
    }

  def roundHalfToEven(scale: Int): NumericValue =
    if (scale >= 0) {
      this
    } else {
      if (scale < -15) {
        new BigIntegerValue(value).roundHalfToEven(scale)
      }
      val absolute: Long = Math.abs(value)
      var factor: Long = 1
      var i: Long = 1
      while (i <= -scale) {
        factor *= 10
        i = i + 1;
      }
      val modulus: Long = absolute % factor
      var rval: Long = absolute - modulus
      val d: Long = modulus * 2
      if (d > factor) {
        rval += factor
      } else if (d < factor) {} else {
        if (rval % (2 * factor) == 0) {} else {
          rval += factor
        }
      }
      if (value < 0) {
        rval = -rval
      }
      new Int64Value(rval)
    }

  def signum(): Int = {
    if (value > 0) return +1
    if (value == 0) return 0
    -1
  }

  def abs(): NumericValue =
    if (value > 0) {
      this
    } else if (value == java.lang.Long.MIN_VALUE) {
      new BigIntegerValue(new BigInteger("9223372036854775808"))
    } else {
      makeIntegerValue(-value)
    }

  def plus(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[Int64Value]) {
      val topa: Long = (value >> 60) & 0xf
      if (topa != 0 && topa != 0xf) {
        new BigIntegerValue(value)
          .plus(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      }
      val topb: Long = (other.asInstanceOf[Int64Value].value >> 60) & 0xf
      if (topb != 0 && topb != 0xf) {
        new BigIntegerValue(value)
          .plus(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      }
      makeIntegerValue(value + other.asInstanceOf[Int64Value].value)
    } else {
      new BigIntegerValue(value).plus(other)
    }

  def minus(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[Int64Value]) {
      val topa: Long = (value >> 60) & 0xf
      if (topa != 0 && topa != 0xf) {
        new BigIntegerValue(value)
          .minus(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      }
      val topb: Long = (other.asInstanceOf[Int64Value].value >> 60) & 0xf
      if (topb != 0 && topb != 0xf) {
        new BigIntegerValue(value)
          .minus(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      }
      makeIntegerValue(value - other.asInstanceOf[Int64Value].value)
    } else {
      new BigIntegerValue(value).minus(other)
    }

  def times(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[Int64Value]) {
      if (isLong || other.asInstanceOf[Int64Value].isLong) {
        new BigIntegerValue(value)
          .times(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      } else {
        makeIntegerValue(value * other.asInstanceOf[Int64Value].value)
      }
    } else {
      new BigIntegerValue(value).times(other)
    }

  def div(other: IntegerValue): NumericValue =
    if (other.isInstanceOf[Int64Value]) {
      val quotient: Long = other.asInstanceOf[Int64Value].value
      if (quotient == 0) {
        throw new XPathException("Integer division by zero", "FOAR0001")
      }
      if (isLong || other.asInstanceOf[Int64Value].isLong) {
        new BigIntegerValue(value).div(new BigIntegerValue(quotient))
      }
      if (value % quotient == 0) {
        makeIntegerValue(value / quotient)
      } else {
        Calculator.decimalDivide(new BigDecimalValue(value),
          new BigDecimalValue(quotient))
      }
    } else {
      new BigIntegerValue(value).div(other)
    }

  def mod(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[Int64Value]) {
      val quotient: Long = other.asInstanceOf[Int64Value].value
      if (quotient == 0) {
        throw new XPathException("Integer modulo zero", "FOAR0001")
      }
      if (isLong || other.asInstanceOf[Int64Value].isLong) {
        new BigIntegerValue(value)
          .mod(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      } else {
        makeIntegerValue(value % quotient)
      }
    } else {
      new BigIntegerValue(value).mod(other)
    }

  def idiv(other: IntegerValue): IntegerValue =
    if (other.isInstanceOf[Int64Value]) {
      if (isLong || other.asInstanceOf[Int64Value].isLong) {
        new BigIntegerValue(value)
          .idiv(new BigIntegerValue(other.asInstanceOf[Int64Value].value))
      }
      try makeIntegerValue(value / other.asInstanceOf[Int64Value].value)
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
    } else {
      new BigIntegerValue(value).idiv(other)
    }

  private def isLong: Boolean = {
    val top: Long = value >> 31
    top != 0
  }

  def asBigInteger(): BigInteger = BigInteger.valueOf(value)

}