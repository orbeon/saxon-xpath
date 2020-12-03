////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import java.math.{BigDecimal, BigInteger, RoundingMode}

import org.orbeon.saxon.expr.parser.Token
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType, Converter, Type}
import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.IntHashMap


/**
  * This class evaluates arithmetic expressions; it acts as a helper class to the ArithmeticExpression
  * class. There are many subclasses for the different kinds of arithmetic expression, and static methods
  * that allow the right subclass to be selected, either at compile time or at run time.
  */
object Calculator {

  val PLUS : Int = 0
  val MINUS: Int = 1
  val TIMES: Int = 2
  val DIV  : Int = 3
  val MOD  : Int = 4
  val IDIV : Int = 5

  def getTokenFromOperator(operator: Int): Int = tokens(operator)

  private val tokens: Array[Int] = Array(Token.PLUS,
                                         Token.MINUS,
                                         Token.MULT,
                                         Token.DIV,
                                         Token.MOD,
                                         Token.IDIV)

  val ANY_ANY: Array[Calculator] = Array(new AnyPlusAny(),
                                         new AnyMinusAny(),
                                         new AnyTimesAny(),
                                         new AnyDivAny(),
                                         new AnyModAny(),
                                         new AnyIdivAny())

  val DOUBLE_DOUBLE: Array[Calculator] = Array(new DoublePlusDouble(),
                                               new DoubleMinusDouble(),
                                               new DoubleTimesDouble(),
                                               new DoubleDivDouble(),
                                               new DoubleModDouble(),
                                               new DoubleIdivDouble())

  val DOUBLE_FLOAT: Array[Calculator] = DOUBLE_DOUBLE

  val DOUBLE_DECIMAL: Array[Calculator] = DOUBLE_DOUBLE

  val DOUBLE_INTEGER: Array[Calculator] = DOUBLE_DOUBLE

  val FLOAT_DOUBLE: Array[Calculator] = DOUBLE_DOUBLE

  val FLOAT_FLOAT: Array[Calculator] = Array(new FloatPlusFloat(),
                                             new FloatMinusFloat(),
                                             new FloatTimesFloat(),
                                             new FloatDivFloat(),
                                             new FloatModFloat(),
                                             new FloatIdivFloat())

  val FLOAT_DECIMAL: Array[Calculator] = FLOAT_FLOAT

//public final static Calculator[] FLOAT_PDECIMAL = FLOAT_FLOAT;
  val FLOAT_INTEGER: Array[Calculator] = FLOAT_FLOAT

  val DECIMAL_DOUBLE: Array[Calculator] = DOUBLE_DOUBLE

  val DECIMAL_FLOAT: Array[Calculator] = FLOAT_FLOAT

  val DECIMAL_DECIMAL: Array[Calculator] = Array(new DecimalPlusDecimal(),
                                                 new DecimalMinusDecimal(),
                                                 new DecimalTimesDecimal(),
                                                 new DecimalDivDecimal(),
                                                 new DecimalModDecimal(),
                                                 new DecimalIdivDecimal())

  val DECIMAL_INTEGER: Array[Calculator] = DECIMAL_DECIMAL

  val INTEGER_DOUBLE: Array[Calculator] = DOUBLE_DOUBLE

  val INTEGER_FLOAT: Array[Calculator] = FLOAT_FLOAT

  val INTEGER_DECIMAL: Array[Calculator] = DECIMAL_DECIMAL

  val INTEGER_INTEGER: Array[Calculator] = Array(new IntegerPlusInteger(),
                                                 new IntegerMinusInteger(),
                                                 new IntegerTimesInteger(),
                                                 new IntegerDivInteger(),
                                                 new IntegerModInteger(),
                                                 new IntegerIdivInteger())

  /*@Nullable*/

  val DATETIME_DATETIME: Array[Calculator] =
    Array(null, new DateTimeMinusDateTime(), null, null, null, null)

  val DATETIME_DURATION: Array[Calculator] = Array(new DateTimePlusDuration(),
                                                   new DateTimeMinusDuration(),
                                                   null,
                                                   null,
                                                   null,
                                                   null)

  val DURATION_DATETIME: Array[Calculator] =
    Array(new DurationPlusDateTime(), null, null, null, null, null)

  val DURATION_DURATION: Array[Calculator] = Array(new DurationPlusDuration(),
                                                   new DurationMinusDuration(),
                                                   null,
                                                   new DurationDivDuration(),
                                                   null,
                                                   null)

  val DURATION_NUMERIC: Array[Calculator] = Array(null,
                                                  null,
                                                  new DurationTimesNumeric(),
                                                  new DurationDivNumeric(),
                                                  null,
                                                  null)

  val NUMERIC_DURATION: Array[Calculator] =
    Array(null, null, new NumericTimesDuration(), null, null, null)

  private val table: IntHashMap[Array[Calculator]] = new IntHashMap(100)
  private val nameTable: IntHashMap[String] = new IntHashMap(100)

  private def `def`(typeA: Int, typeB: Int, calculatorSet: Array[Calculator], setName: String): Unit = {

    val key: Int = (typeA & 0xffff) << 16 | (typeB & 0xffff)

    table.put(key, calculatorSet)
    nameTable.put(key, setName)

    // As well as the entries added directly, we also add derived entries for other types
    // considered primitive
    if (typeA == StandardNames.XS_DURATION) {
      `def`(StandardNames.XS_DAY_TIME_DURATION, typeB, calculatorSet, setName)
      `def`(StandardNames.XS_YEAR_MONTH_DURATION, typeB, calculatorSet, setName)
    }
    if (typeB == StandardNames.XS_DURATION) {
      `def`(typeA, StandardNames.XS_DAY_TIME_DURATION, calculatorSet, setName)
      `def`(typeA, StandardNames.XS_YEAR_MONTH_DURATION, calculatorSet, setName)
    }
    if (typeA == StandardNames.XS_DATE_TIME) {
      `def`(StandardNames.XS_DATE, typeB, calculatorSet, setName)
      `def`(StandardNames.XS_TIME, typeB, calculatorSet, setName)
    }
    if (typeB == StandardNames.XS_DATE_TIME) {
      `def`(typeA, StandardNames.XS_DATE, calculatorSet, setName)
      `def`(typeA, StandardNames.XS_TIME, calculatorSet, setName)
    }
    if (typeA == StandardNames.XS_DOUBLE) {
      `def`(StandardNames.XS_UNTYPED_ATOMIC, typeB, calculatorSet, setName)
    }
    if (typeB == StandardNames.XS_DOUBLE) {
      `def`(typeA, StandardNames.XS_UNTYPED_ATOMIC, calculatorSet, setName)
    }
  }
// As well as the entries added directly, we also add derived entries for other types
// As well as the entries added directly, we also add derived entries for other types

  def getCalculator(typeA: Int, typeB: Int, operator: Int, mustResolve: Boolean): Calculator = {

    val key: Int = (typeA & 0xffff) << 16 | (typeB & 0xffff)

    val set: Array[Calculator] = table.get(key)

    if (set == null) {
      if (mustResolve) {
        null
      } else {
        ANY_ANY(operator)
      }
    } else {
      set(operator)
    }
  }

  def reconstructCalculator(code: String): Calculator = {
    val typeA: Int = typeFromCode(code.charAt(0))
    val typeB: Int = typeFromCode(code.charAt(2))
    val operator: Int = operatorFromCode(code.charAt(1))
    getCalculator(typeA, typeB, operator, mustResolve = false)
  }

  private def typeFromCode(code: Char): Int = code match {
    case 'a' => StandardNames.XS_ANY_ATOMIC_TYPE
    case 'd' => StandardNames.XS_DOUBLE
    case 'i' => StandardNames.XS_INTEGER
    case 'f' => StandardNames.XS_FLOAT
    case 'c' => StandardNames.XS_DECIMAL
    case 'n' => StandardNames.XS_NUMERIC
    case 't' => StandardNames.XS_DATE_TIME
    case 'u' => StandardNames.XS_DURATION
    case _ => throw new AssertionError
  }

  def operatorFromCode(code: Char): Int = code match {
    case '+' => PLUS
    case '-' => MINUS
    case '*' => TIMES
    case '/' => DIV
    case '~' => IDIV
    case '%' => MOD
    case _ => throw new AssertionError
  }

  def getCalculatorSetName(typeA: Int, typeB: Int): String = {
    val key: Int = (typeA & 0xffff) << 16 | (typeB & 0xffff)
    nameTable.get(key)
  }

  `def`(StandardNames.XS_DOUBLE,
        StandardNames.XS_DOUBLE,
        DOUBLE_DOUBLE,
        "DOUBLE_DOUBLE")

  `def`(StandardNames.XS_DOUBLE,
        StandardNames.XS_FLOAT,
        DOUBLE_FLOAT,
        "DOUBLE_FLOAT")

  `def`(StandardNames.XS_DOUBLE,
        StandardNames.XS_DECIMAL,
        DOUBLE_DECIMAL,
        "DOUBLE_DECIMAL")

  `def`(StandardNames.XS_DOUBLE,
        StandardNames.XS_INTEGER,
        DOUBLE_INTEGER,
        "DOUBLE_INTEGER")

  `def`(StandardNames.XS_FLOAT,
        StandardNames.XS_DOUBLE,
        FLOAT_DOUBLE,
        "FLOAT_DOUBLE")

  `def`(StandardNames.XS_FLOAT,
        StandardNames.XS_FLOAT,
        FLOAT_FLOAT,
        "FLOAT_FLOAT")

  `def`(StandardNames.XS_FLOAT,
        StandardNames.XS_DECIMAL,
        FLOAT_DECIMAL,
        "FLOAT_DECIMAL")

  `def`(StandardNames.XS_FLOAT,
        StandardNames.XS_INTEGER,
        FLOAT_INTEGER,
        "FLOAT_INTEGER")

  `def`(StandardNames.XS_DECIMAL,
        StandardNames.XS_DOUBLE,
        DECIMAL_DOUBLE,
        "DECIMAL_DOUBLE")

  `def`(StandardNames.XS_DECIMAL,
        StandardNames.XS_FLOAT,
        DECIMAL_FLOAT,
        "DECIMAL_FLOAT")

  `def`(StandardNames.XS_DECIMAL,
        StandardNames.XS_DECIMAL,
        DECIMAL_DECIMAL,
        "DECIMAL_DECIMAL")

  `def`(StandardNames.XS_DECIMAL,
        StandardNames.XS_INTEGER,
        DECIMAL_INTEGER,
        "DECIMAL_INTEGER")

  `def`(StandardNames.XS_INTEGER,
        StandardNames.XS_DOUBLE,
        INTEGER_DOUBLE,
        "INTEGER_DOUBLE")

  `def`(StandardNames.XS_INTEGER,
        StandardNames.XS_FLOAT,
        INTEGER_FLOAT,
        "INTEGER_FLOAT")

  `def`(StandardNames.XS_INTEGER,
        StandardNames.XS_DECIMAL,
        INTEGER_DECIMAL,
        "INTEGER_DECIMAL")

  `def`(StandardNames.XS_INTEGER,
        StandardNames.XS_INTEGER,
        INTEGER_INTEGER,
        "INTEGER_INTEGER")

  `def`(StandardNames.XS_DATE_TIME,
        StandardNames.XS_DATE_TIME,
        DATETIME_DATETIME,
        "DATETIME_DATETIME")

  `def`(StandardNames.XS_DATE_TIME,
        StandardNames.XS_DURATION,
        DATETIME_DURATION,
        "DATETIME_DURATION")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_DATE_TIME,
        DURATION_DATETIME,
        "DURATION_DATETIME")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_DURATION,
        DURATION_DURATION,
        "DURATION_DURATION")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_DOUBLE,
        DURATION_NUMERIC,
        "DURATION_NUMERIC")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_FLOAT,
        DURATION_NUMERIC,
        "DURATION_NUMERIC")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_DECIMAL,
        DURATION_NUMERIC,
        "DURATION_NUMERIC")

  `def`(StandardNames.XS_DURATION,
        StandardNames.XS_INTEGER,
        DURATION_NUMERIC,
        "DURATION_NUMERIC")

  `def`(StandardNames.XS_DOUBLE,
        StandardNames.XS_DURATION,
        NUMERIC_DURATION,
        "NUMERIC_DURATION")

  `def`(StandardNames.XS_FLOAT,
        StandardNames.XS_DURATION,
        NUMERIC_DURATION,
        "NUMERIC_DURATION")

  `def`(StandardNames.XS_DECIMAL,
        StandardNames.XS_DURATION,
        NUMERIC_DURATION,
        "NUMERIC_DURATION")

  `def`(StandardNames.XS_INTEGER,
        StandardNames.XS_DURATION,
        NUMERIC_DURATION,
        "NUMERIC_DURATION")

  class AnyPlusAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           PLUS,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for + operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class AnyMinusAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           MINUS,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for - operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class AnyTimesAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           TIMES,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for * operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class AnyDivAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           DIV,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for div operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class AnyModAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           MOD,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for mod operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class AnyIdivAny extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val calc: Calculator = getCalculator(a.getItemType.getPrimitiveType,
                                           b.getItemType.getPrimitiveType,
                                           IDIV,
                                           mustResolve = true)
      if (calc == null) {
        throw new XPathException(
          "Unsuitable types for idiv operation (" + Type.displayTypeName(a) +
            ", " +
            Type.displayTypeName(b) +
            ")",
          "XPTY0004",
          c)
      } else {
        calc.compute(a, b, c)
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.ANY_ATOMIC

  }

  class DoublePlusDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new DoubleValue(
        a.asInstanceOf[NumericValue].getDoubleValue + b
          .asInstanceOf[NumericValue]
          .getDoubleValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DOUBLE

  }

  class DoubleMinusDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new DoubleValue(
        a.asInstanceOf[NumericValue].getDoubleValue - b
          .asInstanceOf[NumericValue]
          .getDoubleValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DOUBLE

  }

  class DoubleTimesDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new DoubleValue(
        a.asInstanceOf[NumericValue].getDoubleValue * b
          .asInstanceOf[NumericValue]
          .getDoubleValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DOUBLE

  }

  class DoubleDivDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new DoubleValue(
        a.asInstanceOf[NumericValue].getDoubleValue / b
          .asInstanceOf[NumericValue]
          .getDoubleValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DOUBLE

  }

  class DoubleModDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new DoubleValue(
        a.asInstanceOf[NumericValue].getDoubleValue % b
          .asInstanceOf[NumericValue]
          .getDoubleValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DOUBLE

  }

  private class DoubleIdivDouble extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val A: Double = a.asInstanceOf[NumericValue].getDoubleValue
      val B: Double = b.asInstanceOf[NumericValue].getDoubleValue
      if (B == 0.0) {
        throw new XPathException("Integer division by zero", "FOAR0001", c)
      }
      if (java.lang.Double.isNaN(A) || java.lang.Double.isInfinite(A)) {
        throw new XPathException("First operand of idiv is NaN or infinity",
                                 "FOAR0002",
                                 c)
      }
      if (java.lang.Double.isNaN(B)) {
        throw new XPathException("Second operand of idiv is NaN",
                                 "FOAR0002",
                                 c)
      }
      IntegerValue.makeIntegerValue(new DoubleValue(A / B)).asAtomic
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class FloatPlusFloat extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new FloatValue(
        a.asInstanceOf[NumericValue].getFloatValue + b
          .asInstanceOf[NumericValue]
          .getFloatValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.FLOAT

  }

  class FloatMinusFloat extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new FloatValue(
        a.asInstanceOf[NumericValue].getFloatValue - b
          .asInstanceOf[NumericValue]
          .getFloatValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.FLOAT

  }

  class FloatTimesFloat extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new FloatValue(
        a.asInstanceOf[NumericValue].getFloatValue * b
          .asInstanceOf[NumericValue]
          .getFloatValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.FLOAT

  }

  class FloatDivFloat extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new FloatValue(
        a.asInstanceOf[NumericValue].getFloatValue / b
          .asInstanceOf[NumericValue]
          .getFloatValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.FLOAT

  }

  class FloatModFloat extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      new FloatValue(
        a.asInstanceOf[NumericValue].getFloatValue % b
          .asInstanceOf[NumericValue]
          .getFloatValue)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.FLOAT

  }

  class FloatIdivFloat extends Calculator {

    def compute(a: AtomicValue,
                b: AtomicValue,
                c: XPathContext): IntegerValue = {
      val A: Float = a.asInstanceOf[NumericValue].getFloatValue
      val B: Float = b.asInstanceOf[NumericValue].getFloatValue
      if (B == 0.0) {
        throw new XPathException("Integer division by zero", "FOAR0001", c)
      }
      if (java.lang.Float.isNaN(A) || java.lang.Float.isInfinite(A)) {
        throw new XPathException("First operand of idiv is NaN or infinity",
                                 "FOAR0002",
                                 c)
      }
      if (java.lang.Float.isNaN(B)) {
        throw new XPathException("Second operand of idiv is NaN",
                                 "FOAR0002",
                                 c)
      }
      val quotient: Float = A / B
      if (java.lang.Float.isInfinite(quotient)) {
// bug 29171, test cbcl-numeric-idivide-008
        new DecimalIdivDecimal().compute(
          new BigDecimalValue(a.asInstanceOf[NumericValue].getDecimalValue),
          new BigDecimalValue(b.asInstanceOf[NumericValue].getDecimalValue),
          c)
      } else {
        Converter.FloatToInteger.INSTANCE
          .convert(new FloatValue(quotient))
          .asAtomic
          .asInstanceOf[IntegerValue]
      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class DecimalPlusDecimal extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a match {
        case integerValue: IntegerValue if b.isInstanceOf[IntegerValue] =>
          integerValue.plus(b.asInstanceOf[IntegerValue])
        case _ =>
          new BigDecimalValue(
            a.asInstanceOf[NumericValue]
              .getDecimalValue
              .add(b.asInstanceOf[NumericValue].getDecimalValue))
      }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  class DecimalMinusDecimal extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a match {
        case integerValue: IntegerValue if b.isInstanceOf[IntegerValue] =>
          integerValue.minus(b.asInstanceOf[IntegerValue])
        case _ =>
          new BigDecimalValue(
            a.asInstanceOf[NumericValue]
              .getDecimalValue
              .subtract(b.asInstanceOf[NumericValue].getDecimalValue))
      }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  class DecimalTimesDecimal extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a match {
        case integerValue: IntegerValue if b.isInstanceOf[IntegerValue] =>
          integerValue.times(b.asInstanceOf[IntegerValue])
        case _ =>
          new BigDecimalValue(
            a.asInstanceOf[NumericValue]
              .getDecimalValue
              .multiply(b.asInstanceOf[NumericValue].getDecimalValue))
      }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  class DecimalDivDecimal extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      decimalDivide(a.asInstanceOf[NumericValue], b.asInstanceOf[NumericValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  def decimalDivide(a: NumericValue, b: NumericValue): BigDecimalValue = {
    val A: BigDecimal = a.getDecimalValue
    val B: BigDecimal = b.getDecimalValue
//                Math.max(A.scale(), B.scale()));
    val scale: Int = Math.max(
      BigDecimalValue.DIVIDE_PRECISION,
      A.scale() - B.scale() + BigDecimalValue.DIVIDE_PRECISION)
    try {
      val result: BigDecimal = A.divide(B, scale, RoundingMode.HALF_DOWN)
      new BigDecimalValue(result)
    } catch {
      case err: ArithmeticException =>
        if (b.compareTo(0) == 0) {
          throw new XPathException("Decimal divide by zero", "FOAR0001")
        } else {
          throw err
        }

    }
  }
//        int scale = Math.max(DecimalValue.DIVIDE_PRECISION,
//        int scale = Math.max(DecimalValue.DIVIDE_PRECISION,

  class DecimalModDecimal extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      a match {
        case integerValue: IntegerValue if b.isInstanceOf[IntegerValue] =>
          integerValue.mod(b.asInstanceOf[IntegerValue])
        case _ =>
      }
      val A: BigDecimal = a.asInstanceOf[NumericValue].getDecimalValue
      val B: BigDecimal = b.asInstanceOf[NumericValue].getDecimalValue
      try //BigDecimal remainder = A.subtract(quotient.multiply(B));
      new BigDecimalValue(A.remainder(B))
      catch {
        case err: ArithmeticException =>
          if (b.asInstanceOf[NumericValue].compareTo(0) == 0) {
            throw new XPathException("Decimal modulo zero", "FOAR0001", c)
          } else {
            throw err
          }

      }
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  class DecimalIdivDecimal extends Calculator {

    def compute(a: AtomicValue,
                b: AtomicValue,
                c: XPathContext): IntegerValue = {
      a match {
        case integerValue: IntegerValue if b.isInstanceOf[IntegerValue] =>
          integerValue.idiv(b.asInstanceOf[IntegerValue])
        case _ =>
      }
      val A: BigDecimal = a.asInstanceOf[NumericValue].getDecimalValue
      val B: BigDecimal = b.asInstanceOf[NumericValue].getDecimalValue
      if (B.signum() == 0) {
        throw new XPathException("Integer division by zero", "FOAR0001", c)
      }
//BigInteger quot = A.divide(B, 0, BigDecimal.ROUND_DOWN).toBigInteger();
      val quot: BigInteger = A.divideToIntegralValue(B).toBigInteger
      IntegerValue.makeIntegerValue(quot)
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class IntegerPlusInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].plus(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class IntegerMinusInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].minus(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class IntegerTimesInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].times(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class IntegerDivInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].div(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  class IntegerModInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].mod(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  class IntegerIdivInteger extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[IntegerValue].idiv(b.asInstanceOf[IntegerValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.INTEGER

  }

  private class DateTimeMinusDateTime extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[CalendarValue].subtract(b.asInstanceOf[CalendarValue], c)

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DAY_TIME_DURATION

  }

  private class DateTimePlusDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[CalendarValue].add(b.asInstanceOf[DurationValue]).asInstanceOf[AtomicValue]

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

  private class DateTimeMinusDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[CalendarValue].add(b.asInstanceOf[DurationValue].negate()).asInstanceOf[AtomicValue]

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

  private class DurationPlusDateTime extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      b.asInstanceOf[CalendarValue].add(a.asInstanceOf[DurationValue]).asInstanceOf[AtomicValue]

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeB

  }

  private class DurationPlusDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[DurationValue].add(b.asInstanceOf[DurationValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

  private class DurationMinusDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[DurationValue].subtract(b.asInstanceOf[DurationValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

  private class DurationDivDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a.asInstanceOf[DurationValue].divide(b.asInstanceOf[DurationValue])

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType =
      BuiltInAtomicType.DECIMAL

  }

  private class DurationTimesNumeric extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      b match {
        case int64Value: Int64Value =>
          a.asInstanceOf[DurationValue]
            .multiply(int64Value.longValue)
        case _ =>
          a.asInstanceOf[DurationValue]
            .multiply(b.asInstanceOf[NumericValue].getDoubleValue)
      }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

  private class NumericTimesDuration extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue =
      a match {
        case int64Value: Int64Value =>
          b.asInstanceOf[DurationValue]
            .multiply(int64Value.longValue)
        case _ =>
          b.asInstanceOf[DurationValue]
            .multiply(a.asInstanceOf[NumericValue].getDoubleValue)
      }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeB

  }

  private class DurationDivNumeric extends Calculator {

    def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue = {
      val d: Double = 1.0 / b.asInstanceOf[NumericValue].getDoubleValue
      a.asInstanceOf[DurationValue].multiply(d)
    }

    def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType = typeA

  }

}

abstract class Calculator {

  def code(): String = {
    val name: String = getClass.getSimpleName
    name
      .replaceAll("Any", "a")
      .replaceAll("Double", "d")
      .replaceAll("Float", "f")
      .replaceAll("Decimal", "c")
      .replaceAll("Integer", "i")
      .replaceAll("Numeric", "n")
      .replaceAll("DateTime", "t")
      .replaceAll("Duration", "u")
      .replaceAll("Plus", "+")
      .replaceAll("Minus", "-")
      .replaceAll("Times", "*")
      .replaceAll("Div", "/")
      .replaceAll("Idiv", "~")
      .replaceAll("Mod", "%")
  }

  def compute(a: AtomicValue, b: AtomicValue, c: XPathContext): AtomicValue

  def getResultType(typeA: AtomicType, typeB: AtomicType): AtomicType
}