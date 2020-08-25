////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.tree.util.FastStringBuffer

import java.math.BigInteger

import scala.util.control.Breaks._

object FloatingPointConverter {

  var THE_INSTANCE: FloatingPointConverter = new FloatingPointConverter()

  /**
   * char array holding the characters for the string "-Infinity".
   */
  /**
   * char array holding the characters for the string "-Infinity".
   */
  private val NEGATIVE_INFINITY: Array[Char] = Array('-', 'I', 'N', 'F')

  /**
   * char array holding the characters for the string "Infinity".
   */
  /**
   * char array holding the characters for the string "Infinity".
   */
  private val POSITIVE_INFINITY: Array[Char] = Array('I', 'N', 'F')

  /**
   * char array holding the characters for the string "NaN".
   */
  /**
   * char array holding the characters for the string "NaN".
   */
  private val NaN: Array[Char] = Array('N', 'a', 'N')

  private val charForDigit: Array[Char] =
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  val DOUBLE_SIGN_MASK: Long = 0x8000000000000000L

  private val doubleExpMask: Long = 0x7ff0000000000000L

  private val doubleExpShift: Int = 52

  private val doubleExpBias: Int = 1023

  private val doubleFractMask: Long = 0xfffffffffffffL

  val FLOAT_SIGN_MASK: Int = 0x80000000

  private val floatExpMask: Int = 0x7f800000

  private val floatExpShift: Int = 23

  private val floatExpBias: Int = 127

  private val floatFractMask: Int = 0x7fffff

  private val TEN: BigInteger = BigInteger.valueOf(10)

  private val NINE: BigInteger = BigInteger.valueOf(9)

  def appendInt(s: FastStringBuffer, i: Int): FastStringBuffer = {
    var i1: Int = i
    if (i1 < 0) {
      if (i1 == java.lang.Integer.MIN_VALUE) {
        //cannot make this positive due to integer overflow
        s.append("-2147483648")
        return s
      }
      s.cat('-')
      i1 = -i1
    }
    var c: Int = 0
    if (i1 < 10) {
      //one digit
      s.cat(charForDigit(i1))
      s
    } else if (i1 < 100) {
      //two digits
      s.cat(charForDigit(i1 / 10))
      s.cat(charForDigit(i1 % 10))
      s
    } else if (i1 < 1000) {
      //three digits
      s.cat(charForDigit(i1 / 100))
      c = i1 % 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 10000) {
      //four digits
      s.cat(charForDigit(i1 / 1000))
      c = i1 % 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 100000) {
      //five digits
      s.cat(charForDigit(i1 / 10000))
      c = i1 % 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 1000000) {
      //six digits
      s.cat(charForDigit(i1 / 100000))
      c = i1 % 100000
      s.cat(charForDigit((c) / 10000))
      c %= 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 10000000) {
      //seven digits
      s.cat(charForDigit(i1 / 1000000))
      c = i1 % 1000000
      s.cat(charForDigit((c) / 100000))
      c %= 100000
      s.cat(charForDigit((c) / 10000))
      c %= 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 100000000) {
      //eight digits
      s.cat(charForDigit(i1 / 10000000))
      c = i1 % 10000000
      s.cat(charForDigit((c) / 1000000))
      c %= 1000000
      s.cat(charForDigit((c) / 100000))
      c %= 100000
      s.cat(charForDigit((c) / 10000))
      c %= 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else if (i1 < 1000000000) {
      //nine digits
      s.cat(charForDigit(i1 / 100000000))
      c = i1 % 100000000
      s.cat(charForDigit((c) / 10000000))
      c %= 10000000
      s.cat(charForDigit((c) / 1000000))
      c %= 1000000
      s.cat(charForDigit((c) / 100000))
      c %= 100000
      s.cat(charForDigit((c) / 10000))
      c %= 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    } else {
      //ten digits
      s.cat(charForDigit(i1 / 1000000000))
      c = i1 % 1000000000
      s.cat(charForDigit((c) / 100000000))
      c %= 100000000
      s.cat(charForDigit((c) / 10000000))
      c %= 10000000
      s.cat(charForDigit((c) / 1000000))
      c %= 1000000
      s.cat(charForDigit((c) / 100000))
      c %= 100000
      s.cat(charForDigit((c) / 10000))
      c %= 10000
      s.cat(charForDigit((c) / 1000))
      c %= 1000
      s.cat(charForDigit((c) / 100))
      c %= 100
      s.cat(charForDigit((c) / 10))
      s.cat(charForDigit(c % 10))
      s
    }
  }

  private def fppfpp(sb: FastStringBuffer, e: Int, f: Long, p: Int): Unit = {
    var R: Long = f << Math.max(e - p, 0)
    var S: Long = 1L << Math.max(0, -(e - p))
    var Mminus: Long = 1L << Math.max(e - p, 0)
    var Mplus: Long = Mminus
    var initial: Boolean = true
    if (f == 1L << (p - 1)) {
      Mplus = Mplus << 1
      R = R << 1
      S = S << 1
    }
    var k: Int = 0
    while (R < (S + 9) / 10) {
      // (S+9)/10 == ceiling(S/10)
      k -= 1
      R = R * 10
      Mminus = Mminus * 10
      Mplus = Mplus * 10
    }
    while (2 * R + Mplus >= 2 * S) {
      S = S * 10
      k += 1
    }
    for (z <- k.until(0)) {
      if (initial) {
        sb.append("0.")
      }
      initial = false
      sb.cat('0')
    }
    var low: Boolean = false
    var high: Boolean = false
    var U: Int = 0
    breakable {
      while (true) {
        k -= 1
        val R10: Long = R * 10
        U = (R10 / S).toInt
        // = R*10 % S, but faster - saves a division
        R = R10 - (U * S)
        Mminus = Mminus * 10
        Mplus = Mplus * 10
        low = 2 * R < Mminus
        high = 2 * R > 2 * S - Mplus
        if (low || high) break()
        if (k == -1) {
          if (initial) {
            sb.cat('0')
          }
          sb.cat('.')
        }
        sb.cat(charForDigit(U))
        initial = false
      }
    }

    if (high && (!low || 2 * R > S)) {
      U += 1
    }
    if (k == -1) {
      if (initial) {
        sb.cat('0')
      }
      sb.cat('.')
    }
    sb.cat(charForDigit(U))
    for (z <- 0 until k) {
      sb.cat('0')
    }
  }

  // simpleFixup
  // end simpleFixup
  //int H = k-1;
  // simpleFixup
  // end simpleFixup
  //int H = k-1;

  private def fppfppBig(sb: FastStringBuffer, e: Int, f: Long, p: Int): Unit = {
    //long R = f << Math.max(e-p, 0);
    var R: BigInteger = BigInteger.valueOf(f).shiftLeft(Math.max(e - p, 0))
    //long S = 1L << Math.max(0, -(e-p));
    var S: BigInteger = BigInteger.ONE.shiftLeft(Math.max(0, -(e - p)))
    //long Mminus = 1 << Math.max(e-p, 0);
    var Mminus: BigInteger = BigInteger.ONE.shiftLeft(Math.max(e - p, 0))
    //long Mplus = Mminus;
    var Mplus: BigInteger = Mminus
    var initial: Boolean = true
    if (f == 1L << (p - 1)) {
      Mplus = Mplus.shiftLeft(1)
      R = R.shiftLeft(1)
      S = S.shiftLeft(1)
    }
    var k: Int = 0
    while (R.compareTo(S.add(NINE).divide(TEN)) < 0) {
      // (S+9)/10 == ceiling(S/10)
      k -= 1
      R = R.multiply(TEN)
      Mminus = Mminus.multiply(TEN)
      Mplus = Mplus.multiply(TEN)
    }
    while (R.shiftLeft(1).add(Mplus).compareTo(S.shiftLeft(1)) >=
      0) {
      S = S.multiply(TEN)
      k += 1;
    }
    for (z <- k.until(0)) {
      if (initial) {
        sb.append("0.")
      }
      initial = false
      sb.cat('0')
    }
    var low: Boolean = false
    var high: Boolean = false
    var U: Int = 0
    breakable {
      while (true) {
        k -= 1
        val R10: BigInteger = R.multiply(TEN)
        U = R10.divide(S).intValue()
        R = R10.mod(S)
        Mminus = Mminus.multiply(TEN)
        Mplus = Mplus.multiply(TEN)
        val R2: BigInteger = R.shiftLeft(1)
        low = R2.compareTo(Mminus) < 0
        high = R2.compareTo(S.shiftLeft(1).subtract(Mplus)) > 0
        if (low || high) break()
        if (k == -1) {

          if (initial) {
            sb.cat('0')
          }
          sb.cat('.')
        }
        sb.cat(charForDigit(U))
        initial = false
      }
    }

    if (high && (!low || R.shiftLeft(1).compareTo(S) > 0)) {
      U += 1
    }
    if (k == -1) {
      if (initial) {
        sb.cat('0')
      }
      sb.cat('.')
    }
    sb.cat(charForDigit(U))
    for (z <- 0 until k) {
      sb.cat('0')
    }
  }

  // simpleFixup
  // end simpleFixup
  //int H = k-1;
  // simpleFixup
  // end simpleFixup
  //int H = k-1;

  private def fppfppExponential(sb: FastStringBuffer,
                                e: Int,
                                f: Long,
                                p: Int): Unit = {
    //long R = f << Math.max(e-p, 0);
    var R: BigInteger = BigInteger.valueOf(f).shiftLeft(Math.max(e - p, 0))
    //long S = 1L << Math.max(0, -(e-p));
    var S: BigInteger = BigInteger.ONE.shiftLeft(Math.max(0, -(e - p)))
    //long Mminus = 1 << Math.max(e-p, 0);
    var Mminus: BigInteger = BigInteger.ONE.shiftLeft(Math.max(e - p, 0))
    //long Mplus = Mminus;
    var Mplus: BigInteger = Mminus
    var initial: Boolean = true
    var doneDot: Boolean = false
    if (f == 1L << (p - 1)) {
      Mplus = Mplus.shiftLeft(1)
      R = R.shiftLeft(1)
      S = S.shiftLeft(1)
    }
    var k: Int = 0
    while (R.compareTo(S.add(NINE).divide(TEN)) < 0) {
      // (S+9)/10 == ceiling(S/10)
      k -= 1
      R = R.multiply(TEN)
      Mminus = Mminus.multiply(TEN)
      Mplus = Mplus.multiply(TEN)
    }
    while (R.shiftLeft(1).add(Mplus).compareTo(S.shiftLeft(1)) >= 0) {
      S = S.multiply(TEN)
      k += 1
    }
    val H: Int = k - 1
    var low: Boolean = false
    var high: Boolean = false
    var U: Int = 0
    breakable {
      while (true) {
        k -= 1
        val R10: BigInteger = R.multiply(TEN)
        U = R10.divide(S).intValue()
        R = R10.mod(S)
        Mminus = Mminus.multiply(TEN)
        Mplus = Mplus.multiply(TEN)
        val R2: BigInteger = R.shiftLeft(1)
        low = R2.compareTo(Mminus) < 0
        high = R2.compareTo(S.shiftLeft(1).subtract(Mplus)) > 0
        if (low || high) break()
        sb.cat(charForDigit(U))
        if (initial) {
          sb.cat('.')
          doneDot = true
        }
        initial = false
      }
    }
    if (high && (!low || R.shiftLeft(1).compareTo(S) > 0)) U += 1
    sb.cat(charForDigit(U))
    if (!doneDot) {
      sb.append(".0")
    }
    sb.cat('E')
    appendInt(sb, H)
  }

  // simpleFixup
  // end simpleFixup
  // simpleFixup
  // end simpleFixup

  def appendDouble(s: FastStringBuffer,
                   d: Double,
                   forceExponential: Boolean): FastStringBuffer = {
    var d1: Double = d
    if (d1 == java.lang.Double.NEGATIVE_INFINITY) {
      s.append(NEGATIVE_INFINITY)
    } else if (d1 == java.lang.Double.POSITIVE_INFINITY) {
      s.append(POSITIVE_INFINITY)
    } else if (d1 != d1) {
      s.append(NaN)
    } else if (d1 == 0.0) {
      if ((java.lang.Double.doubleToLongBits(d1) & DOUBLE_SIGN_MASK) !=
        0) {
        s.cat('-')
      }
      s.cat('0')
      if (forceExponential) {
        s.append(".0E0")
      }
    } else if (d1 == java.lang.Double.MAX_VALUE) {
      s.append("1.7976931348623157E308")
    } else if (d1 == -java.lang.Double.MAX_VALUE) {
      s.append("-1.7976931348623157E308")
    } else if (d1 == java.lang.Double.MIN_VALUE) {
      s.append("4.9E-324")
    } else if (d1 == -java.lang.Double.MIN_VALUE) {
      s.append("-4.9E-324")
    } else {
      if (d1 < 0) {
        s.cat('-')
        d1 = -d1
      }
      val bits: Long = java.lang.Double.doubleToLongBits(d1)
      val fraction: Long = (1L << 52) | (bits & doubleFractMask)
      val rawExp: Long = (bits & doubleExpMask) >> doubleExpShift
      val exp: Int = rawExp.toInt - doubleExpBias
      if (rawExp == 0) {
        // don't know how to handle this currently: hand it over to Java to deal with
        s.append(java.lang.Double.toString(d1))
        return s
      }
      if (forceExponential || (d1 >= 1000000 || d1 < 0.000001)) {
        fppfppExponential(s, exp, fraction, 52)
      } else {
        if (d1 <= 0.01) {
          fppfppBig(s, exp, fraction, 52)
        } else {
          fppfpp(s, exp, fraction, 52)
        }
      }
    }
    // test code
    //            try {
    //                if (Double.parseDouble(s.toString()) != value) {
    //                    System.err.println("*** Round-trip failed: input " + value +
    //                            '(' + Double.doubleToLongBits(value) + ')' +
    //                            " != output " + s.toString() +
    //                            '(' + Double.doubleToLongBits(Double.parseDouble(s.toString())) + ')' );
    //                }
    //            } catch (NumberFormatException e) {
    //                System.err.println("*** Bad float " + s.toString() + " for input " + value);
    //            }
    // test code
    //            try {
    //                if (Double.parseDouble(s.toString()) != value) {
    //                    System.err.println("*** Round-trip failed: input " + value +
    //                            '(' + Double.doubleToLongBits(value) + ')' +
    //                            " != output " + s.toString() +
    //                            '(' + Double.doubleToLongBits(Double.parseDouble(s.toString())) + ')' );
    //                }
    //            } catch (NumberFormatException e) {
    //                System.err.println("*** Bad float " + s.toString() + " for input " + value);
    //            }
    s
  }

  /*@NotNull*/

  def appendFloat(s: FastStringBuffer,
                  f: Float,
                  forceExponential: Boolean): FastStringBuffer = {
    var f1: Float = f
    if (f1 == java.lang.Float.NEGATIVE_INFINITY) {
      s.append(NEGATIVE_INFINITY)
    } else if (f1 == java.lang.Float.POSITIVE_INFINITY) {
      s.append(POSITIVE_INFINITY)
    } else if (f1 != f1) {
      s.append(NaN)
    } else if (f1 == 0.0) {
      if ((java.lang.Float.floatToIntBits(f1) & FLOAT_SIGN_MASK) !=
        0) {
        s.cat('-')
      }
      s.cat('0')
    } else if (f1 == java.lang.Float.MAX_VALUE) {
      s.append("3.4028235E38")
    } else if (f1 == -java.lang.Float.MAX_VALUE) {
      s.append("-3.4028235E38")
    } else if (f1 == java.lang.Float.MIN_VALUE) {
      s.append("1.4E-45")
    } else if (f1 == -java.lang.Float.MIN_VALUE) {
      s.append("-1.4E-45")
    } else {
      if (f1 < 0) {
        s.cat('-')
        f1 = -f1
      }
      val bits: Int = java.lang.Float.floatToIntBits(f1)
      val fraction: Int = (1 << 23) | (bits & floatFractMask)
      val rawExp: Int = ((bits & floatExpMask) >> floatExpShift)
      val exp: Int = rawExp - floatExpBias
      val precision: Int = 23
      if (rawExp == 0) {
        // don't know how to handle this currently: hand it over to Java to deal with
        s.append(java.lang.Float.toString(f1))
        return s
      }
      if (forceExponential || (f1 >= 1000000 || f1 < 0.000001F)) {
        fppfppExponential(s, exp, fraction, precision)
      } else {
        fppfpp(s, exp, fraction, precision)
      }
    }
    s
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This is a utility class that handles formatting of numbers as strings.
 * <p>The algorithm for converting a floating point number to a string is taken from Guy L. Steele and
 * Jon L. White, <i>How to Print Floating-Point Numbers Accurately</i>, ACM SIGPLAN 1990. It is algorithm
 * (FPP)<sup>2</sup> from that paper. There are three separate implementations of the algorithm:</p>
 * <ul>
 * <li>One using long arithmetic and generating non-exponential output representations</li>
 * <li>One using BigInteger arithmetic and generating non-exponential output representation</li>
 * <li>One using BigInteger arithmetic and generating exponential output representations</li>
 * </ul>
 * <p>The choice of method depends on the value of the number being formatted.</p>
 * <p>The module contains some residual code (mainly the routine for formatting integers) from the class
 * AppenderHelper by Jack Shirazi in the O'Reilly book <i>Java Performance Tuning</i>. The floating point routines
 * in that module were found to be unsuitable, since they used floating point arithmetic which introduces
 * rounding errors.</p>
 * <p>There are several reasons for doing this conversion within Saxon, rather than leaving it all to Java.
 * Firstly, there are differences in the required output format, notably the absence of ".0" when formatting
 * whole numbers, and the different rules for the range of numbers where exponential notation is used.
 * Secondly, there are bugs in some Java implementations, for example JDK outputs 0.001 as 0.0010, and
 * IKVM/GNU gets things very wrong sometimes. Finally, this implementation is faster for "everyday" numbers,
 * though it is slower for more extreme numbers. It would probably be reasonable to hand over formatting
 * to the Java platform (at least when running the Sun JDK) for exponents outside the range -7 to +7.</p>
 */
class FloatingPointConverter {

}