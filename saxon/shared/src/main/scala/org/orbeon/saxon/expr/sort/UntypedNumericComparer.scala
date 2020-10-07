////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import java.lang.Long
import java.lang.Double
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Token
import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.{BuiltInAtomicType, ConversionResult}

import scala.util.control.Breaks._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._

/**
 * A specialist comparer that implements the rules for comparing an untypedAtomic value
 * (always the first operand) to a numeric value (always the second operand)
 */
object UntypedNumericComparer {
  private val bounds = Array(Array(1, 0e0, 0e1, 0e2, 0e3, 0e4, 0e5, 0e6, 0e7, 0e8, 0e9, 0e10), Array(1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), Array(1, 2e0, 2e1, 2e2, 2e3, 2e4, 2e5, 2e6, 2e7, 2e8, 2e9, 2e10), Array(1, 3e0, 3e1, 3e2, 3e3, 3e4, 3e5, 3e6, 3e7, 3e8, 3e9, 3e10), Array(1, 4e0, 4e1, 4e2, 4e3, 4e4, 4e5, 4e6, 4e7, 4e8, 4e9, 4e10), Array(1, 5e0, 5e1, 5e2, 5e3, 5e4, 5e5, 5e6, 5e7, 5e8, 5e9, 5e10), Array(1, 6e0, 6e1, 6e2, 6e3, 6e4, 6e5, 6e6, 6e7, 6e8, 6e9, 6e10), Array(1, 7e0, 7e1, 7e2, 7e3, 7e4, 7e5, 7e6, 7e7, 7e8, 7e9, 7e10), Array(1, 8e0, 8e1, 8e2, 8e3, 8e4, 8e5, 8e6, 8e7, 8e8, 8e9, 8e10), Array(1, 9e0, 9e1, 9e2, 9e3, 9e4, 9e5, 9e6, 9e7, 9e8, 9e9, 9e10), Array(1, 10e0, 10e1, 10e2, 10e3, 10e4, 10e5, 10e6, 10e7, 10e8, 10e9, 10e10))

  /**
   * Optimized routine to compare an untyped atomic value with a numeric value.
   * This attempts to deliver a quick answer if the comparison is obviously false,
   * without performing the full string-to-double conversion
   *
   * @param a0       the untypedAtomic comparand
   * @param a1       the numeric comparand
   * @param operator the comparison operator: a singleton operator such as Token.FEQ
   * @param rules    the conversion rules
   * @return the result of the comparison
   * @throws XPathException if the first operand is not convertible to a double
   */
  @throws[XPathException]
  def quickCompare(a0: UntypedAtomicValue, a1: NumericValue, operator: Int, rules: ConversionRules): Boolean = {
    val comp = quickComparison(a0, a1, rules)
    operator match {
      case Token.FEQ =>
        comp == 0
      case Token.FLE =>
        comp <= 0
      case Token.FLT =>
        comp < 0
      case Token.FGE =>
        comp >= 0
      case Token.FGT =>
        comp > 0
      case Token.FNE => false
      case _ =>
        comp != 0
    }
  }

  /**
   * Optimized routine to compare an untyped atomic value with a numeric value.
   * This attempts to deliver a quick answer if the comparison if obviously false,
   * without performing the full string-to-double conversion
   *
   * @param a0    the untypedAtomic comparand
   * @param a1    the numeric comparand
   * @param rules the conversion rules
   * @return the result of the comparison (negative if a0 lt a1, 0 if equal, positive if a0 gt a1)
   * @throws XPathException if the first operand is not convertible to a double
   */
  @throws[XPathException]
  private def quickComparison(a0: UntypedAtomicValue, a1: NumericValue, rules: ConversionRules): Int = {
    val d1 = a1.getDoubleValue
    val cs = Whitespace.trimWhitespace(a0.getStringValueCS)
    var simple = true
    var wholePartLength = 0
    var firstDigit = -1
    var decimalPoints = 0
    var sign = '?'
    breakable {
      for (i <- 0 until cs.length) {
        val c = cs.charAt(i)
        if (c >= '0' && c <= '9') {
          if (firstDigit < 0) firstDigit = c - '0'
          if (decimalPoints == 0) wholePartLength += 1
        }
        else if (c == '-') {
          if (sign != '?' || wholePartLength > 0 || decimalPoints > 0) {
            simple = false
            break()
          }
          sign = c
        }
        else if (c == '.') {
          if (decimalPoints > 0) {
            simple = false
            break()
          }
          decimalPoints = 1
        }
        else {
          simple = false
          break()
        }
      }
    }
    if (firstDigit < 0) simple = false
    if (simple && wholePartLength > 0 && wholePartLength <= 10) {
      var lowerBound = bounds(firstDigit)(wholePartLength)
      var upperBound = bounds(firstDigit + 1)(wholePartLength)
      if (sign == '-') {
        val temp = lowerBound
        lowerBound = -upperBound
        upperBound = -temp
      }
      if (upperBound < d1) return -1
      if (lowerBound > d1) return +1
    }
    if (simple && decimalPoints == 0 && wholePartLength <= 15 && a1.isInstanceOf[Int64Value]) {
      val l0 = cs.toString.toLong
      Long.compare(l0, a1.longValue)
    }
    else {
      var result: ConversionResult = null
      a0.synchronized {
        result = BuiltInAtomicType.DOUBLE.getStringConverter(rules).convertString(a0.getPrimitiveStringValue)
      }
      val av = result.asAtomic
      Double.compare(av.asInstanceOf[DoubleValue].getDoubleValue, d1)
    }
  }
}

class UntypedNumericComparer extends AtomicComparer {
  private var rules = ConversionRules.DEFAULT

  /**
   * Compare two AtomicValue objects according to the rules for their data type. UntypedAtomic
   * values are compared as if they were strings; if different semantics are wanted, the conversion
   * must be done by the caller.
   *
   * @param a the first object to be compared. It is intended that this should be an instance
   *          of AtomicValue, though this restriction is not enforced. If it is a StringValue, the
   *          collator is used to compare the values, otherwise the value must implement the java.util.Comparable
   *          interface.
   * @param b the second object to be compared. This must be comparable with the first object: for
   *          example, if one is a string, they must both be strings.
   * @return &lt;0 if a&lt;b, 0 if a=b, &gt;0 if a&gt;b
   * @throws ClassCastException if the objects are not comparable
   */
  override def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = try UntypedNumericComparer.quickComparison(a.asInstanceOf[UntypedAtomicValue], b.asInstanceOf[NumericValue], rules)
  catch {
    case e: XPathException =>
      throw new ComparisonException(e)
  }

  /**
   * Get the collation used by this AtomicComparer if any
   *
   * @return the collation used for comparing strings, or null if not applicable
   */
  override def getCollator: StringCollator = null

  /**
   * Supply the dynamic context in case this is needed for the comparison
   *
   * @param context the dynamic evaluation context
   * @return either the original AtomicComparer, or a new AtomicComparer in which the context
   *         is known. The original AtomicComparer is not modified
   */
  override def provideContext(context: XPathContext): UntypedNumericComparer = {
    rules = context.getConfiguration.getConversionRules
    this
  }

  /**
   * Compare two AtomicValue objects for equality according to the rules for their data type. UntypedAtomic
   * values are compared by converting to the type of the other operand.
   *
   * @param a the first object to be compared.
   * @param b the second object to be compared.
   * @return true if the values are equal, false if not
   * @throws ClassCastException if the objects are not comparable
   */
  override def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = compareAtomicValues(a, b) == 0

  /**
   * Create a string representation of this AtomicComparer that can be saved in a compiled
   * package and used to reconstitute the AtomicComparer when the package is reloaded
   *
   * @return a string representation of the AtomicComparer
   */
  override def save: String = "QUNC"
}