////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr._

/**
 * This class contains static methods to manipulate the cardinality
 * property of a type.
 * Cardinality of expressions is denoted by one of the values ONE_OR_MORE, ZERO_OR_MORE,
 * ZERO_OR_ONE, EXACTLY_ONE, or EMPTY. These are combinations of the three bit-significant
 * values ALLOWS_ZERO, ALLOWS_ONE, and ALLOWS_MANY.
 */
object Cardinality {
  /**
   * Determine whether multiple occurrences are allowed
   *
   * @param cardinality the cardinality of a sequence
   * @return true if the cardinality allows the sequence to contain more than one item
   */
    def allowsMany(cardinality: Int) = (cardinality & StaticProperty.ALLOWS_MANY) != 0

  /**
   * Determine whether multiple occurrences are not only allowed, but likely.
   * This returns false for an expression that is the atomization of a singleton
   * node, since in that case it's quite unusual for the typed value to be a
   * non-singleton sequence.
   *
   * @param expression an expression
   * @return true if multiple occurrences are not only allowed, but likely. Return
   *         false if multiple occurrences are unlikely, even though they might be allowed.
   *         This is typically the case for the atomized sequence that is obtained by atomizing
   *         a singleton node.
   */
  def expectsMany(expression: Expression): Boolean = {
    if (expression.isInstanceOf[VariableReference]) {
      val b = expression.asInstanceOf[VariableReference].getBinding
      if (b.isInstanceOf[LetExpression]) return expectsMany(b.asInstanceOf[LetExpression].getSequence)
    }
    if (expression.isInstanceOf[Atomizer]) return expectsMany(expression.asInstanceOf[Atomizer].getBaseExpression)
    if (expression.isInstanceOf[FilterExpression]) return expectsMany(expression.asInstanceOf[FilterExpression].getSelectExpression)
    allowsMany(expression.getCardinality)
  }

  /**
   * Determine whether empty sequence is allowed
   *
   * @param cardinality the cardinality of a sequence
   * @return true if the cardinality allows the sequence to be empty
   */
  def allowsZero(cardinality: Int): Boolean = (cardinality & StaticProperty.ALLOWS_ZERO) != 0

  /**
   * Form the union of two cardinalities. The cardinality of the expression "if (c) then e1 else e2"
   * is the union of the cardinalities of e1 and e2.
   *
   * @param c1 a cardinality
   * @param c2 another cardinality
   * @return the cardinality that allows both c1 and c2
   */
  def union(c1: Int, c2: Int) = {
    var r = c1 | c2
    // eliminate disallowed options
    if (r == (StaticProperty.ALLOWS_MANY | StaticProperty.ALLOWS_ZERO)) r = StaticProperty.ALLOWS_ZERO_OR_MORE
    r
  }

  /**
   * Add two cardinalities
   *
   * @param c1 the first cardinality
   * @param c2 the second cardinality
   * @return the cardinality of a sequence formed by concatenating the sequences whose cardinalities
   *         are c1 and c2
   */
  def sum(c1: Int, c2: Int) = {
    val minimum = min(c1) + min(c2)
    val maximun = max(c1) + max(c2)
    fromMinAndMax(minimum, maximun)
  }

  /**
   * Get the minimum number of occurrences permitted by a given cardinality
   *
   * @param cardinality the cardinality code
   * @return the minimum size of a sequence that satisfies this cardinality
   */
  private[value] def min(cardinality: Int) = if (allowsZero(cardinality)) 0
  else if (cardinality == StaticProperty.ALLOWS_MANY) 2
  else 1

  /**
   * Get the maximum number of occurrences permitted by a given cardinality
   *
   * @param cardinality the cardinality code
   * @return the maximum size of a sequence that satisfies this cardinality,
   *         where 2 represents infinity.
   */
  private[value] def max(cardinality: Int) = if (allowsMany(cardinality)) 2
  else if (cardinality == StaticProperty.ALLOWS_ZERO) 0
  else 1

  private[value] def fromMinAndMax(min: Int, max: Int) = {
    val zero = min == 0
    val one = min <= 1 || max <= 1
    val many = max > 1
    (if (zero) StaticProperty.ALLOWS_ZERO
    else 0) + (if (one) StaticProperty.ALLOWS_ONE
    else 0) + (if (many) StaticProperty.ALLOWS_MANY
    else 0)
  }

  /**
   * Test if one cardinality subsumes another. Cardinality c1 subsumes c2 if every option permitted
   * by c2 is also permitted by c1.
   *
   * @param c1 a cardinality
   * @param c2 another cardinality
   * @return true if if every option permitted
   *         by c2 is also permitted by c1.
   */
  def subsumes(c1: Int, c2: Int) = (c1 | c2) == c1

  /**
   * Multiply two cardinalities
   *
   * @param c1 the first cardinality
   * @param c2 the second cardinality
   * @return the product of the cardinalities, that is, the cardinality of the sequence
   *         "for $x in S1 return S2", where c1 is the cardinality of S1 and c2 is the cardinality of S2
   */
  def multiply(c1: Int, c2: Int): Int = {
    if (c1 == StaticProperty.EMPTY || c2 == StaticProperty.EMPTY) return StaticProperty.EMPTY
    if (c2 == StaticProperty.EXACTLY_ONE) return c1
    if (c1 == StaticProperty.EXACTLY_ONE) return c2
    if (c1 == StaticProperty.ALLOWS_ZERO_OR_ONE && c2 == StaticProperty.ALLOWS_ZERO_OR_ONE) return StaticProperty.ALLOWS_ZERO_OR_ONE
    if (c1 == StaticProperty.ALLOWS_ONE_OR_MORE && c2 == StaticProperty.ALLOWS_ONE_OR_MORE) return StaticProperty.ALLOWS_ONE_OR_MORE
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  /**
   * Display the cardinality as a string
   *
   * @param cardinality the cardinality value to be displayed
   * @return the representation as a string, for example "zero or one", "zero or more"
   */
  def toString(cardinality: Int) = cardinality match {
    case StaticProperty.ALLOWS_ZERO_OR_ONE =>
      "zero or one"
    case StaticProperty.EXACTLY_ONE =>
      "exactly one"
    case StaticProperty.ALLOWS_ZERO_OR_MORE =>
      "zero or more"
    case StaticProperty.ALLOWS_ONE_OR_MORE =>
      "one or more"
    case StaticProperty.EMPTY =>
      "exactly zero"
    case StaticProperty.ALLOWS_MANY =>
      "more than one"
    case _ =>
      "code " + cardinality
  }

  /**
   * Get the occurence indicator representing the cardinality
   *
   * @param cardinality the cardinality value
   * @return the occurrence indicator, for example "*", "+", "?", "".
   */
  /*@NotNull*/ def getOccurrenceIndicator(cardinality: Int) = cardinality match {
    case StaticProperty.ALLOWS_ZERO_OR_ONE =>
      "?"
    case StaticProperty.EXACTLY_ONE =>
      ""
    case StaticProperty.ALLOWS_ZERO_OR_MORE =>
      "*"
    case StaticProperty.ALLOWS_ONE_OR_MORE =>
      "+"
    case StaticProperty.ALLOWS_MANY =>
      "+"
    case StaticProperty.EMPTY =>
      "0"
    case _ =>
      "*"
    // Covers no bits set (which shouldn't arise) and zero_or_many (which can arise, but is too specific for our purposes)
  }

  def fromOccurrenceIndicator(indicator: String) = indicator match {
    case "?" =>
      StaticProperty.ALLOWS_ZERO_OR_ONE
    case "*" =>
      StaticProperty.ALLOWS_ZERO_OR_MORE
    case "+" =>
      StaticProperty.ALLOWS_ONE_OR_MORE
    case "1" =>
      StaticProperty.ALLOWS_ONE
    case "" =>
      StaticProperty.ALLOWS_ONE
    case "\u00B0" => // TODO: obsolescent, delete this
    case "0" =>
    case _ =>
      StaticProperty.ALLOWS_ZERO
  }

  /**
   * Generate Javascript code to check whether a number satisfies the cardinality property.
   *
   * @param card the cardinality value
   * @return a Javascript function which checks whether a number satisfies the cardinality property.
   */
  def generateJavaScriptChecker(card: Int) = if (Cardinality.allowsZero(card) && Cardinality.allowsMany(card)) "function c() {return true;};"
  else if (card == StaticProperty.EXACTLY_ONE) "function c(n) {return n==1;};"
  else if (card == StaticProperty.EMPTY) "function c(n) {return n==0;};"
  else if (!Cardinality.allowsZero(card)) "function c(n) {return n>=1;};"
  else "function c(n) {return n<=1;};"
}


/**
 * Private constructor: no instances allowed
 */
final class Cardinality private(){
}