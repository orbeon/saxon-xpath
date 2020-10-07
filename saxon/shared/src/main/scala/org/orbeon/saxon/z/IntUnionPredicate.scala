////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.z

import java.util.function.IntPredicate

/**
 * An IntPredicate formed as the union of two other predicates: it matches
 * an integer if either of the operands matches the integer
 */
class IntUnionPredicate(var p1: IntPredicate, var p2: IntPredicate) extends IntPredicate {
  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  override def test(value: Int) = p1.test(value) || p2.test(value)

  /**
   * Get the operands
   *
   * @return an array containing the two operands
   */
  def getOperands = Array[IntPredicate](p1, p2)

  override def toString = p1.toString + "||" + p2.toString
}