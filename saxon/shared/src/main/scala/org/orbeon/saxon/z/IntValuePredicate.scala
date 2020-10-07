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
 * An IntPredicate that matches a single specific integer
 */
class IntValuePredicate(var target: Int) extends IntPredicate {
  /**
   * Ask whether a given value matches this predicate
   *
   * @param value the value to be tested
   * @return true if the value matches; false if it does not
   */
  override def test(value: Int) = value == target

  /**
   * Get the value matched by this predicate
   *
   * @return the value that this predicate matches
   */
  def getTarget = target
}