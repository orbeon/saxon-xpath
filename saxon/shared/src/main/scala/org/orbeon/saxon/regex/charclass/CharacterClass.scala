////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex.charclass

import org.orbeon.saxon.z.IntSet

import java.util.function.IntPredicate




trait CharacterClass extends IntPredicate {

  def isDisjoint(other: CharacterClass): Boolean

  def getIntSet: IntSet

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. It extends IntPredicate,
  * so there is a mechanism for testing whether a particular codepoint is a member of the class. In
  * addition it provides a method for testing whether two classes are disjoint, which is used when
  * optimizing regular expressions.
  */
