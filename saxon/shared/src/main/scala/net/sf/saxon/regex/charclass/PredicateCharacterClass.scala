////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex.charclass

import net.sf.saxon.z.IntSet

import java.util.function.IntPredicate




class PredicateCharacterClass(private var predicate: IntPredicate)
    extends CharacterClass {

  def test(value: Int): Boolean = predicate.test(value)

  def isDisjoint(other: CharacterClass): Boolean =
    other.isInstanceOf[InverseCharacterClass] && other.isDisjoint(this)

  def getIntSet(): IntSet = // Not known
    null

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. A predicate
  * character class is one where the determination of membership is established by executing
  * a function.
  */
