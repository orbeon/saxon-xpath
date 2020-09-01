////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex.charclass

import net.sf.saxon.z.IntEmptySet

import net.sf.saxon.z.IntSet

import EmptyCharacterClass._




object EmptyCharacterClass {

  private val THE_INSTANCE: EmptyCharacterClass = new EmptyCharacterClass()

  private val COMPLEMENT: InverseCharacterClass = new InverseCharacterClass(
    THE_INSTANCE)

  def getInstance: EmptyCharacterClass = THE_INSTANCE

  def getComplement: CharacterClass = COMPLEMENT

}

class EmptyCharacterClass private () extends CharacterClass {

  def test(value: Int): Boolean = false

  def isDisjoint(other: CharacterClass): Boolean = // intersection of the two sets is empty
    true

  override def getIntSet(): IntSet = IntEmptySet.getInstance

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. The empty
  * character class matches no characters
  */
