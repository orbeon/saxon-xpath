////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex.charclass

import net.sf.saxon.z.IntSet

import net.sf.saxon.z.IntSingletonSet

import scala.beans.{BeanProperty, BooleanBeanProperty}




class SingletonCharacterClass(@BeanProperty var codepoint: Int)
    extends CharacterClass {

  def test(value: Int): Boolean = value == codepoint

  def isDisjoint(other: CharacterClass): Boolean = !other.test(codepoint)

  def getIntSet(): IntSet = new IntSingletonSet(codepoint)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. A singleton
  * character class matches exactly one character
  */
