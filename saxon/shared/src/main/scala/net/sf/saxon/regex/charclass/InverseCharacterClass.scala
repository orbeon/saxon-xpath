////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex.charclass

import net.sf.saxon.z.IntComplementSet

import net.sf.saxon.z.IntSet

import scala.beans.{BeanProperty, BooleanBeanProperty}




class InverseCharacterClass(@BeanProperty var complement: CharacterClass)
    extends CharacterClass {

  def test(value: Int): Boolean = !complement.test(value)

  def isDisjoint(other: CharacterClass): Boolean = other == complement

  override def getIntSet(): IntSet = {
    val comp: IntSet = complement.getIntSet
    if (comp == null) null else new IntComplementSet(complement.getIntSet)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. An inverse
  * character class is the complement of another character class
  */
