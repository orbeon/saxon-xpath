////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex.charclass

import org.orbeon.saxon.z.IntSet

import scala.beans.{BeanProperty, BooleanBeanProperty}




class IntSetCharacterClass(@BeanProperty var intSet: IntSet)
    extends CharacterClass {

  def test(value: Int): Boolean = intSet.contains(value)

  def isDisjoint(other: CharacterClass): Boolean =
    if (other.isInstanceOf[IntSetCharacterClass]) {
      intSet.intersect(other.asInstanceOf[IntSetCharacterClass].intSet).isEmpty
    } else if (other.isInstanceOf[InverseCharacterClass]) {
      other.isDisjoint(this)
    } else {
      false
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A character class represents a set of characters for regex matching purposes. An
  * IntSetCharacterClass is a character class represented by a set of integer
  * codepoints
  */
