////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.value.AtomicValue


object EqualityComparer {
  val getInstance: EqualityComparer = new EqualityComparer
}

/**
  * A comparer for comparing two atomic values where (a) equals is defined, and is implemented
  * using the Java equals() method, and (b) ordering is not defined, and results in a dynamic error.
  */
class EqualityComparer private () extends AtomicComparer {

  /*@Nullable*/
  def getCollator: StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int =
    throw new ClassCastException("Values are not comparable")

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = a == b

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "EQC"
}
