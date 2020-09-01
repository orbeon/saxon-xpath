////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import DecimalSortComparer._




object DecimalSortComparer {

  private var THE_INSTANCE: DecimalSortComparer = new DecimalSortComparer()

  def getDecimalSortComparerInstance: DecimalSortComparer = THE_INSTANCE

}

class DecimalSortComparer private () extends ComparableAtomicValueComparer {

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  override def save(): String = "DecSC"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An AtomicComparer used for sorting values that are known to be instances of xs:decimal (including xs:integer),
  * It also supports a separate method for getting a collation key to test equality of items
  */
