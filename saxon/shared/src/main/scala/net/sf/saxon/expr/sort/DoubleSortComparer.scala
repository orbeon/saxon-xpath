////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.NumericValue

import DoubleSortComparer._




object DoubleSortComparer {

  private var THE_INSTANCE: DoubleSortComparer = new DoubleSortComparer()

  def getInstance(): DoubleSortComparer = THE_INSTANCE

}

class DoubleSortComparer private () extends AtomicComparer {

  /*@Nullable*/

  def getCollator(): StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) {
        return  0
      } else {
        return -1
      }
    } else if (b == null) {
      return +1
    }
    val an: NumericValue = a.asInstanceOf[NumericValue]
    val bn: NumericValue = b.asInstanceOf[NumericValue]
    if (an.isNaN) {
      if (bn.isNaN) return 0 else return -1
    } else if (bn.isNaN) {
      return +1
    }
    an.compareTo(bn)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "DblSC"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An AtomicComparer used for sorting values that are known to be numeric.
  * It also supports a separate method for getting a collation key to test equality of items.
  * This comparator treats NaN values as equal to each other, and less than any other value.
  *
  */
