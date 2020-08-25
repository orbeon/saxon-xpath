////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.model.StringToDouble

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.NumericValue

import NumericComparer._




object NumericComparer {

  private var THE_INSTANCE: NumericComparer = new NumericComparer()

  def getInstance(): NumericComparer = THE_INSTANCE

}

class NumericComparer  () extends AtomicComparer {

   var converter: StringToDouble = StringToDouble.getInstance

  /*@Nullable*/

  def getCollator(): StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    var d1: Double = 0.0
    var d2: Double = 0.0
    if (a.isInstanceOf[NumericValue]) {
      d1 = a.asInstanceOf[NumericValue].getDoubleValue
    } else if (a == null) {
      d1 = java.lang.Double.NaN
    } else {
      try d1 = converter.stringToNumber(a.getStringValueCS)
      catch {
        case err: NumberFormatException => d1 = java.lang.Double.NaN

      }
    }
    if (b.isInstanceOf[NumericValue]) {
      d2 = b.asInstanceOf[NumericValue].getDoubleValue
    } else if (b == null) {
      d2 = java.lang.Double.NaN
    } else {
      try d2 = converter.stringToNumber(b.getStringValueCS)
      catch {
        case err: NumberFormatException => d2 = java.lang.Double.NaN

      }
    }
    if (java.lang.Double.isNaN(d1)) {
      if (java.lang.Double.isNaN(d2)) {
        return  0
      } else {
        return -1
      }
    }
    if (java.lang.Double.isNaN(d2)) {
      return +1
    }
    if (d1 < d2) {
      return -1
    }
    if (d1 > d2) {
      return +1
    }
    0
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "NC"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer used for comparing sort keys when data-type="number". The items to be
  * compared are converted to numbers, and the numbers are then compared directly. NaN values
  * compare equal to each other, and equal to an empty sequence, but less than anything else.
  * <p>This class is used in XSLT only, so there is no need to handle XQuery's "empty least" vs
  * "empty greatest" options.</p>
  */
