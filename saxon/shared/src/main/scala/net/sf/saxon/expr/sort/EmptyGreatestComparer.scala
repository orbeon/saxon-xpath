////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.value.AtomicValue

import scala.beans.{BeanProperty, BooleanBeanProperty}




class EmptyGreatestComparer(@BeanProperty var baseComparer: AtomicComparer)
    extends AtomicComparer {

  def getCollator(): StringCollator = baseComparer.getCollator

  def provideContext(context: XPathContext): AtomicComparer = {
    val newBase: AtomicComparer = baseComparer.provideContext(context)
    if (newBase != baseComparer) {
      new EmptyGreatestComparer(newBase)
    } else {
      this
    }
  }

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) {
        return 0
      } else {
        return +1
      }
    } else if (b == null) {
      return -1
    }
    if (a.isNaN) {
      if (b.isNaN) return 0 else return +1
    } else if (b.isNaN) {
      return -1
    }
    baseComparer.compareAtomicValues(a, b)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    (a == null && b == null) || baseComparer.comparesEqual(a, b)

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "EG|" + baseComparer.save()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer that modifies a base comparer by sorting empty key values and NaN values last (greatest),
  * as opposed to the default which sorts them first.
  */
