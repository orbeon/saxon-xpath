////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.trans.NoDynamicContextException

import org.orbeon.saxon.value.AtomicValue

import scala.beans.{BeanProperty, BooleanBeanProperty}




class DescendingComparer(@BeanProperty var baseComparer: AtomicComparer)
    extends AtomicComparer {

  def getCollator(): StringCollator = baseComparer.getCollator

  /*@NotNull*/

  def provideContext(context: XPathContext): AtomicComparer = {
    val newBase: AtomicComparer = baseComparer.provideContext(context)
    if (newBase != baseComparer) {
      new DescendingComparer(newBase)
    } else {
      this
    }
  }

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int =
    0 - baseComparer.compareAtomicValues(a, b)

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    baseComparer.comparesEqual(a, b)

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "DESC|" + baseComparer.save()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer used for comparing descending keys. This simply returns the inverse of the result
  * delivered by the base comparer.
  */
