////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import scala.beans.{BeanProperty, BooleanBeanProperty}




class TextComparer(@BeanProperty var baseComparer: AtomicComparer)
    extends AtomicComparer {

  def getCollator(): StringCollator = baseComparer.getCollator

  def provideContext(context: XPathContext): AtomicComparer = {
    val newBase: AtomicComparer = baseComparer.provideContext(context)
    if (newBase != baseComparer) {
      new TextComparer(newBase)
    } else {
      this
    }
  }

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int =
    baseComparer.compareAtomicValues(toStringValue(a), toStringValue(b))

  private def toStringValue(a: AtomicValue): StringValue =
    if (a.isInstanceOf[StringValue]) {
      a.asInstanceOf[StringValue]
    } else {
      new StringValue(if (a == null) "" else a.getStringValue)
    }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  def save(): String = "TEXT|" + baseComparer.save()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Comparer used for comparing sort keys when data-type="text". The items to be
  * compared are converted to strings, and the strings are then compared using an
  * underlying collator
  *
  * @author Michael H. Kay
  */
