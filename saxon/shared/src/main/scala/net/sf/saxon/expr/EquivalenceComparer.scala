////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.CalendarValue

import net.sf.saxon.value.StringValue




/**
  * A comparer that compares atomic values for equality, with the properties:
  * - non-comparable types compare false
  * - NaN compares equal to NaN
  */
class EquivalenceComparer (collator: StringCollator,
                                     context: XPathContext)
    extends GenericAtomicComparer(collator, context) {

  override def provideContext(context: XPathContext): EquivalenceComparer =
    new EquivalenceComparer(getStringCollator, context)

  override def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = // System.err.println("Comparing " + a.getClass() + ": " + a + " with " + b.getClass() + ": " + b);
    if (a.isInstanceOf[StringValue] && b.isInstanceOf[StringValue]) {
      getStringCollator.comparesEqual(a.getStringValue, b.getStringValue)
    } else if (a.isInstanceOf[CalendarValue] && b
                 .isInstanceOf[CalendarValue]) {
      a.asInstanceOf[CalendarValue]
        .compareTo(b.asInstanceOf[CalendarValue],
                   getContext.getImplicitTimezone) ==
        0
    } else if (a.isNaN && b.isNaN) {
      true
    } else {
      val implicitTimezone: Int = getContext.getImplicitTimezone
      val ac: AnyRef =
        a.getXPathComparable(false, getStringCollator, implicitTimezone)
      val bc: AnyRef =
        b.getXPathComparable(false, getStringCollator, implicitTimezone)
      ac == bc
    }

  /**
    * Create a string representation of this AtomicComparer that can be saved in a compiled
    * package and used to reconstitute the AtomicComparer when the package is reloaded
    *
    * @return a string representation of the AtomicComparer
    */
  override def save(): String = "EQUIV|" + super.save()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2010-2020 Saxonica Limited
