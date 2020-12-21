package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.CalendarValue

class CalendarValueComparer(@transient private var context: XPathContext)
  extends AtomicComparer {

  def getCollator(): StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer =
    new CalendarValueComparer(context)

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) return 0 else return -1
    } else if (b == null) {
      return +1
    }
    a.asInstanceOf[CalendarValue]
      .compareTo(b.asInstanceOf[CalendarValue], context.getImplicitTimezone)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  def save(): String = "CalVC"

}
