package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.CalendarValue

class CalendarValueComparer(@transient private var context: XPathContext)
  extends AtomicComparer {

  def getCollator(): StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer =
    new CalendarValueComparer(context)

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) 0 else -1
    } else if (b == null) {
      +1
    }
    a.asInstanceOf[CalendarValue]
      .compareTo(b.asInstanceOf[CalendarValue], context.getImplicitTimezone)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  def save(): String = "CalVC"

}
