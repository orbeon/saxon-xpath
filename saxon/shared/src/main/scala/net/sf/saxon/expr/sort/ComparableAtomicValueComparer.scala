package net.sf.saxon.expr.sort

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.value.AtomicValue

object ComparableAtomicValueComparer {

  private var THE_INSTANCE: ComparableAtomicValueComparer =
    new ComparableAtomicValueComparer()

  def getInstance(): ComparableAtomicValueComparer = THE_INSTANCE

}

class ComparableAtomicValueComparer  () extends AtomicComparer {

  def getCollator(): StringCollator = null

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) return 0 else return -1
    } else if (b == null) return +1
    a.asInstanceOf[Comparable[AtomicValue]].compareTo(b)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean = a == b

  def save(): String = "CAVC"

}
