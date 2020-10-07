package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.value.AtomicValue

class CollatingAtomicComparer(collator: StringCollator) extends AtomicComparer {

  var strCollator: StringCollator = if (collator == null) CodepointCollator.getInstance else collator

  def provideContext(context: XPathContext): AtomicComparer = this

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int = {
    if (a == null) {
      if (b == null) return 0 else return -1
    } else if (b == null) return +1
    collator.compareStrings(a.getStringValue, b.getStringValue)
  }

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean =
    compareAtomicValues(a, b) == 0

  def save(): String = "CAC|" + getCollator.getCollationURI

  override def hashCode: Int = collator.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[CollatingAtomicComparer] && collator.equals(obj.asInstanceOf[CollatingAtomicComparer])

  override def getCollator(): StringCollator = strCollator
}
