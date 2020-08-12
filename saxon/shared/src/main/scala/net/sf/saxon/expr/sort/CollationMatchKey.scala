package net.sf.saxon.expr.sort

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Base64BinaryValue

import java.text.CollationKey

class CollationMatchKey(private var key: CollationKey) extends AtomicMatchKey with Comparable[CollationMatchKey] {

  override def compareTo(o: CollationMatchKey): Int =
    if (o.isInstanceOf[CollationMatchKey]) {
      key.compareTo(o.key)
    } else {
      throw new ClassCastException()
    }

  override def hashCode(): Int = key.hashCode

  override def equals(o: Any): Boolean = o match {
    case o: CollationMatchKey => key == o.key
    case _ => false

  }

  def asAtomic(): AtomicValue = new Base64BinaryValue(key.toByteArray())
}
