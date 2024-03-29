package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.value.AtomicValue


trait AtomicIterator[T <: AtomicValue] extends UnfailingIterator {
  def next(): T
}
