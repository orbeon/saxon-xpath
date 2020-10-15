////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.{lang => jl}

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.AtomicValue


trait AtomicSequence
    extends GroundedValue
    with jl.Iterable[AtomicValue] {

  def head: AtomicValue
  def iterate(): AtomicIterator[_]
  def itemAt(n: Int): AtomicValue
  def getLength: Int
  def getCanonicalLexicalRepresentation: CharSequence
  def getSchemaComparable: Comparable[_]
  def getStringValueCS: CharSequence
  def getStringValue: String
}