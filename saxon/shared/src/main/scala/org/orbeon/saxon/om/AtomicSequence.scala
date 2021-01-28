package org.orbeon.saxon.om

import java.{lang => jl}

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.AtomicValue


/**
 * Interface representing a sequence of atomic values. This is often used to represent the
 * typed value of a node. In most cases the typed value of a node is a single atomic value,
 * so the class AtomicValue implements this interface.
 *
 * An AtomicSequence is always represented as a GroundedValue: that is, the entire sequence
 * is in memory, making operations such as {@link #itemAt ( int )} and {@link #getLength} possible.
 */
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