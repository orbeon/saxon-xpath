package org.orbeon.saxon.om


/**
 * This interface represents an XDM Value, that is, a sequence of items.
 *
 * Note that different implementations of Sequence might have very different
 * performance characteristics, though all should exhibit the same behaviour.
 * With some sequences, calling iterate() may trigger evaluation of the logic
 * that computes the sequence, and calling iterate() again may cause re-evaluation.
 *
 * Users should avoid assuming that a sequence of length one will always
 * be represented as an instance of Item. If you are confident that the sequence
 * will be of length one, call the head() function to get the first item.
 *
 * @since 9.5. Generified in 9.9. Generics dropped in 10.0.
 */
trait Sequence {

  def head: Item
  def iterate(): SequenceIterator

  def makeRepeatable(): Sequence = this
  def materialize: GroundedValue = iterate().materialize
}
