package org.orbeon.saxon.om


trait Sequence {
  def head: Item
  def iterate(): SequenceIterator
  def materialize(): GroundedValue = iterate().materialize()
  def makeRepeatable(): Sequence = this
}
