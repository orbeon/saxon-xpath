package org.orbeon.saxon.om

import org.orbeon.saxon.tree.iter.AtomicIterator

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.value.AtomicValue

import java.util.Collections

import java.util.Iterator

object EmptyAtomicSequence extends Enumeration {

  val INSTANCE: EmptyAtomicSequence = new EmptyAtomicSequence()

  class EmptyAtomicSequence extends Val with AtomicSequence {

    def head: AtomicValue = null

    def iterate(): AtomicIterator[AtomicValue] = EmptyIterator.ofAtomic()

    def itemAt(n: Int): AtomicValue = null

    def getLength: Int = 0

    def getCanonicalLexicalRepresentation(): CharSequence = ""

    def getSchemaComparable(): Comparable[_] = null

    def getStringValueCS: CharSequence = ""

    def getStringValue: String = ""

    def subsequence(start: Int, length: Int): EmptyAtomicSequence = this

    override def effectiveBooleanValue: Boolean = false

    override def reduce(): EmptyAtomicSequence = this

    def iterator: Iterator[AtomicValue] =
      Collections.emptyList[AtomicValue]().iterator

  }

  def getInstance: EmptyAtomicSequence = INSTANCE

  implicit def convertValue(v: Value): EmptyAtomicSequence =
    v.asInstanceOf[EmptyAtomicSequence]

}
