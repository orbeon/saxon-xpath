////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import java.util.Collections

import org.orbeon.saxon.om.{GroundedValue, Item}
import org.orbeon.saxon.tree.iter.{EmptyIterator, UnfailingIterator}


/**
  * An EmptySequence object represents a sequence containing no members.
  */
object EmptySequence extends GroundedValue {

  // ORBEON: For legacy callers.
  def getInstance[T <: Item]: EmptySequence.type = this

  def getStringValue: String = ""

  var a: Collections = _

  def getStringValueCS: CharSequence = ""

  /**
    * Get the first item in the sequence.
    *
    * @return the first item in the sequence if there is one, or null if the sequence
    *         is empty
    */
  def head: Item = null

  /*@NotNull*/
  def iterate(): UnfailingIterator = EmptyIterator.emptyIterator

  /*@Nullable*/
  def asItem(): Item = null

  def getLength: Int = 0

  override def effectiveBooleanValue: Boolean = false

  /*@Nullable*/
  def itemAt(n: Int): Item = null

  /*@NotNull*/
  def subsequence(min: Int, length: Int): GroundedValue = this

  /*@NotNull*/
  override def toString: String = "()"

  /**
    * Reduce the sequence to its simplest form. If the value is an empty sequence, the result will be
    * EmptySequence.getInstance(). If the value is a single atomic value, the result will be an instance
    * of AtomicValue. If the value is a single item of any other kind, the result will be an instance
    * of SingletonItem. Otherwise, the result will typically be unchanged.
    *
    * @return the simplified sequence
    */
  override def reduce(): GroundedValue = this
}
