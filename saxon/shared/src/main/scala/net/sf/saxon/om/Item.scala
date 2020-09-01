////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An Item is an object that can occur as a member of a sequence.
  * It corresponds directly to the concept of an item in the XPath 2.0 data model.
  * There are four kinds of Item: atomic values, nodes, function items, and external objects.
  * <p>This interface is part of the public Saxon API. As such (starting from Saxon 8.4),
  * methods that form part of the stable API are labelled with a JavaDoc "since" tag
  * to identify the Saxon release at which they were introduced.</p>
  * <p>Note: there is no method getItemType(). This is to avoid having to implement it
  * on every implementation of NodeInfo. Instead, use the static method Type.getItemType(Item).</p>
  *
  */

package net.sf.saxon.om

import net.sf.saxon.om.Genre.Genre
import net.sf.saxon.tree.iter.SingletonIterator
import net.sf.saxon.value.EmptySequence


object Item {
  def toGroundedValue(item: Item): GroundedValue = item.reduce()
}

trait Item extends GroundedValue {

  def getGenre: Genre
  def head: Item = this
  def getStringValue: String
  def getStringValueCS: CharSequence
  def atomize(): AtomicSequence

  override def toShortString: String = toString

  /**
    * Get the n'th item in the value, counting from 0
    *
    * @param n the index of the required item, with 0 representing the first item in the sequence
    * @return the n'th item if it exists, or null otherwise
    */
  def itemAt(n: Int): Item = if (n == 0) head else null

  /**
    * Get a subsequence of the value
    *
    * @param start  the index of the first item to be included in the result, counting from zero.
    *               A negative value is taken as zero. If the value is beyond the end of the sequence, an empty
    *               sequence is returned
    * @param length the number of items to be included in the result. Specify Integer.MAX_VALUE to
    *               get the subsequence up to the end of the base sequence. If the value is negative, an empty sequence
    *               is returned. If the value goes off the end of the sequence, the result returns items up to the end
    *               of the sequence
    * @return the required subsequence. If min is
    */
  def subsequence(start: Int, length: Int): GroundedValue =
    if (start <= 0 && (start + length) > 0) this else EmptySequence.getInstance

  /**
    * Get the size of the value (the number of items)
    *
    * @return the number of items in the sequence. Note that for a single item, including a map or array,
    * the result is always 1 (one).
    */
  def getLength: Int = 1

  /**
    * Get an iterator over all the items in the sequence
    *
    * @return an iterator over all the items
    */
  def iterate(): SingletonIterator[_ <: Item] = new SingletonIterator(this)

  /**
    * Reduce the sequence to its simplest form. If the value is an empty sequence, the result will be
    * EmptySequence.getInstance(). If the value is a single atomic value, the result will be an instance
    * of AtomicValue. If the value is a single item of any other kind, the result will be an instance
    * of Item. Otherwise, the result will typically be unchanged.
    *
    * @return the simplified sequence
    */
  override def reduce(): GroundedValue = this

  def isStreamed: Boolean = false

}
