////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.arrays

import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.z.IntSet

import scala.jdk.CollectionConverters._

/**
 * Implementation of ArrayItem backed by a persistent immutable array, so that operations
 * that "update" the array do not have to copy the whole array
 */
class ImmutableArrayItem(val vector: Vector[GroundedValue]) extends AbstractArrayItem {

  def this(other: SimpleArrayItem) =
    this(other.getMembersList.asScala.toVector)

  /**
   * Get a member of the array
   *
   * @param index the position of the member to retrieve (zero-based)
   * @return the value at the given position.
   * @throws IndexOutOfBoundsException if the index is out of range
   */
  def get(index: Int): GroundedValue = vector(index)

  /**
   * Replace a member of the array
   *
   * @param index    the position of the member to replace (zero-based)
   * @param newValue the replacement value
   * @return the value at the given position.
   * @throws IndexOutOfBoundsException if the index is out of range
   */
  def put(index: Int, newValue: GroundedValue): ArrayItem = {
    val v2 = vector.updated(index, newValue)
    if (v2 eq vector)
      this
    else
      new ImmutableArrayItem(v2)
  }

  /**
   * Insert a new member into an array
   *
   * @param position the 0-based position that the new item will assume
   * @param member   the new member to be inserted
   * @return a new array item with the new member inserted
   * @throws IndexOutOfBoundsException if position is out of range
   */
  def insert(position: Int, member: GroundedValue): ArrayItem = {
    if (position < 0 || position > vector.size)
      throw new IndexOutOfBoundsException(s"Requested $position, actual size ${vector.size}")
    val v2 = vector.patch(position, Iterator.single(member), 0)
    new ImmutableArrayItem(v2)
  }

  /**
   * Get the number of members in the array
   * <p>
   * <p>Note: the {@link #getLength method always returns 1, because an array is an item}</p>
   *
   * @return the number of members in this array.
   */
  def arrayLength(): Int = vector.size

  /**
   * Ask whether the array is empty
   *
   * @return true if and only if the size of the array is zero
   */
  def isEmpty: Boolean = vector.isEmpty

  /**
   * Get the list of all members of the array
   *
   * @return an iterator over the members of the array
   */
  def members: Iterable[GroundedValue] = vector

  /**
   * Get a subarray given a start and end position
   *
   * @param start the start position (zero based)
   * @param end   the end position (the position of the first item not to be returned)
   *              (zero based)
   * @throws IndexOutOfBoundsException if start, or start+end, is out of range
   */
  def subArray(start: Int, end: Int): ArrayItem = {
    if (start < 0 || start > vector.size || end < start || end > vector.size)
      throw new IndexOutOfBoundsException(s"start: $start, end: $end, size: ${vector.size}")
    new ImmutableArrayItem(vector.slice(start, end))
  }

  /**
   * Concatenate this array with another
   *
   * @param other the second array
   * @return the concatenation of the two arrays; that is, an array
   *         containing first the members of this array, and then the members of the other array
   */
  def concat(other: ArrayItem): ArrayItem = {
    if (other.arrayLength() == 0)
      return this
    val v1 =
      other match {
        case arrayItem: ImmutableArrayItem => arrayItem.vector
        case _                             => new ImmutableArrayItem(other.asInstanceOf[SimpleArrayItem]).vector
      }
    val v2 = vector ++ v1
    new ImmutableArrayItem(v2)
  }

  /**
   * Remove a member from the array
   *
   * @param index the position of the member to be removed (zero-based)
   * @return a new array in which the requested member has been removed.
   * @throws IndexOutOfBoundsException if index is out of range
   */
  def remove(index: Int): ArrayItem = {
    if (index < 0 || index >= vector.size)
      throw new IndexOutOfBoundsException(s"Requested $index, actual size ${vector.size}")
    val v2 = vector.patch(index, Nil, 1)
    if (v2 eq vector)
      this
    else
      new ImmutableArrayItem(v2)
  }

  /**
   * Remove zero or more members from the array
   *
   * @param positions the positions of the members to be removed (zero-based).
   *                  A value that is out of range is ignored.
   * @return a new array in which the requested member has been removed
   */
  def removeSeveral(positions: IntSet): ArrayItem = {
    val p = Array.ofDim[Int](positions.size)
    var i = 0
    val ii = positions.iterator
    while (ii.hasNext) {
      p(i) = ii.next()
      i += 1
    }
    scala.util.Sorting.quickSort(p)
    var v2 = vector
    var j  = p.length - 1
    while (j >= 0) {
      val idx = p(j)
      if (idx < 0 || idx >= v2.size)
        throw new IndexOutOfBoundsException(s"Requested $idx, actual size ${v2.size}")
      v2 = v2.patch(idx, Nil, 1)
      j = j - 1
    }
    if (v2 eq vector)
      this
    else
      new ImmutableArrayItem(v2)
  }
}
