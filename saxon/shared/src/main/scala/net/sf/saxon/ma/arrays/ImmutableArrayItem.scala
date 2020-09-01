////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.arrays

import java.util.Arrays

import net.sf.saxon.ma.parray.ImmList
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.z.{IntIterator, IntSet}

//remove if not needed

class ImmutableArrayItem extends AbstractArrayItem {
  var other: SimpleArrayItem = _
  private var vector: ImmList[GroundedValue] = ImmList.fromList(other.getMembersList)

  def this(other: SimpleArrayItem) {
    this()
    this.other = other
  }

  def this(vector: ImmList[GroundedValue]) = {
    this()
    this.vector = vector
  }

  /**
   * Get a member of the array
   *
   * @param index the position of the member to retrieve (zero-based)
   * @return the value at the given position.
   * @throws IndexOutOfBoundsException if the index is out of range
   */
  override def get(index: Int): GroundedValue = vector.get(index)

  /**
   * Replace a member of the array
   *
   * @param index    the position of the member to replace (zero-based)
   * @param newValue the replacement value
   * @return the value at the given position.
   * @throws IndexOutOfBoundsException if the index is out of range
   */
  override def put(index: Int, newValue: GroundedValue): ArrayItem = {
    val v2: ImmList[GroundedValue] = vector.replace(index, newValue)
    if (v2 == vector) this else new ImmutableArrayItem(v2)
  }

  /**
   * Insert a new member into an array
   *
   * @param position the 0-based position that the new item will assume
   * @param member   the new member to be inserted
   * @return a new array item with the new member inserted
   * @throws IndexOutOfBoundsException if position is out of range
   */
  override def insert(position: Int, member: GroundedValue): ArrayItem = {
    val v2: ImmList[GroundedValue] = vector.insert(position, member)
    new ImmutableArrayItem(v2)
  }

  /**
   * Get the number of members in the array
   * <p>
   * <p>Note: the {@link #getLength method always returns 1, because an array is an item}</p>
   *
   * @return the number of members in this array.
   */
  override def arrayLength(): Int = vector.size

  /**
   * Ask whether the array is empty
   *
   * @return true if and only if the size of the array is zero
   */
  override def isEmpty: Boolean = vector.isEmpty

  /**
   * Get the list of all members of the array
   *
   * @return an iterator over the members of the array
   */
  override def members(): Iterable[GroundedValue] = vector

  /**
   * Get a subarray given a start and end position
   *
   * @param start the start position (zero based)
   * @param end   the end position (the position of the first item not to be returned)
   *              (zero based)
   * @throws IndexOutOfBoundsException if start, or start+end, is out of range
   */
  override def subArray(start: Int, end: Int): ArrayItem =
    new ImmutableArrayItem(vector.subList(start, end))

  /**
   * Concatenate this array with another
   *
   * @param other the second array
   * @return the concatenation of the two arrays; that is, an array
   *         containing first the members of this array, and then the members of the other array
   */
  override def concat(other: ArrayItem): ArrayItem = {
    if (other.arrayLength() == 0) {
      return this
    }
    var v1: ImmList[GroundedValue] = null
    v1 =
      if (other.isInstanceOf[ImmutableArrayItem])
        other.asInstanceOf[ImmutableArrayItem].vector
      else new ImmutableArrayItem(other.asInstanceOf[SimpleArrayItem]).vector
    val v2: ImmList[GroundedValue] = vector.appendList(v1)
    new ImmutableArrayItem(v2)
  }

  /**
   * Remove a member from the array
   *
   * @param index the position of the member to be removed (zero-based)
   * @return a new array in which the requested member has been removed.
   * @throws IndexOutOfBoundsException if index is out of range
   */
  override def remove(index: Int): ArrayItem = {
    //try {
    val v2: ImmList[GroundedValue] = vector.remove(index)
    if (v2 == vector) this else new ImmutableArrayItem(v2)
  }

  //        } catch (IndexOutOfBoundsException e) {
  //            throw new XPathException(e.getMessage(), "FOAR0001");
  //        }
  //        } catch (IndexOutOfBoundsException e) {
  //            throw new XPathException(e.getMessage(), "FOAR0001");
  //        }

  /**
   * Remove zero or more members from the array
   *
   * @param positions the positions of the members to be removed (zero-based).
   *                  A value that is out of range is ignored.
   * @return a new array in which the requested member has been removed
   */
  override def removeSeveral(positions: IntSet): ArrayItem = {
    val p: Array[Int] = Array.ofDim[Int](positions.size)
    var i: Int = 0
    val ii: IntIterator = positions.iterator()
    while (ii.hasNext) {
      i = i + 1
      p(i) = ii.next
    }
    Arrays.sort(p)
    var v2: ImmList[GroundedValue] = vector
    var j: Int = p.length - 1
    while (j >= 0) {
      v2 = v2.remove(p(j))
      j = j - 1
    }
    if (v2 == vector) this else new ImmutableArrayItem(v2)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Implementation of ArrayItem backed by a persistent immutable array, so that operations
 * that "update" the array do not have to copy the whole array
 */
