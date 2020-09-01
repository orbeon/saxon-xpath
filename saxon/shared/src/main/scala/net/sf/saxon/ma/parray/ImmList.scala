////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An immutable list of elements
 */
package net.sf.saxon.ma.parray

import java.util.List




object ImmList {

  def empty[E](): ImmList[E] = ImmList0.INSTANCE.asInstanceOf[ImmList[E]]

  def singleton[E](member: E): ImmList[E] = new ImmList1(member)

  def pair[E](first: E, second: E): ImmList[E] =
    new ImmList2[E](new ImmList1[E](first), new ImmList1[E](second))

  /**
    * Construct an immutable list from a Java list of members
    * @param members the members to be added to the list
    * @return the immutable list
    */
  def fromList[E](members: List[E]): ImmList[E] = {
    val size: Int = members.size
    if (size == 0) {
      empty()
    } else if (size == 1) {
      singleton(members.get(0))
    } else {
      val split: Int = size / 2
      val left: List[E] = members.subList(0, split)
      val right: List[E] = members.subList(split, size)
      new ImmList2(fromList(left), fromList(right))
    }
  }

}

abstract class ImmList[E] extends Iterable[E] {

  def get(index: Int): E

  override def head: E = get(0)

  def size(): Int

  def isEmpty: Boolean

  def replace(index: Int, member: E): ImmList[E]

  def insert(index: Int, member: E): ImmList[E]

  def append(member: E): ImmList[E]

  def appendList(members: ImmList[E]): ImmList[E]

  def remove(index: Int): ImmList[E]

  def subList(start: Int, end: Int): ImmList[E]

  override def tail(): ImmList[E] = remove(0)

   def rebalance(): ImmList[E] = this

   def outOfBounds(requested: Int,
                            actual: Int): IndexOutOfBoundsException =
    new IndexOutOfBoundsException(
      "Requested " + requested + ", actual size " + actual)

}


