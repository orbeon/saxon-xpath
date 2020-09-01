////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012 Michael Froh.
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An immutable list implementation that only supports sequential traversal using an iterator,
 * prepending an item to the start, and extraction of the head()/tail() of the list. Unlike
 * {@link net.sf.saxon.ma.parray.ImmList}, it is optimized for sequential access rather than
 * direct access.
 */
package net.sf.saxon.ma.trie

import java.util.{Iterator, NoSuchElementException}

import net.sf.saxon.ma.trie.ImmutableList._

import scala.jdk.CollectionConverters._

object ImmutableList {

  /**
   * Return an empty list
   *
   * @return an empty list
   */
  def empty[T](): ImmutableList[T] = new EmptyList()

  private class EmptyList[T] extends ImmutableList[T] {

    override def head: T =
      throw new NoSuchElementException("head() called on empty list")

    override def tail(): ImmutableList[T] =
      throw new NoSuchElementException("head() called on empty list")

    override def isEmpty: Boolean = true
  }

  private class NonEmptyList[T](private val element: T, var tail: ImmutableList[T])
    extends ImmutableList[T] {
    override def head: T = element
    override def isEmpty: Boolean = false
  }

}

abstract class ImmutableList[T] extends java.lang.Iterable[T] {

  /**
   * Get the first item in the list
   *
   * @return the first item in the list
   */
  def head: T

  def tail(): ImmutableList[T]

  def isEmpty: Boolean

  def size(): Int = {
    var input: ImmutableList[T] = this
    var size: Int = 0
    while (!input.isEmpty) {
      size += 1
      input = input.tail()
    }
    size
  }

  def prepend(element: T): ImmutableList[T] = new NonEmptyList[T](element, this)

  override def equals(o: Any): Boolean = {
    if (!(o.isInstanceOf[ImmutableList[_]])) {
      return false
    }
    if (o == this) {
      return true
    }
    val thisIter: Iterator[T] = this.iterator()
    val otherIter: Iterator[_] = o.asInstanceOf[ImmutableList[_]].iterator()
    while (thisIter.hasNext && otherIter.hasNext) if (thisIter
      .next() != otherIter
      .next()) {
      false
    }
    thisIter.hasNext == otherIter.hasNext
  }

  def iterator(): Iterator[T] = new Iterator[T]() {
    private var list: ImmutableList[T] = ImmutableList.this

    def hasNext(): Boolean = !list.isEmpty

    def next(): T = {
      val element: T = list.head
      list = list.tail()
      element
    }
  }

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder("[")
    var first: Boolean = true
    for (elem <- this.asScala) {
      if (!first) {
        builder.append(", ")
      }
      builder.append(elem)
      first = false
    }
    builder.append(']')
    builder.toString
  }

  def reverse(): ImmutableList[T] = {
    var result: ImmutableList[T] = empty()
    for (element <- this.asScala) {
      result = result.prepend(element)
    }
    result
  }

}


