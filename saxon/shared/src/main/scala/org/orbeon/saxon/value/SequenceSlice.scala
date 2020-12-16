////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ListIterator

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.util.Iterator

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

class SequenceSlice(private var value: List[_ <: Item],
                    private var offset: Int,
                    lengthParam: Int)
  extends GroundedValue {

  @BeanProperty
  var length: Int =
    if (lengthParam > value.size || offset + lengthParam > value.size)
      value.size - offset
    else lengthParam

  if (offset < 0 || length < 0) {
    throw new IndexOutOfBoundsException()
  }

  def getStringValue: String = SequenceTool.getStringValue(this)

  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  /**
   * Get the first item in the sequence.
   *
   * @return the first item in the sequence if there is one, or null if the sequence
   *         is empty
   */
  def head: Item = itemAt(0)

  def getCardinality: Int = getLength match {
    case 0 => StaticProperty.EMPTY
    case 1 => StaticProperty.EXACTLY_ONE
    case _ => StaticProperty.ALLOWS_ONE_OR_MORE

  }

  /*@Nullable*/

  def itemAt(n: Int): Item =
    if (n < 0 || n >= getLength) {
      null
    } else {
      value.get(n + offset)
    }

  /*@NotNull*/

  def iterate(): ListIterator[_ <: Item] =
    new ListIterator(value.subList(offset, offset + length))

  /*@NotNull*/

  def subsequence(start: Int, length: Int): GroundedValue = {
    var start1: Int = start
    if (start1 < 0) {
     start1 = 0
    }
    val newStart: Int = start1 + offset
    if (newStart > value.size) {
     return EmptySequence.getInstance
    }
    if (length < 0) {
     return EmptySequence.getInstance
    }
    var newLength: Int = java.lang.Integer.min(length, this.length)
    if (newStart + newLength > value.size) {
      newLength = value.size - newStart
    }
    newLength match {
      case 0 => EmptySequence.getInstance
      case 1 => value.get(newStart)
      case _ => new SequenceSlice(value, newStart, newLength)

    }
  }

  /*@NotNull*/

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    for (i <- 0 until getLength) {
      fsb.append(if (i == 0) "(" else ", ")
      fsb.append(itemAt(i).toString)
    }
    fsb.cat(')')
    fsb.toString
  }

  /**
   * Reduce the sequence to its simplest form. If the value is an empty sequence, the result will be
   * EmptySequence.getInstance(). If the value is a single atomic value, the result will be an instance
   * of AtomicValue. If the value is a single item of any other kind, the result will be an instance
   * of One. Otherwise, the result will typically be unchanged.
   *
   * @return the simplified sequence
   */
  override def reduce(): GroundedValue = {
    val len = getLength
    if (len == 0) {
      EmptySequence.getInstance
    } else if (len == 1) {
      itemAt(0)
    } else {
      this
    }
  }

  override def asIterable(): java.lang.Iterable[_ <: Item] =
    value.subList(offset, offset + length)

  def iterator: Iterator[_ <: Item] =
    value.subList(offset, offset + length).iterator

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A sequence value implemented extensionally. That is, this class represents a sequence
 * by allocating memory to each item in the sequence.
 */
