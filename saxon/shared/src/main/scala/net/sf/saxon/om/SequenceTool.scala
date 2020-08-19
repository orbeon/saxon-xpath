////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.RangeIterator

import net.sf.saxon.expr.ReverseRangeIterator

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.functions.Count

import net.sf.saxon.model._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.UnfailingIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.tree.wrapper.VirtualNode

import net.sf.saxon.value._
import scala.util.control.Breaks._

object SequenceTool {

  val INDETERMINATE_ORDERING: Int = java.lang.Integer.MIN_VALUE

  def toGroundedValue[T <: Item](iterator: SequenceIterator): GroundedValue =
    iterator.materialize()

  def toMemoSequence(iterator: SequenceIterator): Sequence =
    if (iterator.isInstanceOf[EmptyIterator]) {
      EmptySequence.getInstance
    } else if (iterator.getProperties.contains(
      SequenceIterator.Property.GROUNDED)) {
      iterator.materialize()
    } else {
      new MemoSequence(iterator)
    }

  def toLazySequence(iterator: SequenceIterator): Sequence =
    if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED) &&
      !(iterator.isInstanceOf[RangeIterator]) &&
      !(iterator.isInstanceOf[ReverseRangeIterator])) {
      iterator.materialize()
    } else {
      new LazySequence(iterator)
    }

  def toLazySequence2(iterator: SequenceIterator): Sequence =
    if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED) &&
      !(iterator.isInstanceOf[RangeIterator]) &&
      !(iterator.isInstanceOf[ReverseRangeIterator])) {
      iterator.materialize()
    } else {
      new LazySequence(iterator)
    }

  def isUnrepeatable(seq: Sequence): Boolean =
    seq.isInstanceOf[LazySequence] ||
      (seq.isInstanceOf[Closure] &&
        !(seq.isInstanceOf[MemoClosure] || seq.isInstanceOf[SingletonClosure]))

  def getLength(sequence: Sequence): Int = {
    if (sequence.isInstanceOf[GroundedValue]) {
      sequence.asInstanceOf[GroundedValue].getLength
    }
    Count.count(sequence.iterate())
  }

  def hasLength(iter: SequenceIterator, length: Int): Boolean =
    if (iter.getProperties.contains(
      SequenceIterator.Property.LAST_POSITION_FINDER)) {
      iter.asInstanceOf[LastPositionFinder].getLength == length
    } else {
      var n: Int = 0
      while (iter.next() != null) if ( {
        n += 1;
        n - 1
      } == length) {
        iter.close()
        false
      }
      length == 0
    }

  def sameLength(a: SequenceIterator, b: SequenceIterator): Boolean =
    if (a.getProperties.contains(
      SequenceIterator.Property.LAST_POSITION_FINDER)) {
      hasLength(b, a.asInstanceOf[LastPositionFinder].getLength)
    } else if (b.getProperties.contains(
      SequenceIterator.Property.LAST_POSITION_FINDER)) {
      hasLength(a, b.asInstanceOf[LastPositionFinder].getLength)
    } else {
      while (true) {
        val itA: Item = a.next()
        val itB: Item = b.next()
        if (itA == null || itB == null) {
          if (itA != null) {
            a.close()
          }
          if (itB != null) {
            b.close()
          }
          return itA == null && itB == null
        }
      }
      false
    }

  def itemAt(sequence: Sequence, index: Int): Item = {
    if (sequence.isInstanceOf[Item] && index == 0) {
      sequence.asInstanceOf[Item]
    }
    sequence.materialize().itemAt(index)
  }

  /*@Nullable*/

  def asItem(sequence: Sequence): Item = {
    if (sequence.isInstanceOf[Item]) {
      sequence.asInstanceOf[Item]
    }
    val iter: SequenceIterator = sequence.iterate()
    val first: Item = iter.next()
    if (first == null) {
      null
    }
    if (iter.next() != null) {
      throw new XPathException("Sequence contains more than one item")
    }
    first
  }

  def convertToJava(item: Item): Any =
    if (item.isInstanceOf[NodeInfo]) {
      var node: Any = item
      while (node
        .isInstanceOf[VirtualNode]) // strip off any layers of wrapping
        node = node.asInstanceOf[VirtualNode].getRealNode
      return node
    } else if (item.isInstanceOf[Function]) {
      return item
    } else if (item.isInstanceOf[ExternalObject[AnyRef]]) {
      item.asInstanceOf[ExternalObject[Any]].getObject()
    } else {
      val value: AtomicValue = item.asInstanceOf[AtomicValue]
      value.getItemType.getPrimitiveType match {
        case StandardNames.XS_STRING | StandardNames.XS_UNTYPED_ATOMIC |
             StandardNames.XS_ANY_URI | StandardNames.XS_DURATION =>
          return value.getStringValue
        case StandardNames.XS_BOOLEAN =>
          if (value.asInstanceOf[BooleanValue].getBooleanValue) true else false
        case StandardNames.XS_DECIMAL =>
          return value.asInstanceOf[BigDecimalValue].getDecimalValue
        case StandardNames.XS_INTEGER =>
          return value.asInstanceOf[NumericValue].longValue()
        case StandardNames.XS_DOUBLE =>
          return value.asInstanceOf[DoubleValue].getDoubleValue
        case StandardNames.XS_FLOAT =>
          return value.asInstanceOf[FloatValue].getFloatValue
        case StandardNames.XS_DATE_TIME =>
          return value.asInstanceOf[DateTimeValue].getCalendar.getTime
        case StandardNames.XS_DATE =>
          return value.asInstanceOf[DateValue].getCalendar.getTime
        case StandardNames.XS_TIME => value.getStringValue
        case StandardNames.XS_BASE64_BINARY =>
          return value.asInstanceOf[Base64BinaryValue].getBinaryValue
        case StandardNames.XS_HEX_BINARY =>
          return value.asInstanceOf[HexBinaryValue].getBinaryValue
        case _ => item

      }
    }

  def getStringValue(sequence: Sequence): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    sequence
      .iterate()
      .forEachOrFail((item) => {
        if (!fsb.isEmpty) {
          fsb.cat(' ')
        }
        fsb.cat(item.getStringValueCS)
      })
    fsb.toString
  }

  def getItemType(sequence: Sequence, th: TypeHierarchy): ItemType =
    if (sequence.isInstanceOf[Item]) {
      Type.getItemType(sequence.asInstanceOf[Item], th)
    } else if (sequence.isInstanceOf[GroundedValue]) {
      try {
        var `type`: ItemType = null
        val iter: SequenceIterator = sequence.iterate()
        var item: Item = null
        breakable {
          while ((item = iter.next()) != null) {
            `type` =
              if (`type` == null) Type.getItemType(item, th)
              else
                Type.getCommonSuperType(`type`, Type.getItemType(item, th), th)
            if (`type` == AnyItemType.getInstance) {
              break
            }
          }
        }
        if (`type` == null) ErrorType.getInstance else `type`
      } catch {
        case err: XPathException => AnyItemType.getInstance

      }
    } else {
      AnyItemType.getInstance
    }

  def getUType(sequence: Sequence): UType =
    if (sequence.isInstanceOf[Item]) {
      UType.getUType(sequence.asInstanceOf[Item])
    } else if (sequence.isInstanceOf[GroundedValue]) {
      var `type`: UType = UType.VOID
      val iter: UnfailingIterator =
        sequence.asInstanceOf[GroundedValue].iterate()
      var item: Item = null
      breakable { while ((item = iter.next()) != null) {
        `type` = `type`.union(UType.getUType(item))
        if (`type` == UType.ANY) {
          break
        }
      }
    }
      `type`
    } else {
      UType.ANY
    }

  def getCardinality(sequence: Sequence): Int = {
    if (sequence.isInstanceOf[Item]) {
      StaticProperty.EXACTLY_ONE
    }
    if (sequence.isInstanceOf[GroundedValue]) {
      val len: Int = sequence.asInstanceOf[GroundedValue].getLength
      len match {
        case 0 => StaticProperty.ALLOWS_ZERO
        case 1 => StaticProperty.EXACTLY_ONE
        case _ => StaticProperty.ALLOWS_ONE_OR_MORE

      }
    }
    try {
      val iter: SequenceIterator = sequence.iterate()
      var item: Item = iter.next()
      if (item == null) {
        StaticProperty.ALLOWS_ZERO
      }
      item = iter.next()
      if (item == null) StaticProperty.EXACTLY_ONE
      else StaticProperty.ALLOWS_ONE_OR_MORE
    } catch {
      case err: XPathException => StaticProperty.ALLOWS_ONE_OR_MORE

    }
  }

  def process(value: Sequence, output: Outputter, locationId: Location): Unit = {
    value
      .iterate()
      .forEachOrFail((it) =>
        output.append(it, locationId, ReceiverOption.ALL_NAMESPACES))
  }

  def makeSequenceArray(length: Int): Array[Sequence] =
    Array.ofDim[Sequence](length).asInstanceOf[Array[Sequence]]

  def fromItems(items: Item*): Array[Sequence] = {
    val seq: Array[Sequence] =
      Array.ofDim[Sequence](items.length).asInstanceOf[Array[Sequence]]
    System.arraycopy(items, 0, seq, 0, items.length)
    seq
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
