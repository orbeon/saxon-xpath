////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.{Outputter, ReceiverOption}
import org.orbeon.saxon.expr.{LastPositionFinder, RangeIterator, ReverseRangeIterator, StaticProperty}
import org.orbeon.saxon.functions.Count
import org.orbeon.saxon.model._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.EmptyIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.tree.wrapper.VirtualNode
import org.orbeon.saxon.value._

import scala.util.control.Breaks._


object SequenceTool {

  val INDETERMINATE_ORDERING: Int = java.lang.Integer.MIN_VALUE

  def toGroundedValue[T <: Item](iterator: SequenceIterator): GroundedValue =
    iterator.materialize

  def toMemoSequence(iterator: SequenceIterator): Sequence =
    if (iterator.isInstanceOf[EmptyIterator])
      EmptySequence.getInstance
    else if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED))
      iterator.materialize
    else
      new MemoSequence(iterator)

  def toLazySequence(iterator: SequenceIterator): Sequence =
    if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED) &&
      ! iterator.isInstanceOf[RangeIterator] && ! iterator.isInstanceOf[ReverseRangeIterator])
      iterator.materialize
    else
      new LazySequence(iterator)

  def toLazySequence2(iterator: SequenceIterator): Sequence =
    if (iterator.getProperties.contains(SequenceIterator.Property.GROUNDED) &&
      ! iterator.isInstanceOf[RangeIterator] && ! iterator.isInstanceOf[ReverseRangeIterator])
      iterator.materialize
    else
      new LazySequence(iterator)

  def isUnrepeatable(seq: Sequence): Boolean =
    seq.isInstanceOf[LazySequence] ||
      (seq.isInstanceOf[Closure] &&
        ! (seq.isInstanceOf[MemoClosure] || seq.isInstanceOf[SingletonClosure]))

  def getLength(sequence: Sequence): Int =
    sequence match {
      case value: GroundedValue => value.getLength
      case _                    => Count.count(sequence.iterate())
    }

  def hasLength(iter: SequenceIterator, length: Int): Boolean =
    if (iter.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER)) {
      iter.asInstanceOf[LastPositionFinder].getLength == length
    } else {
      var n = 0
      while (iter.next() != null) if ( {
        n += 1
        n - 1
      } == length) {
        iter.close()
        return false
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

  def itemAt(sequence: Sequence, index: Int): Item =
    sequence match {
      case item: Item if index == 0 => item
      case _                        => sequence.materialize.itemAt(index)
    }

  /*@Nullable*/
  def asItem(sequence: Sequence): Item = {
    sequence match {
      case item: Item => return item
      case _ =>
    }
    val iter = sequence.iterate()
    val first = iter.next()
    if (first == null)
      return null
    if (iter.next() != null)
      throw new XPathException("Sequence contains more than one item")
    first
  }

  def convertToJava(item: Item): Any =
    item match {
      case _: NodeInfo =>
        var node: Any = item
        while (node.isInstanceOf[VirtualNode]) // strip off any layers of wrapping
          node = node.asInstanceOf[VirtualNode].getRealNode
        node
      case _: Function =>
        item
      case externalObject: ExternalObject[_] =>
        externalObject.getObject
      case _ =>
        val value = item.asInstanceOf[AtomicValue]
        value.getItemType.getPrimitiveType match {
          case StandardNames.XS_STRING         |
               StandardNames.XS_UNTYPED_ATOMIC |
               StandardNames.XS_ANY_URI        |
               StandardNames.XS_DURATION =>
            value.getStringValue
          case StandardNames.XS_BOOLEAN =>
            value.asInstanceOf[BooleanValue].getBooleanValue
          case StandardNames.XS_DECIMAL =>
            value.asInstanceOf[BigDecimalValue].getDecimalValue
          case StandardNames.XS_INTEGER =>
            value.asInstanceOf[NumericValue].longValue
          case StandardNames.XS_DOUBLE =>
            value.asInstanceOf[DoubleValue].getDoubleValue
          case StandardNames.XS_FLOAT =>
            value.asInstanceOf[FloatValue].getFloatValue
          case StandardNames.XS_DATE_TIME =>
            // ORBEON: GregorianCalendar
            ???
//            value.asInstanceOf[DateTimeValue].getCalendar.getTime
          case StandardNames.XS_DATE =>
            // ORBEON: GregorianCalendar
            ???
//            value.asInstanceOf[DateValue].getCalendar.getTime
          case StandardNames.XS_TIME => value.getStringValue
          case StandardNames.XS_BASE64_BINARY =>
            value.asInstanceOf[Base64BinaryValue].getBinaryValue
          case StandardNames.XS_HEX_BINARY =>
            value.asInstanceOf[HexBinaryValue].getBinaryValue
          case _ => item
        }
    }

  def getStringValue(sequence: Sequence): String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    sequence
      .iterate()
      .forEachOrFail(item => {
        if (! fsb.isEmptySB)
          fsb.cat(' ')
        fsb.cat(item.getStringValueCS)
      })
    fsb.toString
  }

  def getItemType(sequence: Sequence, th: TypeHierarchy): ItemType =
    sequence match {
      case item: Item =>
        Type.getItemType(item, th)
      case _: GroundedValue =>
        try {
          var `type`: ItemType = null
          val iter = sequence.iterate()
          var item: Item = null
          breakable {
            while ({
              item = iter.next()
              item
            } != null) {
              `type` =
                if (`type` == null)
                  Type.getItemType(item, th)
                else
                  Type.getCommonSuperType(`type`, Type.getItemType(item, th), th)
              if (`type` == AnyItemType)
                break()
            }
          }
          if (`type` == null) ErrorType else `type`
        } catch {
          case _: XPathException => AnyItemType
        }
      case _ =>
        AnyItemType
    }

  def getUType(sequence: Sequence): UType =
    sequence match {
      case item: Item =>
        UType.getUType(item)
      case value: GroundedValue =>
        var `type` = UType.VOID
        val iter = value.iterate()
        var item: Item = null
        breakable {
          while ({
            item = iter.next()
            item
          } != null) {
            `type` = `type`.union(UType.getUType(item))
            if (`type` == UType.ANY)
              break()
          }
        }
        `type`
      case _ =>
        UType.ANY
    }

  def getCardinality(sequence: Sequence): Int = {
    sequence match {
      case _: Item => return StaticProperty.EXACTLY_ONE
      case _ =>
    }
    sequence match {
      case value: GroundedValue =>
        val len = value.getLength
        return len match {
          case 0 => return StaticProperty.ALLOWS_ZERO
          case 1 => return StaticProperty.EXACTLY_ONE
          case _ => return StaticProperty.ALLOWS_ONE_OR_MORE
        }
      case _ =>
    }
    try {
      val iter = sequence.iterate()
      var item = iter.next()
      if (item == null)
        return StaticProperty.ALLOWS_ZERO
      item = iter.next()
      if (item == null)
        StaticProperty.EXACTLY_ONE
      else
        StaticProperty.ALLOWS_ONE_OR_MORE
    } catch {
      case _: XPathException =>
        StaticProperty.ALLOWS_ONE_OR_MORE
    }
  }

  def process(value: Sequence, output: Outputter, locationId: Location): Unit =
    value
      .iterate()
      .forEachOrFail(it =>
        output.append(it, locationId, ReceiverOption.ALL_NAMESPACES))

  def makeSequenceArray(length: Int): Array[Sequence] =
    Array.ofDim[Sequence](length)

  def fromItems(items: Item*): Array[Sequence] = {
    val seq = Array.ofDim[Sequence](items.length)
    System.arraycopy(items, 0, seq, 0, items.length)
    seq
  }
}
