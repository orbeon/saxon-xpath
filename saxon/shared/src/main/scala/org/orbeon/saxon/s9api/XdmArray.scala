package org.orbeon.saxon.s9api

import org.orbeon.saxon.ma.arrays.ArrayFunctionSet
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.arrays.SimpleArrayItem
import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.om.Sequence
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import org.orbeon.saxon.s9apir.XdmFunctionItem

object XdmArray {

  def makeArray(input: Array[Any]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(XdmValue.makeValue(o))
    }
    new XdmArray(result)
  }

  def makeArray(input: Array[Boolean]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(new XdmAtomicValue(o))
    }
    new XdmArray(result)
  }

  def makeArray(input: Array[Long]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(new XdmAtomicValue(o))
    }
    new XdmArray(result)
  }

  def makeArray(input: Array[Int]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(new XdmAtomicValue(o))
    }
    new XdmArray(result)
  }

  def makeArray(input: Array[Short]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(new XdmAtomicValue(o))
    }
    new XdmArray(result)
  }

  def makeArray(input: Array[Byte]): XdmArray = {
    val result: List[XdmValue] = new ArrayList[XdmValue](input.length)
    for (o <- input) {
      result.add(new XdmAtomicValue(o))
    }
    new XdmArray(result)
  }

}

class XdmArray extends XdmFunctionItem {

  this.setValue(SimpleArrayItem.EMPTY_ARRAY)

  def this(array: ArrayItem) = {
    this()
    this.setValue(array)
  }

  def this(members: Array[XdmValue]) = {
    this()
    val values: List[GroundedValue] = new ArrayList[GroundedValue]()
    for (member <- members) {
      values.add(member.getUnderlyingValue)
    }
    this.setValue(new SimpleArrayItem(values))
  }

  def this(members: java.lang.Iterable[_ <: XdmValue]) = {
    this()
    val values: List[GroundedValue] = new ArrayList[GroundedValue]()
    for (member <- members.asScala) {
      values.add(member.getUnderlyingValue)
    }
    this.setValue( new SimpleArrayItem(values))
  }

  def arrayLength(): Int = getUnderlyingValue.arrayLength()

  def get(n: Int): XdmValue = {
    val member: Sequence = getUnderlyingValue.get(n)
    XdmValue.wrap(member)
  }

  def put(n: Int, value: XdmValue): XdmArray = {
    val member: GroundedValue = value.getUnderlyingValue
    XdmValue.wrap(getUnderlyingValue.put(n, member)).asInstanceOf[XdmArray]
  }

  def addMember(value: XdmValue): XdmArray = {
    val member: GroundedValue = value.getUnderlyingValue
    val newArray: ArrayItem =
      ArrayFunctionSet.ArrayAppend.append(getUnderlyingValue, member)
    XdmValue.wrap(newArray).asInstanceOf[XdmArray]
  }

  def concat(value: XdmArray): XdmArray = {
    val other: ArrayItem = value.getUnderlyingValue
    val newArray: ArrayItem = getUnderlyingValue.concat(other)
    XdmValue.wrap(newArray).asInstanceOf[XdmArray]
  }

  def asList(): List[XdmValue] = {
    val members: Iterator[GroundedValue] =
      getUnderlyingValue.members().iterator.asJava
    val result: List[XdmValue] =
      new ArrayList[XdmValue](getUnderlyingValue.getLength)
    while (members.hasNext) result.add(XdmValue.wrap(members.next()))
    result
  }

  override def getUnderlyingValue: ArrayItem =
    super.getUnderlyingValue.asInstanceOf[ArrayItem]

}
