package org.orbeon.saxon.s9api

import java.util

import org.orbeon.saxon.ma.map.HashTrieMap
import org.orbeon.saxon.ma.map.KeyValuePair
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.om.{AtomicSequence, GroundedValue}
import org.orbeon.saxon.tree.jiter.MappingJavaIterator
import org.orbeon.saxon.value.DoubleValue
import org.orbeon.saxon.value.Int64Value
import org.orbeon.saxon.value.StringValue
import java.util._

import XdmMap._
import org.orbeon.saxon.s9apir.XdmFunctionItem

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object XdmMap {

  def makeMap(input: Map[_, _]): XdmMap = {
    val result: HashTrieMap = new HashTrieMap()
    for ((k, v) <- input.asScala) {
      val key: Any = k
      val value: Any = v
      val xKey: XdmAtomicValue = XdmAtomicValue.makeAtomicValue(key.asInstanceOf[AnyRef])
      val xValue: XdmValue = XdmValue.makeValue(value)
      result.initialPut(xKey.getUnderlyingValue, xValue.getUnderlyingValue)
    }
    new XdmMap(result)
  }

  private class XdmMapEntry(var pair: KeyValuePair)
    extends Map.Entry[XdmAtomicValue, XdmValue] {

    override def getKey(): XdmAtomicValue =
      XdmValue.wrap(pair.key).asInstanceOf[XdmAtomicValue]

    override def getValue(): XdmValue = XdmValue.wrap(pair.value)

    override def setValue(value: XdmValue): XdmValue =
      throw new UnsupportedOperationException()

  }

}

class XdmMap extends XdmFunctionItem {

  this.setValue(new HashTrieMap())

  def this(map: MapItem) = {
    this()
    this.setValue(map)
  }

  def this(map: Map[_ <: XdmAtomicValue, _ <: XdmValue]) = {
    this()
    val `val`: HashTrieMap = new HashTrieMap()
    for ((key, value) <- map.asScala) {
      `val`.initialPut(key.getUnderlyingValue, value.getUnderlyingValue)
    }
    this.setValue(`val`)
  }

  override def getUnderlyingValue: MapItem =
    super.getUnderlyingValue.asInstanceOf[MapItem]

  def mapSize(): Int = getUnderlyingValue.size

  def put(key: XdmAtomicValue, value: XdmValue): XdmMap = {
    val map2: XdmMap = new XdmMap()
    map2.setValue(
      getUnderlyingValue.addEntry(key.getUnderlyingValue,
        value.getUnderlyingValue))
    map2
  }

  def remove(key: XdmAtomicValue): XdmMap = {
    val map2: XdmMap = new XdmMap()
    map2.setValue(getUnderlyingValue.remove(key.getUnderlyingValue))
    map2
  }

  def keySet(): Set[XdmAtomicValue] = new AbstractSet[XdmAtomicValue]() {
    override def iterator: Iterator[XdmAtomicValue] =
      new MappingJavaIterator(
        getUnderlyingValue.keyValuePairs().iterator,
        (kvp: KeyValuePair) => XdmValue.wrap(kvp.key).asInstanceOf[XdmAtomicValue])

    override def size(): Int = getUnderlyingValue.size

    override def contains(o: AnyRef): Boolean =
      getUnderlyingValue
        .get(o.asInstanceOf[XdmAtomicValue].getUnderlyingValue) !=
        null
  }

  def asImmutableMap(): Map[XdmAtomicValue, XdmValue] = {
    val base: XdmMap = this
    new AbstractMap[XdmAtomicValue, XdmValue]() {
      override def entrySet(): Set[Map.Entry[XdmAtomicValue, XdmValue]] =
        base.entrySet()

      override def size(): Int = base.mapSize()

      override def isEmpty: Boolean = base.isEmpty

      override def containsKey(key: AnyRef): Boolean =
        key.isInstanceOf[XdmAtomicValue] && base.containsKey(
          key.asInstanceOf[XdmAtomicValue])

      override def get(key: AnyRef): XdmValue =
        if (key.isInstanceOf[XdmAtomicValue])
          base.get(key.asInstanceOf[XdmAtomicValue])
        else null

      override def put(key: XdmAtomicValue, value: XdmValue): XdmValue =
        throw new UnsupportedOperationException()

      override def remove(key: AnyRef): XdmValue =
        throw new UnsupportedOperationException()

      override def putAll(m: Map[_ <: XdmAtomicValue, _ <: XdmValue]): Unit = {
        throw new UnsupportedOperationException()
      }

      override def clear(): Unit = {
        throw new UnsupportedOperationException()
      }

      override def keySet(): Set[XdmAtomicValue] = base.keySet

      override def values(): Collection[XdmValue] = base.values
    }
  }

  override def asMap(): Map[XdmAtomicValue, XdmValue] = new HashMap(asImmutableMap())

  def clear(): Unit = {
    throw new UnsupportedOperationException("XdmMap is immutable")
  }

  override def isEmpty: Boolean = getUnderlyingValue.isEmpty

  def containsKey(key: XdmAtomicValue): Boolean =
    getUnderlyingValue.get(key.getUnderlyingValue) != null

  def get(key: XdmAtomicValue): XdmValue = {
    if (key == null) {
      throw new NullPointerException()
    }
    val v: GroundedValue = getUnderlyingValue.get(key.getUnderlyingValue)
    if (v == null) null else XdmValue.wrap(v)
  }

  def get(key: String): XdmValue = {
    if (key == null) {
      throw new NullPointerException()
    }
    val v: GroundedValue = getUnderlyingValue.get(new StringValue(key))
    if (v == null) null else XdmValue.wrap(v)
  }

  def get(key: Long): XdmValue = {
    val v: GroundedValue = getUnderlyingValue.get(new Int64Value(key))
    if (v == null) null else XdmValue.wrap(v)
  }

  def get(key: Double): XdmValue = {
    val v: GroundedValue = getUnderlyingValue.get(new DoubleValue(key))
    if (v == null) null else XdmValue.wrap(v)
  }

  def values(): Collection[XdmValue] = {
    val result: List[XdmValue] = new ArrayList[XdmValue]()
    for (keyValuePair <- getUnderlyingValue.keyValuePairs().asScala) {
      result.add(XdmValue.wrap(keyValuePair.value))
    }
    result
  }

  def entrySet(): Set[Map.Entry[XdmAtomicValue, XdmValue]] = {
    val result: Set[Map.Entry[XdmAtomicValue, XdmValue]] =
      new HashSet[Map.Entry[XdmAtomicValue, XdmValue]]()
    for (keyValuePair <- getUnderlyingValue.keyValuePairs().asScala) {
      result.add(new XdmMapEntry(keyValuePair))
    }
    result
  }

}
