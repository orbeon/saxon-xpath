package org.orbeon.saxon.ma.map

import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{GroundedValue, SequenceTool}
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.{AtomicValue, Cardinality, SequenceType, StringValue}

import java.util._

import scala.jdk.CollectionConverters._


class DictionaryMap extends MapItem {

  private val hashMap: HashMap[String, GroundedValue] = new HashMap

  def initialPut(key: String, value: GroundedValue): Unit =
    hashMap.put(key, value)

  def initialAppend(key: String, value: GroundedValue): Unit = {
    val existingValue = hashMap.get(key)
    if (existingValue == null)
      initialPut(key, value)
    else
      hashMap.put(key, existingValue.concatenate(value))
  }

  override def get(key: AtomicValue): GroundedValue =
    if (key.isInstanceOf[StringValue])
      hashMap.get(key.getStringValue)
    else
      null

  override def size: Int = hashMap.size
  override def isEmpty: Boolean = hashMap.isEmpty

  override def keys: AtomicIterator[StringValue] = {
    val base = hashMap.keySet.iterator
    if (base.hasNext)
      new StringValue(base.next()).asInstanceOf[AtomicIterator[StringValue]] else null
  }

  override def keyValuePairs: java.lang.Iterable[KeyValuePair] = {
    val pairs = new ArrayList[KeyValuePair]
    hashMap.asScala.foreach { case (k, v) =>
      pairs.add(KeyValuePair(new StringValue(k), v))
    }
    pairs
  }

  override def addEntry(key: AtomicValue, value: GroundedValue): MapItem =
    toHashTrieMap.addEntry(key, value)

  override def remove(key: AtomicValue): MapItem = toHashTrieMap.remove(key)

  override def conforms(
    keyType   : AtomicType,
    valueType : SequenceType,
    th        : TypeHierarchy
  ): Boolean = {
    if (isEmpty)
      return true

    if (!(keyType == BuiltInAtomicType.STRING || keyType == BuiltInAtomicType.ANY_ATOMIC))
      return false

    if (valueType == SequenceType.ANY_SEQUENCE)
      return true

    for (mapVal <- hashMap.values.asScala if !valueType.matches(mapVal, th))
      return false

    true
  }

  override def getItemType(th: TypeHierarchy): ItemType = {
    var valueType: ItemType = null
    var valueCard           = 0
    for ((_, value) <- hashMap.asScala) {
      val `val` = value
      if (valueType == null) {
        valueType = SequenceTool.getItemType(`val`, th)
        valueCard = SequenceTool.getCardinality(`val`)
      } else {
        valueType = Type.getCommonSuperType(
          valueType,
          SequenceTool.getItemType(`val`, th),
          th)
        valueCard = Cardinality.union(valueCard, SequenceTool.getCardinality(`val`))
      }
    }
    if (valueType == null)
      MapType.EMPTY_MAP_TYPE
    else
      new MapType(BuiltInAtomicType.STRING, SequenceType.makeSequenceType(valueType, valueCard))
  }

  override def getKeyUType: UType =
    if (hashMap.isEmpty) UType.VOID else UType.STRING

  private def toHashTrieMap: HashTrieMap = {
    val target = new HashTrieMap()
    hashMap.asScala.foreach { case (k, v) => target.initialPut(new StringValue(k), v) }
    target
  }
}
