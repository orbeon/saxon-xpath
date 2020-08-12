package net.sf.saxon.ma.map

import java.lang.Iterable

import net.sf.saxon.model._
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.SequenceTool
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.SequenceType
import net.sf.saxon.value.StringValue
import java.util._

import scala.jdk.CollectionConverters._

class DictionaryMap extends MapItem {

  private var hashMap: HashMap[String, GroundedValue] = new HashMap()

  def initialPut(key: String, value: GroundedValue): Unit = {
    hashMap.put(key, value)
  }

  def initialAppend(key: String, value: GroundedValue): Unit = {
    val existingValue: GroundedValue = hashMap.get(key)
    if (existingValue == null) {
      initialPut(key, value)
    } else {
      hashMap.put(key, existingValue.concatenate(value))
    }
  }

  override def get(key: AtomicValue): GroundedValue =
    if (key.isInstanceOf[StringValue]) {
      hashMap.get(key.getStringValue)
    } else {
      null
    }

  override def size(): Int = hashMap.size

  override def isEmpty(): Boolean = hashMap.isEmpty

  override def keys(): AtomicIterator[StringValue] = {
    val base: Iterator[String] = hashMap.keySet.iterator()

    if (base.hasNext) new StringValue(base.next()).asInstanceOf[AtomicIterator[StringValue]] else null
  }

  override def keyValuePairs(): java.lang.Iterable[KeyValuePair] = {
    val pairs: List[KeyValuePair] = new ArrayList[KeyValuePair]()
    hashMap.forEach((k, v) =>
      pairs.add(new KeyValuePair(new StringValue(k), v)))
    pairs
  }

  override def addEntry(key: AtomicValue, value: GroundedValue): MapItem =
    toHashTrieMap().addEntry(key, value)

  override def remove(key: AtomicValue): MapItem = toHashTrieMap().remove(key)

  override def conforms(keyType: AtomicType,
                        valueType: SequenceType,
                        th: TypeHierarchy): Boolean = {
    if (isEmpty) {
      return true
    }
    if (!(keyType == BuiltInAtomicType.STRING || keyType == BuiltInAtomicType.ANY_ATOMIC)) {
      return false
    }
    if (valueType == SequenceType.ANY_SEQUENCE) {
      return true
    }
    for (mapVal <- hashMap.values.asScala if !valueType.matches(mapVal, th)) {
      false
    }
    true
  }

  override def getItemType(th: TypeHierarchy): ItemType = {
    var valueType: ItemType = null
    var valueCard: Int = 0
    val keyIter: AtomicIterator[_ <: AtomicValue] = keys
    val key: AtomicValue = null
    for ((key, value) <- hashMap.asScala) {
      val `val`: GroundedValue = value
      if (valueType == null) {
        valueType = SequenceTool.getItemType(`val`, th)
        valueCard = SequenceTool.getCardinality(`val`)
      } else {
        valueType = Type.getCommonSuperType(
          valueType,
          SequenceTool.getItemType(`val`, th),
          th)
        valueCard =
          Cardinality.union(valueCard, SequenceTool.getCardinality(`val`))
      }
    }
    if (valueType == null) {
      MapType.EMPTY_MAP_TYPE
    } else {
      new MapType(BuiltInAtomicType.STRING,
        SequenceType.makeSequenceType(valueType, valueCard))
    }
  }

  override def getKeyUType(): UType =
    if (hashMap.isEmpty) UType.VOID else UType.STRING

  private def toHashTrieMap(): HashTrieMap = {
    val target: HashTrieMap = new HashTrieMap()
    hashMap.forEach((k, v) => target.initialPut(new StringValue(k), v))
    target
  }

}
