package org.orbeon.saxon.ma.map

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.ma.trie.{ImmutableHashTrieMap, ImmutableMap, Tuple2}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{GroundedValue, Sequence, SequenceTool}

//import scala.collection.compat._
import java.util.Iterator

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.{AtomicValue, Cardinality, SequenceType}

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object HashTrieMap {

  def singleton(key: AtomicValue, value: GroundedValue): HashTrieMap =
    new HashTrieMap().addEntry(key, value)

  def copy(map: MapItem): HashTrieMap = {
    if (map.isInstanceOf[HashTrieMap]) {
      return map.asInstanceOf[HashTrieMap]
    }
    var m2: HashTrieMap = new HashTrieMap()
    for (pair <- map.keyValuePairs.asScala) {
      m2 = m2.addEntry(pair.key, pair.value)
    }
    m2
  }

}

class HashTrieMap extends MapItem {

  private var imap: ImmutableMap[AtomicMatchKey, KeyValuePair] =
    ImmutableHashTrieMap.empty()

  @BeanProperty
  var keyUType: UType = UType.VOID

  private var valueUType: UType = UType.VOID

  private var keyAtomicType: AtomicType = ErrorType

  private var valueItemType: ItemType = ErrorType

  private var valueCardinality: Int = 0

  private var entries: Int = 0

  def this(imap: ImmutableMap[AtomicMatchKey, KeyValuePair]) = {
    this()
    this.imap = imap
    entries = -1
  }

  private def updateTypeInformation(key: AtomicValue,
                                    `val`: Sequence,
                                    wasEmpty: Boolean): Unit = {
    if (wasEmpty) {
      keyUType = key.getUType
      valueUType = SequenceTool.getUType(`val`)
      keyAtomicType = key.getItemType
      valueItemType = MapItem.getItemTypeOfSequence(`val`)
      valueCardinality = SequenceTool.getCardinality(`val`)
    } else {
      keyUType = keyUType.union(key.getUType)
      valueUType = valueUType.union(SequenceTool.getUType(`val`))
      valueCardinality =
        Cardinality.union(valueCardinality, SequenceTool.getCardinality(`val`))
      if (key.getItemType != keyAtomicType) {
        keyAtomicType = null
      }
      if (!MapItem.isKnownToConform(`val`, valueItemType)) {
        valueItemType = null
      }
    }
  }

  def size(): Int = {
    if (entries >= 0) {
      return entries
    }
    var count: Int = 0
    for (entry <- keyValuePairs().asScala) {
      count += 1
    }
    entries = count
    entries
  }

  def isEmpty: Boolean = entries == 0 || !imap.iterator.hasNext

  override def conforms(requiredKeyType: AtomicType,
                        requiredValueType: SequenceType,
                        th: TypeHierarchy): Boolean = {
    if (isEmpty) {
      return true
    }
    if (keyAtomicType == requiredKeyType && valueItemType == requiredValueType.getPrimaryType &&
      Cardinality.subsumes(requiredValueType.getCardinality,
        valueCardinality)) {
      return true
    }
    var needFullCheck: Boolean = false
    if (requiredKeyType != BuiltInAtomicType.ANY_ATOMIC) {
      val upperBoundKeyType: ItemType = keyUType.toItemType
      val rel: Affinity.Affinity = th.relationship(requiredKeyType, upperBoundKeyType)
      if (rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMES) {} else if (rel == Affinity.DISJOINT) {
        return false
      } else {
        needFullCheck = true
      }
    }
    val requiredValueItemType: ItemType = requiredValueType.getPrimaryType
    if (requiredValueItemType != BuiltInAtomicType.ANY_ATOMIC) {
      val upperBoundValueType: ItemType = valueUType.toItemType
      val rel: Affinity.Affinity =
        th.relationship(requiredValueItemType, upperBoundValueType)
      if (rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMES) {} else if (rel == Affinity.DISJOINT) {
        return false
      } else {
        needFullCheck = true
      }
    }
    val requiredValueCard: Int = requiredValueType.getCardinality
    if (!Cardinality.subsumes(requiredValueCard, valueCardinality)) {
      needFullCheck = true
    }
    if (needFullCheck) {
      val keyIter: AtomicIterator[_ <: AtomicValue] = keys
      var key: AtomicValue = null
      while (({
        key = keyIter.next()
        key
      }) != null) {
        if (!requiredKeyType.matches(key, th)) {
          return false
        }
        val `val`: Sequence = get(key)
        if (!requiredValueType.matches(`val`, th)) {
          false
        }
      }
    }
    true
  }

  override def getItemType(th: TypeHierarchy): MapType = {
    var keyType: AtomicType = null
    var valueType: ItemType = null
    var valueCard: Int = 0
    val keyIter: AtomicIterator[_ <: AtomicValue] = keys
    var key: AtomicValue = null
    while (({
      key = keyIter.next()
      key
    }) != null) {
      val `val`: Sequence = get(key)
      if (keyType == null) {
        keyType = key.getItemType
        valueType = SequenceTool.getItemType(`val`, th)
        valueCard = SequenceTool.getCardinality(`val`)
      } else {
        keyType = Type
          .getCommonSuperType(keyType, key.getItemType, th)
          .asInstanceOf[AtomicType]
        valueType = Type.getCommonSuperType(
          valueType,
          SequenceTool.getItemType(`val`, th),
          th)
        valueCard =
          Cardinality.union(valueCard, SequenceTool.getCardinality(`val`))
      }
    }
    if (keyType == null) {
      this.keyUType = UType.VOID
      this.valueUType = UType.VOID
      this.valueCardinality = 0
      MapType.ANY_MAP_TYPE
    } else {
      this.keyUType = keyType.getUType
      this.valueUType = valueType.getUType
      this.valueCardinality = valueCard
      new MapType(keyType, SequenceType.makeSequenceType(valueType, valueCard))
    }
  }

  def addEntry(key: AtomicValue, value: GroundedValue): HashTrieMap = {
    val empty: Boolean = isEmpty
    val imap2: ImmutableMap[AtomicMatchKey, KeyValuePair] =
      imap.put(makeKey(key), new KeyValuePair(key, value))
    val t2: HashTrieMap = new HashTrieMap(imap2)
    t2.valueCardinality = this.valueCardinality
    t2.keyUType = keyUType
    t2.valueUType = valueUType
    t2.keyAtomicType = keyAtomicType
    t2.valueItemType = valueItemType
    t2.updateTypeInformation(key, value, empty)
    t2
  }

  def initialPut(key: AtomicValue, value: GroundedValue): Boolean = {
    val empty: Boolean = isEmpty
    val exists: Boolean = get(key) != null
    imap = imap.put(makeKey(key), new KeyValuePair(key, value))
    updateTypeInformation(key, value, empty)
    entries = -1
    exists
  }

  private def makeKey(key: AtomicValue): AtomicMatchKey = key.asMapKey()

  def remove(key: AtomicValue): HashTrieMap = {
    val m2: ImmutableMap[AtomicMatchKey, KeyValuePair] =
      imap.remove(makeKey(key))
    if (m2 == imap) {
      return this
    }
    val result: HashTrieMap = new HashTrieMap(m2)
    result.keyUType = keyUType
    result.valueUType = valueUType
    result.valueCardinality = valueCardinality
    result.entries = entries - 1
    result
  }

  def get(key: AtomicValue): GroundedValue = {
    val o: KeyValuePair = imap.get(makeKey(key))
    if (o == null) null else o.value
  }

  def getKeyValuePair(key: AtomicValue): KeyValuePair = imap.get(makeKey(key))

  def keys: AtomicIterator[_ <: AtomicValue] =
    new AtomicIterator[AtomicValue]() {
      var base: Iterator[Tuple2[AtomicMatchKey, KeyValuePair]] =
        imap.iterator

      def next(): AtomicValue =
        if (base.hasNext) {
          base.next()._2.key
        } else {
          null
        }
    }

  def keyValuePairs(): java.lang.Iterable[KeyValuePair] =
    () =>
      new Iterator[KeyValuePair]() {
        var base: Iterator[Tuple2[AtomicMatchKey, KeyValuePair]] =
          imap.iterator

        def hasNext: Boolean = base.hasNext

        def next(): KeyValuePair = base.next()._2

        override def remove(): Unit = {
          base.remove()
        }
      }

  def diagnosticDump(): Unit = {
    System.err.println("Map details:")
    for (entry <- imap.asScala) {
      val k1: AtomicMatchKey = entry._1
      val k2: AtomicValue = entry._2.key
      val v: Sequence = entry._2.value
      System.err.println(
        k1.getClass.toString + " " + k1.toString + " #:" + k1.hashCode.toString + " = (" +
          k2.getClass.toString +
          " " +
          k2.toString +
          " : " +
          v.toString +
          ")")
    }
  }

  override def toString: String = MapItem.mapToString(this)

}
