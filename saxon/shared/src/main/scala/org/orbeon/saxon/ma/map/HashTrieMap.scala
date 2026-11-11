package org.orbeon.saxon.ma.map

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{GroundedValue, Sequence, SequenceTool}

import java.util.Iterator

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.{AtomicValue, Cardinality, SequenceType}
import scala.jdk.CollectionConverters._

object HashTrieMap {

  def singleton(key: AtomicValue, value: GroundedValue): HashTrieMap =
    new HashTrieMap().addEntry(key, value)

  def copy(map: MapItem): HashTrieMap =
    map match {
      case hashTrieMap: HashTrieMap =>
        hashTrieMap
      case _                 =>
        var m2 = new HashTrieMap
        for (pair <- map.keyValuePairs)
          m2 = m2.addEntry(pair.key, pair.value)
        m2
    }
}

class HashTrieMap extends MapItem {

  private var imap: Map[AtomicMatchKey, KeyValuePair] = Map.empty

  var keyUType: UType = UType.VOID

  private var valueUType: UType = UType.VOID
  private var keyAtomicType: AtomicType = ErrorType
  private var valueItemType: ItemType = ErrorType
  private var valueCardinality: Int = 0

  def this(imap: Map[AtomicMatchKey, KeyValuePair]) = {
    this()
    this.imap = imap
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
      if (key.getItemType != keyAtomicType)
        keyAtomicType = null
      if (!MapItem.isKnownToConform(`val`, valueItemType)) {
        valueItemType = null
      }
    }
  }

  def size: Int = imap.size

  def isEmpty: Boolean = imap.isEmpty

  override def conforms(requiredKeyType: AtomicType,
                        requiredValueType: SequenceType,
                        th: TypeHierarchy): Boolean = {
    if (isEmpty)
      return true
    if (keyAtomicType == requiredKeyType && valueItemType == requiredValueType.getPrimaryType &&
      Cardinality.subsumes(requiredValueType.getCardinality,
        valueCardinality)) {
      return true
    }
    var needFullCheck = false
    if (requiredKeyType != BuiltInAtomicType.ANY_ATOMIC) {
      val upperBoundKeyType = keyUType.toItemType
      val rel               = th.relationship(requiredKeyType, upperBoundKeyType)
      if (rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMES) {} else if (rel == Affinity.DISJOINT)
        return false
      else
        needFullCheck = true
    }
    val requiredValueItemType = requiredValueType.getPrimaryType
    if (requiredValueItemType != BuiltInAtomicType.ANY_ATOMIC) {
      val upperBoundValueType = valueUType.toItemType
      val rel                 = th.relationship(requiredValueItemType, upperBoundValueType)
      if (rel == Affinity.SAME_TYPE || rel == Affinity.SUBSUMES) {} else if (rel == Affinity.DISJOINT)
        return false
      else
        needFullCheck = true
    }
    val requiredValueCard = requiredValueType.getCardinality
    if (! Cardinality.subsumes(requiredValueCard, valueCardinality)) {
      needFullCheck = true
    }
    if (needFullCheck) {
      val keyIter = keys
      var key: AtomicValue = null
      while ({
        key = keyIter.next()
        key
      } != null) {
        if (! requiredKeyType.matches(key, th))
          return false
        val `val` = get(key)
        if (! requiredValueType.matches(`val`, th))
          return false
      }
    }
    true
  }

  override def getItemType(th: TypeHierarchy): MapType = {
    var keyType: AtomicType = null
    var valueType: ItemType = null
    var valueCard           = 0
    val keyIter             = keys
    var key: AtomicValue = null
    while ({
      key = keyIter.next()
      key
    } != null) {
      val `val` = get(key)
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
    val empty = isEmpty
    val imap2 = imap.updated(makeKey(key), KeyValuePair(key, value))
    val t2    = new HashTrieMap(imap2)

    t2.valueCardinality = this.valueCardinality
    t2.keyUType         = keyUType
    t2.valueUType       = valueUType
    t2.keyAtomicType    = keyAtomicType
    t2.valueItemType    = valueItemType
    t2.updateTypeInformation(key, value, empty)
    t2
  }

  def initialPut(key: AtomicValue, value: GroundedValue): Boolean = {
    val empty  = isEmpty
    val k      = makeKey(key)
    val exists = imap.contains(k)
    imap = imap.updated(k, KeyValuePair(key, value))
    updateTypeInformation(key, value, empty)
    exists
  }

  private def makeKey(key: AtomicValue): AtomicMatchKey = key.asMapKey()

  def remove(key: AtomicValue): HashTrieMap = {
    val k = makeKey(key)
    if (! imap.contains(k))
      return this
    val m2 = imap.removed(k)
    val result = new HashTrieMap(m2)
    result.keyUType = keyUType
    result.valueUType = valueUType
    result.valueCardinality = valueCardinality
    result
  }

  def get(key: AtomicValue): GroundedValue =
    imap.get(makeKey(key)).map(_.value).orNull

  def getKeyValuePair(key: AtomicValue): KeyValuePair =
    imap.get(makeKey(key)).orNull

  def keys: AtomicIterator[_ <: AtomicValue] =
    new AtomicIterator[AtomicValue]() {
      val base = imap.valuesIterator

      def next(): AtomicValue =
        if (base.hasNext)
          base.next().key
        else
          null
    }

  def keyValuePairs: Iterable[KeyValuePair] = imap.values

  def diagnosticDump(): Unit = {
    System.err.println("Map details:")
    for ((k1, entry) <- imap) {
      val k2 = entry.key
      val v  = entry.value
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
