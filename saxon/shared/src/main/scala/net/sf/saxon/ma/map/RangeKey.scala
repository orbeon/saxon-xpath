package net.sf.saxon.ma.map

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.sort.AtomicComparer
import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.functions.Count
import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.model.UType
import net.sf.saxon.om.Function
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.Sequence
import net.sf.saxon.regex.UnicodeString
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.value.{AtomicValue, _}
import java.util.Iterator
import java.util.List
import java.util.TreeMap


class RangeKey
  extends MapItem {

  private var min: UnicodeString = _
  private var max: UnicodeString = _
  private var index: TreeMap[AtomicMatchKey, Any] = _

  def this(min: UnicodeString, max: UnicodeString, index: TreeMap[AtomicMatchKey, Any]) {
    this()
    this.min = if (min == null) null else UnicodeString.makeUnicodeString(min)
    this.max = if (max == null) null else UnicodeString.makeUnicodeString(max)
    this.index = index
  }


  def get(key: AtomicValue): GroundedValue = {
    val k: UnicodeString =
      UnicodeString.makeUnicodeString(key.getStringValueCS)
    if ((min == null || min.compareTo(k) <= 0) && (max == null || max
      .compareTo(k) >= 0)) {
      val value: Any = index.get(k)
      if (value == null) {
        EmptySequence.getInstance
      } else if (value.isInstanceOf[NodeInfo]) {
        value.asInstanceOf[NodeInfo]
      } else {
        val nodes: List[NodeInfo] = value.asInstanceOf[List[NodeInfo]]
        SequenceExtent.makeSequenceExtent(nodes)
      }
    }
    EmptySequence.getInstance
  }

  def size(): Int =
    try Count.count(keys)
    catch {
      case err: XPathException => 0

    }

  def isEmpty(): Boolean = keys.next() == null

  def keys(): AtomicIterator[_ <: AtomicValue] = new RangeKeyIterator()

  def keyValuePairs(): java.lang.Iterable[KeyValuePair] =
    () =>
      new Iterator[KeyValuePair]() {
        var keyAtItr: AtomicIterator[_ <: AtomicValue] = keys()

        var nextVal: AtomicValue = keyAtItr.next()

        def hasNext(): Boolean = nextVal != null

        def next(): KeyValuePair =
          if (nextVal == null) {
            null
          } else {
            val kvp: KeyValuePair = new KeyValuePair(nextVal, get(nextVal))
            nextVal = keyAtItr.next()
            kvp
          }
      }

  def remove(key: AtomicValue): MapItem = HashTrieMap.copy(this).remove(key)

  def getKeyUType(): UType = UType.STRING

  override def addEntry(key: AtomicValue, value: GroundedValue): MapItem =
    HashTrieMap.copy(this).addEntry(key, value)

  override def conforms(keyType: AtomicType,
                        valueType: SequenceType,
                        th: TypeHierarchy): Boolean = {
    val keyIter: AtomicIterator[_ <: AtomicValue] = keys
    var key: AtomicValue = null
    while (({
      key = keyIter.next()
      key
    }) != null) {
      val value: Sequence = get(key)
      if (!valueType.matches(value, th)) {
        false
      }
    }
    true
  }

  override def getItemType(th: TypeHierarchy): MapType =
    new MapType(BuiltInAtomicType.STRING, SequenceType.NODE_SEQUENCE)

  override def getFunctionItemType(): MapType =
    new MapType(BuiltInAtomicType.STRING, SequenceType.NODE_SEQUENCE)

  override def getDescription(): String = "range key"

  override def deepEquals(other: Function,
                          context: XPathContext,
                          comparer: AtomicComparer,
                          flags: Int): Boolean =
    if (other.isInstanceOf[RangeKey]) {
      val rk: RangeKey = other.asInstanceOf[RangeKey]
      min == rk.min && max == rk.max && index == rk.index
    } else {
      false
    }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("range-key-map")
    out.emitAttribute("size", size.toString)
    out.endElement()
  }

  override def isTrustedResultType(): Boolean = false

  override def toString: String = MapItem.mapToString(this)

  private class RangeKeyIterator extends AtomicIterator[StringValue] {

    var pos: Int = 0
    var curr: UnicodeString = null
    var top: UnicodeString = (if (max == null) index.lastKey() else index.floorKey(max)).asInstanceOf[UnicodeString]

    def next(): StringValue = {
      if (pos <= 0) {
        if (pos < 0) {
          return null
        }
        if (min == null) {
          curr = index.firstKey().asInstanceOf[UnicodeString]
        } else {
          curr = index.ceilingKey(min).asInstanceOf[UnicodeString]
          if (curr != null && max != null && curr.compareTo(max) > 0) {
            curr = null
          }
        }
      } else
        curr =
          if (curr == top) null
          else index.higherKey(curr).asInstanceOf[UnicodeString]
      if (curr == null) {
        pos = -1
        null
      } else {
        {
          pos += 1
        }
        new StringValue(curr)
      }
    }

  }

}
