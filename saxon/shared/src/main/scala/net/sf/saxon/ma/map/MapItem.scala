package net.sf.saxon.ma.map

import net.sf.saxon.expr.ContextOriginator

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.OperandRole

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.functions.DeepEqual

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.query.AnnotationList

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.SequenceType

import scala.jdk.CollectionConverters._

object MapItem {

  def isKnownToConform(value: Sequence, itemType: ItemType): Boolean = {
    if (itemType == AnyItemType) {
      return true
    }
    try {
      val iter: SequenceIterator = value.iterate()
      var item: Item = null
      while (({
        item = iter.next()
        item
      }) != null) if (item
        .isInstanceOf[AtomicValue]) {
        if (itemType.isInstanceOf[AtomicType]) {
          if (!Type.isSubType(item.asInstanceOf[AtomicValue].getItemType,
            itemType.asInstanceOf[AtomicType])) {
            false
          }
        } else {
          false
        }
      } else if (item.isInstanceOf[NodeInfo]) {
        if (itemType.isInstanceOf[NodeTest]) {
          if (!itemType
            .asInstanceOf[NodeTest]
            .test(item.asInstanceOf[NodeInfo])) {
            false
          }
        } else {
          false
        }
      } else {
        false
      }
      true
    } catch {
      case e: XPathException => false

    }
  }

  def getItemTypeOfSequence(`val`: Sequence): ItemType =
    try {
      val first: Item = `val`.head()
      if (first == null) {
        AnyItemType
      } else {
        var `type`: ItemType = null
        `type` =
          if (first.isInstanceOf[AtomicValue])
            first.asInstanceOf[AtomicValue].getItemType
          else if (first.isInstanceOf[NodeInfo])
            NodeKindTest.makeNodeKindTest(
              first.asInstanceOf[NodeInfo].getNodeKind)
          else AnyFunctionType.getInstance
        if (isKnownToConform(`val`, `type`)) {
          `type`
        } else {
          AnyItemType
        }
      }
    } catch {
      case e: XPathException => AnyItemType

    }

  def mapToString(map: MapItem): String = {
    val buffer: FastStringBuffer = new FastStringBuffer(256)
    buffer.append("map{")
    for (pair <- map.keyValuePairs().asScala) {
      if (buffer.length > 4) {
        buffer.append(",")
      }
      buffer.append(pair.key.toString)
      buffer.append(":")
      buffer.append(pair.value.toString)
    }
    buffer.append("}")
    buffer.toString
  }

}

trait MapItem extends Function {

  def get(key: AtomicValue): GroundedValue

  def size(): Int

  def isEmpty(): Boolean

  def keys(): AtomicIterator[_ <: AtomicValue]

  def keyValuePairs(): java.lang.Iterable[KeyValuePair]

  def addEntry(key: AtomicValue, value: GroundedValue): MapItem

  def remove(key: AtomicValue): MapItem

  def conforms(keyType: AtomicType,
               valueType: SequenceType,
               th: TypeHierarchy): Boolean

  def getItemType(th: TypeHierarchy): ItemType

  def getKeyUType(): UType

  override def toShortString(): String = {
    val sb: StringBuilder = new StringBuilder()
    sb.append("map{")
    val sizeInt: Int = size
    if (sizeInt == 0) {
      sb.append("}")
    } else if (sizeInt <= 5) {
      var pos: Int = 0
      for (pair <- keyValuePairs().asScala) {
        if ( {
          pos += 1;
          pos - 1
        } > 0) {
          sb.append(",")
        }
        sb.append(Err.depict(pair.key))
          .append(":")
          .append(Err.depictSequence(pair.value))
      }
      sb.append("}")
    } else {
      sb.append("(:size ").append(sizeInt).append(":)}")
    }
    sb.toString
  }

  override def getGenre(): Genre.Genre = Genre.MAP

  def isArray(): Boolean = false

  def isMap(): Boolean = true

  override def getAnnotations(): AnnotationList = AnnotationList.EMPTY

  def atomize(): AtomicSequence =
    throw new XPathException("Cannot atomize a map (" + toShortString() + ")",
      "FOTY0013")

  def getOperandRoles(): Array[OperandRole] = Array(OperandRole.SINGLE_ATOMIC)

  def getFunctionItemType(): FunctionItemType = MapType.ANY_MAP_TYPE

  def getFunctionName(): StructuredQName = null

  def getDescription(): String = "map"

  def getArity(): Int = 1

  def makeNewContext(callingContext: XPathContext,
                     originator: ContextOriginator): XPathContext =
    callingContext

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val key: AtomicValue = args(0).head().asInstanceOf[AtomicValue]
    val value: Sequence = get(key)
    if (value == null) {
      EmptySequence.getInstance
    } else {
      value
    }
  }

  def getStringValue(): String =
    throw new UnsupportedOperationException("A map has no string value")

  def getStringValueCS(): CharSequence =
    throw new UnsupportedOperationException("A map has no string value")

  def getTypedValue(): SequenceIterator =
    throw new XPathException("A map has no typed value")

  def deepEquals(other: Function,
                 context: XPathContext,
                 comparer: AtomicComparer,
                 flags: Int): Boolean = {
    if (other
      .isInstanceOf[MapItem] && other.asInstanceOf[MapItem].size == size) {
      val keysL: AtomicIterator[_ <: AtomicValue] = keys
      var key: AtomicValue = null
      while (({
        key = keysL.next()
        key
      }) != null) {
        val thisValue: Sequence = get(key)
        val otherValue: Sequence = other.asInstanceOf[MapItem].get(key)
        if (otherValue == null) {
          return false
        }
        if (!DeepEqual.deepEqual(otherValue.iterate(),
          thisValue.iterate(),
          comparer,
          context,
          flags)) {
          false
        }
      }
      return true
    }
    false
  }

  override def itemAt(n: Int): MapItem = if (n == 0) this else null

  override def effectiveBooleanValue(): Boolean =
    throw new XPathException("A map item has no effective boolean value")

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("map")
    out.emitAttribute("size", "" + size)
    for (kvp <- keyValuePairs().asScala) {
      Literal.exportAtomicValue(kvp.key, out)
      Literal.exportValue(kvp.value, out)
    }
    out.endElement()
  }

  def isTrustedResultType(): Boolean = true

}
