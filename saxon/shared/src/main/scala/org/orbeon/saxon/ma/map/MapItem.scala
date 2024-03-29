package org.orbeon.saxon.ma.map

import org.orbeon.saxon.expr.{ContextOriginator, Literal, OperandRole, XPathContext}
import org.orbeon.saxon.expr.sort.AtomicComparer
import org.orbeon.saxon.functions.DeepEqual
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.{NodeKindTest, NodeTest}
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{AtomicValue, EmptySequence, SequenceType}

import scala.jdk.CollectionConverters._


object MapItem {

  def isKnownToConform(value: Sequence, itemType: ItemType): Boolean = {

    if (itemType == AnyItemType)
      return true

    try {
      val iter = value.iterate()
      var item: Item = null
      while ({
        item = iter.next()
        item
      } != null)
        item match {
          case atomicValue: AtomicValue =>
            itemType match {
              case atomicType: AtomicType =>
                if (! Type.isSubType(atomicValue.getItemType, atomicType))
                  return false
              case _ =>
                return false
            }
          case nodeInfo: NodeInfo =>
            itemType match {
              case nodeTest: NodeTest =>
                if (! nodeTest.test(nodeInfo))
                  return false
              case _ =>
                return false
            }
          case _ =>
            return false
        }
      true
    } catch {
      case _: XPathException => false
    }
  }

  def getItemTypeOfSequence(`val`: Sequence): ItemType =
    try {
      val first = `val`.head
      if (first == null) {
        AnyItemType
      } else {
        val `type` =
          first match {
            case atomicValue: AtomicValue => atomicValue.getItemType
            case info: NodeInfo           => NodeKindTest.makeNodeKindTest(info.getNodeKind)
            case _                        => AnyFunctionType
          }
        if (isKnownToConform(`val`, `type`))
          `type`
        else
          AnyItemType
      }
    } catch {
      case _: XPathException => AnyItemType
    }

  def mapToString(map: MapItem): String = {
    val buffer = new FastStringBuffer(256)
    buffer.append("map{")
    for (pair <- map.keyValuePairs.asScala) {
      if (buffer.length > 4)
        buffer.append(",")
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
  def size: Int
  def isEmpty: Boolean
  def keys: AtomicIterator[_ <: AtomicValue]
  def keyValuePairs: java.lang.Iterable[KeyValuePair]
  def addEntry(key: AtomicValue, value: GroundedValue): MapItem
  def remove(key: AtomicValue): MapItem

  def conforms(keyType: AtomicType,
               valueType: SequenceType,
               th: TypeHierarchy): Boolean

  def getItemType(th: TypeHierarchy): ItemType

  def getKeyUType: UType

  override def toShortString: String = {
    val sb = new StringBuilder
    sb.append("map{")
    val sizeInt = size
    if (sizeInt == 0) {
      sb.append("}")
    } else if (sizeInt <= 5) {
      var pos = 0
      for (pair <- keyValuePairs.asScala) {
        if ({
          pos += 1
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

  override def getGenre: Genre.Genre = Genre.MAP

  def isArray: Boolean = false
  def isMap: Boolean = true

  def getAnnotations: AnnotationList = AnnotationList.EMPTY

  def atomize(): AtomicSequence =
    throw new XPathException("Cannot atomize a map (" + toShortString + ")", "FOTY0013")

  def getOperandRoles: Array[OperandRole] = Array(OperandRole.SINGLE_ATOMIC)
  def getFunctionItemType: FunctionItemType = MapType.ANY_MAP_TYPE
  def getFunctionName: StructuredQName = null
  def getDescription: String = "map"
  def getArity: Int = 1

  def makeNewContext(callingContext: XPathContext, originator: ContextOriginator): XPathContext =
    callingContext

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val key   = args(0).head.asInstanceOf[AtomicValue]
    val value = get(key)
    if (value == null)
      EmptySequence.getInstance
    else
      value
  }

  def getStringValue: String =
    throw new UnsupportedOperationException("A map has no string value")

  def getStringValueCS: CharSequence =
    throw new UnsupportedOperationException("A map has no string value")

  def getTypedValue: SequenceIterator =
    throw new XPathException("A map has no typed value")

  def deepEquals(
    other    : Function,
    context  : XPathContext,
    comparer : AtomicComparer,
    flags    : Int
  ): Boolean =
    other match {
      case item: MapItem if item.size == size =>
        val keysL = keys
        var key: AtomicValue = null
        while ({
          key = keysL.next()
          key
        } != null) {
          val thisValue  = get(key)
          val otherValue = item.get(key)
          if (otherValue == null)
            return false
          if (! DeepEqual.deepEqual(otherValue.iterate(),
            thisValue.iterate(),
            comparer,
            context,
            flags)) {
            return false
          }
        }
        true
      case _ =>
        false
    }

  override def itemAt(n: Int): MapItem = if (n == 0) this else null

  override def effectiveBooleanValue: Boolean =
    throw new XPathException("A map item has no effective boolean value")

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("map")
    out.emitAttribute("size", "" + size)
    for (kvp <- keyValuePairs.asScala) {
      Literal.exportAtomicValue(kvp.key, out)
      Literal.exportValue(kvp.value, out)
    }
    out.endElement()
  }

  def isTrustedResultType: Boolean = true
}
