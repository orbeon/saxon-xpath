package net.sf.saxon.s9api

import net.sf.saxon.event.Receiver
import net.sf.saxon.event.SequenceCopier
import net.sf.saxon.expr.sort.DocumentOrderIterator
import net.sf.saxon.expr.sort.GlobalOrderComparer
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.om._
import net.sf.saxon.s9api.streams.Step
import net.sf.saxon.s9api.streams.XdmStream
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.ExternalObject
import net.sf.saxon.value.SequenceExtent
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import java.io.StringWriter
import java.util.ArrayList
import java.util.Iterator
import java.util.List
import java.util.Map
import java.util.stream.Stream
import java.util.stream.StreamSupport

import net.sf.saxon.s9apir.XdmFunctionItem
import net.sf.saxon.utils.Configuration

import scala.jdk.CollectionConverters._

object XdmValue {

   def fromGroundedValue(value: GroundedValue): XdmValue = {
    val xv: XdmValue = new XdmValue()
    xv.setValue(value)
    xv
  }

  def wrap(value: Sequence): XdmValue = {
    if (value == null) {
      XdmEmptySequence.getInstance
    }
    var gv: GroundedValue = null
    gv = value.materialize()
    if (gv.getLength == 0) {
      XdmEmptySequence.getInstance
    } else if (gv.getLength == 1) {
      val first: Item = gv.head()
      if (first.isInstanceOf[NodeInfo]) {
        new XdmNode(first.asInstanceOf[NodeInfo])
      } else if (first.isInstanceOf[AtomicValue]) {
        new XdmAtomicValue(first.asInstanceOf[AtomicValue], true)
      } else if (first.isInstanceOf[MapItem]) {
        new XdmMap(first.asInstanceOf[MapItem]).asInstanceOf[XdmValue]
      } else if (first.isInstanceOf[ArrayItem]) {
        new XdmArray(first.asInstanceOf[ArrayItem]).asInstanceOf[XdmValue]
      } else if (first.isInstanceOf[Function]) {
        new XdmFunctionItem(first.asInstanceOf[Function])
      } else if (first.isInstanceOf[ExternalObject[_]]) {
        new XdmExternalObject(first)
      } else {
        throw new IllegalArgumentException(
          "Unknown item type " + first.getClass)
      }
    } else {
      fromGroundedValue(gv)
    }
  }

  def wrap(value: AtomicSequence): XdmValue = value.getLength match {
    case 0 => XdmEmptySequence.getInstance
    case 1 => new XdmAtomicValue(value.head(), true)
    case _ => fromGroundedValue(value)

  }

  def makeSequence(list: java.lang.Iterable[_]): XdmValue = {
    val result: List[Item] = new ArrayList[Item]()
    for (o <- list.asScala) {
      val v: XdmValue = XdmValue.makeValue(o)
      if (v.isInstanceOf[XdmItem]) {
        result.add(v.getUnderlyingValue.asInstanceOf[Item])
      } else {
        result.add(new XdmArray(v).getUnderlyingValue)
      }
    }
    XdmValue.wrap(SequenceExtent.makeSequenceExtent(result))
  }

  def makeValue(o: Any): XdmValue =
    if (o.isInstanceOf[Sequence]) {
      XdmValue.wrap(o.asInstanceOf[Sequence])
    } else if (o.isInstanceOf[XdmValue]) {
      o.asInstanceOf[XdmValue]
    } else if (o.isInstanceOf[Map[_, _]]) {
      XdmMap.makeMap(o.asInstanceOf[Map[_, _]]).asInstanceOf[XdmValue]
    } else if (o.isInstanceOf[Array[Any]]) {
      XdmArray.makeArray(o.asInstanceOf[Array[Any]]).asInstanceOf[XdmValue]
    } else if (o.isInstanceOf[java.lang.Iterable[_]]) {
      XdmValue.makeSequence(o.asInstanceOf[java.lang.Iterable[_]])
    } else {
      XdmAtomicValue.makeAtomicValue(o.asInstanceOf[AnyRef])
    }

}

class XdmValue extends java.lang.Iterable[XdmItem] {

  private var value: GroundedValue = _

  def this(items: java.lang.Iterable[_ <: XdmItem]) = {
    this()
    val values: List[Item] = new ArrayList[Item]()
    for (item <- items.asScala) {
      values.add(item.getUnderlyingValue)
    }
    value = new SequenceExtent(values)
  }

  def this(iterator: Iterator[_ <: XdmItem]) = {
    this()
    val values: List[Item] = new ArrayList[Item]()
    while (iterator.hasNext) values.add(iterator.next().getUnderlyingValue)
    value = new SequenceExtent(values)
  }

  def this(stream: Stream[_ <: XdmItem]) = this(stream.iterator())

   def setValue(value: GroundedValue): Unit = {
    this.value = value
  }

  def append(otherValue: XdmValue): XdmValue = {
    val values: List[Item] = new ArrayList[Item]()
    for (item <- this.asScala) {
      values.add(item.getUnderlyingValue)
    }
    for (item <- otherValue.asScala) {
      values.add(item.getUnderlyingValue)
    }
    val gv: GroundedValue = SequenceExtent.makeSequenceExtent(values)
    XdmValue.fromGroundedValue(gv)
  }

  def size(): Int = value.getLength

  def isEmpty(): Boolean = value.head() == null

  def itemAt(n: Int): XdmItem = {
    if (n < 0 || n >= size) {
      throw new IndexOutOfBoundsException("" + n)
    }
    val item: Item = SequenceTool.itemAt(value, n)
    XdmItem.wrapItem(item)
  }

  def iterator(): Iterator[XdmItem] = {
    val v: Sequence = getUnderlyingValue
    new XdmSequenceIterator(v.iterate())
  }

  def getUnderlyingValue(): GroundedValue = value

  override def toString(): String =
    try {
      var config: Configuration = null
      val iter: SequenceIterator = value.iterate()
      var item: Item = null
      while ((item = iter.next()) != null) if (item.isInstanceOf[NodeInfo]) {
        config = item.asInstanceOf[NodeInfo].getConfiguration
        //break
      }
      if (config == null) {
        config = Configuration.newConfiguration
      }
      val writer: StringWriter = new StringWriter()
      val result: StreamResult = new StreamResult(writer)
      val properties: SerializationProperties = new SerializationProperties()
      properties.setProperty(OutputKeys.METHOD, "adaptive")
      properties.setProperty(OutputKeys.INDENT, "true")
      properties.setProperty(OutputKeys.OMIT_XML_DECLARATION, "true")
      val r: Receiver =
        config.getSerializerFactory.getReceiver(result, properties)
      SequenceCopier.copySequence(value.iterate(), r)
      writer.toString
    } catch {
      case e: XPathException => super.toString

    }

  def documentOrder(): XdmValue = {
    val iter: SequenceIterator = value.iterate()
    val sorted: SequenceIterator =
      new DocumentOrderIterator(iter, GlobalOrderComparer.getInstance)
    XdmValue.fromGroundedValue(sorted.materialize())
  }

  def stream(): XdmStream[_ <: XdmItem] =
    new XdmStream(StreamSupport.stream(spliterator(), false))

  def select[T <: XdmItem](step: Step[T]): XdmStream[T] =
    stream().flatMapToXdm(step)

}
