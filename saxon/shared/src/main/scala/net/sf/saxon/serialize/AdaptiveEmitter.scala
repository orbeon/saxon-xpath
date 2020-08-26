package net.sf.saxon.serialize

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.ReceiverWithOutputProperties

import net.sf.saxon.event.SequenceWriter

import net.sf.saxon.functions.FormatNumber

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.SaxonOutputKeys

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.ma.map.KeyValuePair

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.query.QueryResult

import net.sf.saxon.serialize.codenorm.Normalizer

import scala.jdk.CollectionConverters._

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.DoubleValue

import net.sf.saxon.value.QualifiedNameValue

import javax.xml.transform.stream.StreamResult

import java.io.IOException

import java.io.StringWriter

import java.io.Writer

import java.util.Properties

import scala.beans.{BeanProperty, BooleanBeanProperty}


class AdaptiveEmitter(pipe: PipelineConfiguration, private var writer: Writer)
  extends SequenceWriter(pipe)
    with ReceiverWithOutputProperties {

  private var characterMap: CharacterMap = _

  @BeanProperty
  var output_Properties: Properties = _

  private var itemSeparator: String = "\n"

  private var started: Boolean = false

  def setOutputProperties(props: Properties): Unit = {
    output_Properties = props
    val sep: String = props.getProperty(SaxonOutputKeys.ITEM_SEPARATOR)
    if (sep != null && "#absent" != sep) {
      itemSeparator = sep
    }
  }

  def getOutputProperties() : Properties = {
    return output_Properties
  }


  def setNormalizer(normalizer: Normalizer): Unit = {}

  def setCharacterMap(map: CharacterMap): Unit = {
    this.characterMap = map
  }

  private def emit(s: CharSequence): Unit = {
    writer.append(s)
  }

  override def write(item: Item): Unit = {
    if (started) {
      emit(itemSeparator)
    } else {
      if (writer == null) {}
      started = true
    }
    serializeItem(item)
  }

  private def serializeItem(item: Item): Unit = {
    if (item.isInstanceOf[AtomicValue]) {
      emit(serializeAtomicValue(item.asInstanceOf[AtomicValue]))
    } else if (item.isInstanceOf[NodeInfo]) {
      serializeNode(item.asInstanceOf[NodeInfo])
    } else if (item.isInstanceOf[MapItem]) {
      serializeMap(item.asInstanceOf[MapItem])
    } else if (item.isInstanceOf[ArrayItem]) {
      serializeArray(item.asInstanceOf[ArrayItem])
    } else if (item.isInstanceOf[Function]) {
      serializeFunction(item.asInstanceOf[Function])
    }
  }

  private def serializeAtomicValue(value: AtomicValue): String =
    value.getPrimitiveType.getFingerprint match {
      case StandardNames.XS_STRING | StandardNames.XS_ANY_URI |
           StandardNames.XS_UNTYPED_ATOMIC => {
        var s: String = value.getStringValue
        if (s.contains("\"")) {
          s = s.replace("\"", "\"\"")
        }
        if (characterMap != null) {
          s = characterMap.map(s, false).toString
        }
        "\"" + s + "\""
      }
      case StandardNames.XS_BOOLEAN =>
        if (value.effectiveBooleanValue()) "true()" else "false()"
      case StandardNames.XS_DECIMAL | StandardNames.XS_INTEGER =>
        value.getStringValue
      case StandardNames.XS_DOUBLE =>
        FormatNumber.formatExponential(value.asInstanceOf[DoubleValue])
      case StandardNames.XS_FLOAT | StandardNames.XS_DURATION |
           StandardNames.XS_DATE_TIME | StandardNames.XS_DATE |
           StandardNames.XS_TIME | StandardNames.XS_G_YEAR_MONTH |
           StandardNames.XS_G_MONTH | StandardNames.XS_G_MONTH_DAY |
           StandardNames.XS_G_YEAR | StandardNames.XS_G_DAY |
           StandardNames.XS_HEX_BINARY | StandardNames.XS_BASE64_BINARY =>
        value.getPrimitiveType.getDisplayName + "(\"" + value.getStringValue +
          "\")"
      case StandardNames.XS_DAY_TIME_DURATION |
           StandardNames.XS_YEAR_MONTH_DURATION =>
        "xs:duration(\"" + value.getStringValue + "\")"
      case StandardNames.XS_QNAME | StandardNames.XS_NOTATION =>
        value.asInstanceOf[QualifiedNameValue].getStructuredQName.getEQName
      case _ => "***"

    }

  private def serializeFunction(fn: Function): Unit = {
    val fname: StructuredQName = fn.getFunctionName
    if (fname == null || fname.hasURI(NamespaceConstant.ANONYMOUS)) {
      emit("(anonymous-function)")
    } else if (fname.hasURI(NamespaceConstant.FN)) {
      emit("fn:" + fname.getLocalPart)
    } else if (fname.hasURI(NamespaceConstant.MATH)) {
      emit("math:" + fname.getLocalPart)
    } else if (fname.hasURI(NamespaceConstant.MAP_FUNCTIONS)) {
      emit("map:" + fname.getLocalPart)
    } else if (fname.hasURI(NamespaceConstant.ARRAY_FUNCTIONS)) {
      emit("array:" + fname.getLocalPart)
    } else if (fname.hasURI(NamespaceConstant.SCHEMA)) {
      emit("xs:" + fname.getLocalPart)
    } else {
      emit(fname.getEQName)
    }
    emit("#" + fn.getArity)
  }

  private def serializeNode(node: NodeInfo): Unit = {
    node.getNodeKind match {
      case Type.ATTRIBUTE =>
        emit(node.getDisplayName)
        emit("=\"")
        emit(node.getStringValueCS)
        emit("\"")
      case Type.NAMESPACE =>
        emit(
          if (node.getLocalPart.isEmpty) "xmlns"
          else "xmlns:" + node.getLocalPart)
        emit("=\"")
        emit(node.getStringValueCS)
        emit("\"")
      case _ =>
        var sw: StringWriter = new StringWriter()
        var props: Properties = new Properties(output_Properties)
        var nodeMethod: String =
          output_Properties.getProperty(SaxonOutputKeys.JSON_NODE_OUTPUT_METHOD)
        if (nodeMethod == null) {
          nodeMethod = "xml"
        }
        props.setProperty("method", nodeMethod)
        props.setProperty("indent", "no")
        props.setProperty("omit-xml-declaration", "yes")
        props.setProperty(SaxonOutputKeys.UNFAILING, "yes")
        var cmi: CharacterMapIndex = null
        if (characterMap != null) {
          cmi = new CharacterMapIndex()
          cmi.putCharacterMap(characterMap.getName, characterMap)
        }
        var sProps: SerializationProperties =
          new SerializationProperties(props, cmi)
        QueryResult.serialize(node, new StreamResult(sw), sProps)
        emit(sw.toString.trim())

    }
  }

  private def serializeArray(array: ArrayItem): Unit = {
    emit("[")
    var first: Boolean = true
    for (seq <- array.members()) {
      if (first) {
        first = false
      } else {
        emit(",")
      }
      outputInternalSequence(seq)
    }
    emit("]")
  }

  private def serializeMap(map: MapItem): Unit = {
    emit("map{")
    var first: Boolean = true
    for (pair <- map.keyValuePairs().asScala) {
      if (first) {
        first = false
      } else {
        emit(",")
      }
      serializeItem(pair.key)
      emit(":")
      val value: Sequence = pair.value
      outputInternalSequence(value)
    }
    emit("}")
  }

  private def outputInternalSequence(value: Sequence): Unit = {
    var first: Boolean = true
    var it: Item = null
    val iter: SequenceIterator = value.iterate()
    val omitParens: Boolean = value.isInstanceOf[GroundedValue] && value
      .asInstanceOf[GroundedValue]
      .getLength == 1
    if (!omitParens) {
      emit("(")
    }
    while (({
      it = iter.next()
      it
    }) != null) {
      if (!first) {
        emit(",")
      }
      first = false
      serializeItem(it)
    }
    if (!omitParens) {
      emit(")")
    }
  }

  override def close(): Unit = {
    super.close()
    if (writer != null) {
      writer.close()
    }
  }

}
