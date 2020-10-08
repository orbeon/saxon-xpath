package org.orbeon.saxon.s9api

import java.io.{File, OutputStream, StringWriter, Writer}
import java.net.{URI, URISyntaxException}
import java.util.{HashMap, Map}

import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, Result, Source}
import org.orbeon.saxon.event._
import org.orbeon.saxon.lib.{NamespaceConstant, SaxonOutputKeys, SerializerFactory}
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.query.QueryResult
import org.orbeon.saxon.serialize.{CharacterMapIndex, SerializationProperties}

//import scala.collection.compat._
import java.util.Properties

import org.orbeon.saxon.s9api.Serializer.Property.Property

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

object Serializer {

  private var standardProperties: Map[String, Property.Property] = new HashMap()

  for (p <- Property.values) {
    standardProperties.put(p.name, p)
  }

  object Property extends Enumeration {

    val METHOD: Property = new Property(OutputKeys.METHOD)

    val VERSION: Property = new Property(OutputKeys.VERSION)

    val ENCODING: Property = new Property(OutputKeys.ENCODING)

    val OMIT_XML_DECLARATION: Property = new Property(
      OutputKeys.OMIT_XML_DECLARATION)

    val STANDALONE: Property = new Property(OutputKeys.STANDALONE)

    val DOCTYPE_PUBLIC: Property = new Property(OutputKeys.DOCTYPE_PUBLIC)

    val DOCTYPE_SYSTEM: Property = new Property(OutputKeys.DOCTYPE_SYSTEM)

    val CDATA_SECTION_ELEMENTS: Property = new Property(
      OutputKeys.CDATA_SECTION_ELEMENTS)

    val INDENT: Property = new Property(OutputKeys.INDENT)

    val MEDIA_TYPE: Property = new Property(OutputKeys.MEDIA_TYPE)

    val USE_CHARACTER_MAPS: Property = new Property(
      SaxonOutputKeys.USE_CHARACTER_MAPS)

    val INCLUDE_CONTENT_TYPE: Property = new Property(
      SaxonOutputKeys.INCLUDE_CONTENT_TYPE)

    val UNDECLARE_PREFIXES: Property = new Property(
      SaxonOutputKeys.UNDECLARE_PREFIXES)

    val ESCAPE_URI_ATTRIBUTES: Property = new Property(
      SaxonOutputKeys.ESCAPE_URI_ATTRIBUTES)

    val BYTE_ORDER_MARK: Property = new Property(
      SaxonOutputKeys.BYTE_ORDER_MARK)

    val NORMALIZATION_FORM: Property = new Property(
      SaxonOutputKeys.NORMALIZATION_FORM)

    val ITEM_SEPARATOR: Property = new Property(SaxonOutputKeys.ITEM_SEPARATOR)

    val HTML_VERSION: Property = new Property(SaxonOutputKeys.HTML_VERSION)

    val BUILD_TREE: Property = new Property(SaxonOutputKeys.BUILD_TREE)

    val SAXON_INDENT_SPACES: Property = new Property(
      SaxonOutputKeys.INDENT_SPACES)

    val SAXON_LINE_LENGTH: Property = new Property(SaxonOutputKeys.LINE_LENGTH)

    val SAXON_ATTRIBUTE_ORDER: Property = new Property(
      SaxonOutputKeys.ATTRIBUTE_ORDER)

    val SAXON_CANONICAL: Property = new Property(SaxonOutputKeys.CANONICAL)

    val SAXON_NEWLINE: Property = new Property(SaxonOutputKeys.NEWLINE)

    val SAXON_SUPPRESS_INDENTATION: Property = new Property(
      SaxonOutputKeys.SUPPRESS_INDENTATION)

    val SAXON_DOUBLE_SPACE: Property = new Property(
      SaxonOutputKeys.DOUBLE_SPACE)

    val SAXON_STYLESHEET_VERSION: Property = new Property(
      SaxonOutputKeys.STYLESHEET_VERSION)

    val SAXON_CHARACTER_REPRESENTATION: Property = new Property(
      SaxonOutputKeys.CHARACTER_REPRESENTATION)

    val SAXON_RECOGNIZE_BINARY: Property = new Property(
      SaxonOutputKeys.RECOGNIZE_BINARY)

    val SAXON_REQUIRE_WELL_FORMED: Property = new Property(
      SaxonOutputKeys.REQUIRE_WELL_FORMED)

    val SAXON_WRAP: Property = new Property(SaxonOutputKeys.WRAP)

    val SAXON_SUPPLY_SOURCE_LOCATOR: Property = new Property(
      SaxonOutputKeys.SUPPLY_SOURCE_LOCATOR)

    class Property(var name: String) extends Val {

      override def toString: String = name

      def getQName: QName = QName.fromClarkName(name)

    }

    def get(s: String): Property =
      Property.values.find(_.name == s).getOrElse(null)

    implicit def convertValue(v: Value): Property = v.asInstanceOf[Property]

  }

  def getProperty(name: QName): Property.Property = {
    val clarkName: String = name.getClarkName
    val prop: Property.Property = standardProperties.get(clarkName)
    if (prop != null) {
      prop
    } else {
      throw new IllegalArgumentException(
        "Unknown serialization property " + clarkName)
    }
  }

}

class Serializer(@BeanProperty var processor: Processor)
  extends AbstractDestination {

  private var properties: Map[StructuredQName, String] = new HashMap(10)

  @BeanProperty
   var result: StreamResult = new StreamResult()

  private var characterMap: CharacterMapIndex = null

  private var mustClose: Boolean = false

  def setOutputProperties(suppliedProperties: Properties): Unit = {
    for (name <- suppliedProperties.stringPropertyNames().asScala) {
      properties.put(StructuredQName.fromClarkName(name),
        suppliedProperties.getProperty(name))
    }
  }

  def setOutputProperties(suppliedProperties: SerializationProperties): Unit = {
    this.setOutputProperties(suppliedProperties.getProperties)
    this.characterMap = suppliedProperties.getCharacterMapIndex
  }

  def setCloseOnCompletion(value: Boolean): Unit = {
    mustClose = value
  }

  def setCharacterMap(characterMap: CharacterMapIndex): Unit = {
    var existingIndex: CharacterMapIndex = this.characterMap
    if (existingIndex == null || existingIndex.isEmpty) {
      existingIndex = characterMap
    } else if (characterMap != null && !characterMap.isEmpty && existingIndex != characterMap) {
      existingIndex = existingIndex.copy()
      for (map <- characterMap.asScala) {
        existingIndex.putCharacterMap(map.getName, map)
      }
    }
    this.characterMap = existingIndex
  }

  def setOutputProperty(property: Property, value: String): Unit = {
    var valStr = value
    val sf: SerializerFactory =
      processor.getUnderlyingConfiguration.getSerializerFactory
    valStr = sf.checkOutputProperty(property.toString, valStr)
    if (valStr == null) {
      properties.remove(property.getQName.getStructuredQName)
    } else {
      properties.put(property.getQName.getStructuredQName, valStr)
    }
  }

  def getOutputProperty(property: Property): String =
    properties.get(property.getQName.getStructuredQName)

  def setOutputProperty(property: QName, value: String): Unit = {
    var valStr = value
    val sf: SerializerFactory =
      processor.getUnderlyingConfiguration.getSerializerFactory
    val uri: String = property.getNamespaceURI
    if (uri.isEmpty || uri == NamespaceConstant.SAXON) {
      valStr = sf.checkOutputProperty(property.getClarkName, valStr)
      if (uri == NamespaceConstant.SAXON && property.getLocalName.==(
        "next-in-chain")) {
        throw new IllegalArgumentException(
          "saxon:next-in-chain is not a valid serialization property")
      }
    }
    if (valStr == null) {
      properties.remove(property.getStructuredQName)
    } else {
      properties.put(property.getStructuredQName, valStr)
    }
  }

  def getOutputProperty(property: QName): String =
    properties.get(property.getStructuredQName)

  def setOutputWriter(writer: Writer): Unit = {
    result.setOutputStream(null)
    result.setSystemId(null.asInstanceOf[String])
    result.setWriter(writer)
    mustClose = false
  }

  def setOutputStream(stream: OutputStream): Unit = {
    result.setWriter(null)
    result.setSystemId(null.asInstanceOf[String])
    result.setOutputStream(stream)
    mustClose = false
  }

  // ORBEON: No `File` support.
//  def setOutputFile(file: File): Unit = {
//    result.setOutputStream(null)
//    result.setWriter(null)
//    result.setSystemId(file)
//    this.setDestinationBaseURI(file.toURI())
//    mustClose = true
//  }

  def serializeNode(node: XdmNode): Unit = {
    val res: StreamResult = result
    if (res.getOutputStream == null && res.getWriter == null &&
      res.getSystemId == null) {
      throw new IllegalStateException(
        "Either an outputStream, or a Writer, or a File must be supplied")
    }
    serializeNodeToResult(node, res)
  }

  def serializeXdmValue(value: XdmValue): Unit = {
    if (value.isInstanceOf[XdmNode]) {
      serializeNode(value.asInstanceOf[XdmNode])
    } else {
      val properties: SerializationProperties =
        new SerializationProperties(getLocallyDefinedProperties, characterMap)
      QueryResult.serializeSequence(value.getUnderlyingValue.iterate(),
        processor.getUnderlyingConfiguration,
        result,
        properties)
    }
    closeAndNotify()
  }

  def serialize(source: Source): Unit = {
    val sf: SerializerFactory =
      processor.getUnderlyingConfiguration.getSerializerFactory
    val tr: Receiver = sf.getReceiver(
      result,
      new SerializationProperties(getLocallyDefinedProperties))
    Sender.send(source,
      tr,
      processor.getUnderlyingConfiguration.getParseOptions)
    closeAndNotify()
  }

  def serializeToString(source: Source): String = {
    val sf: SerializerFactory =
      processor.getUnderlyingConfiguration.getSerializerFactory
    val sw: StringWriter = new StringWriter()
    val tr: Receiver = sf.getReceiver(
      new StreamResult(sw),
      new SerializationProperties(getLocallyDefinedProperties))
    Sender.send(source,
      tr,
      processor.getUnderlyingConfiguration.getParseOptions)
    closeAndNotify()
    sw.toString
  }

  def serializeNodeToString(node: XdmNode): String = {
    val sw: StringWriter = new StringWriter()
    val sr: StreamResult = new StreamResult(sw)
    serializeNodeToResult(node, sr)
    sw.toString
  }

  private def serializeNodeToResult(node: XdmNode, res: Result): Unit = {
    QueryResult.serialize(node.getUnderlyingNode,
      res,
      getLocallyDefinedProperties)
  }

  def getXMLStreamWriter: StreamWriterToReceiver = {
    val pipe: PipelineConfiguration =
      processor.getUnderlyingConfiguration.makePipelineConfiguration
    var r: Receiver = getReceiver(pipe, getSerializationProperties)
    r = new NamespaceReducer(r)
    new StreamWriterToReceiver(r)
  }

  def getContentHandler: org.xml.sax.ContentHandler = {
    val pipe: PipelineConfiguration =
      processor.getUnderlyingConfiguration.makePipelineConfiguration
    var r: Receiver = getReceiver(pipe, getSerializationProperties)
    r = new NamespaceReducer(r)
    val rch: ReceivingContentHandler = new ReceivingContentHandler()
    rch.setReceiver(r)
    rch.setPipelineConfiguration(r.getPipelineConfiguration)
    rch
  }

  def getOutputDestination: AnyRef = {
    if (result.getOutputStream != null) {
      result.getOutputStream
    }
    if (result.getWriter != null) {
      result.getWriter
    }
    val systemId: String = result.getSystemId
    if (systemId != null) {
      try new File(new URI(systemId))
      catch {
        case e: URISyntaxException => null

      }
    } else {
      null
    }
  }

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    val sf: SerializerFactory = pipe.getConfiguration.getSerializerFactory
    val mergedParams: SerializationProperties =
      getSerializationProperties.combineWith(params)
    var target: Receiver = sf.getReceiver(result, mergedParams, pipe)
    if (helper.getListeners != null) {
      if (target.isInstanceOf[SequenceNormalizer]) {
        target.asInstanceOf[SequenceNormalizer].onClose(helper.getListeners)
      } else {
        target = new CloseNotifier(target, helper.getListeners)
      }
    }
    if (target.getSystemId == null && getDestinationBaseURI != null) {
      target.setSystemId(getDestinationBaseURI.toASCIIString())
    }
    target
  }

  def getCombinedOutputProperties(
                                   defaultOutputProperties: Properties): Properties = {
    val props: Properties =
      if (defaultOutputProperties == null) new Properties()
      else new Properties(defaultOutputProperties)
    for (p <- properties.keySet.asScala) {
      val value: String = properties.get(p)
      props.setProperty(p.getClarkName, value)
    }
    props
  }

   def getLocallyDefinedProperties: Properties = {
    val props: Properties = new Properties()
    for (p <- properties.keySet.asScala) {
      val value: String = properties.get(p)
      props.setProperty(p.getClarkName, value)
    }
    props
  }

  def getSerializationProperties: SerializationProperties =
    new SerializationProperties(getLocallyDefinedProperties, characterMap)

  def close(): Unit = {
    if (mustClose) {
      val stream: OutputStream = result.getOutputStream
      if (stream != null) {
        stream.close()
      }
      val writer: Writer = result.getWriter
      if (writer != null) {
        writer.close()
      }
    }
  }

  def isMustCloseAfterUse: Boolean = mustClose
}
