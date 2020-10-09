package org.orbeon.saxon.event

import java.util.Properties

import javax.xml.transform.Result
import javax.xml.transform.sax.TransformerHandler
import org.orbeon.saxon.event.ContentHandlerProxy._
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.{Logger, SaxonOutputKeys, TraceListener}
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaException, SchemaType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
import org.orbeon.saxon.tree.util.AttributeCollectionImpl
import org.orbeon.saxon.utils.{Configuration, Controller}
import org.orbeon.saxon.value.Whitespace
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.{Attributes, ContentHandler, Locator, SAXException}

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


object ContentHandlerProxy {

  private val MARKER: String = "##"

  class ContentHandlerProxyTraceListener extends TraceListener {

    @BeanProperty
    var contextItemStack: List[Item] = Nil

    def setOutputDestination(stream: Logger): Unit = ()

    def open(controller: Controller): Unit =
      contextItemStack = Nil

    def close(): Unit =
      contextItemStack = null

    def startCurrentItem(currentItem: Item): Unit =
      contextItemStack ::= currentItem

    def endCurrentItem(currentItem: Item): Unit =
      contextItemStack = contextItemStack.tail
  }
}

class ContentHandlerProxy extends Receiver {

  private var pipe: PipelineConfiguration = _
  var systemId: String = _
  var handler: ContentHandler = _
  var lexicalHandler: LexicalHandler = _
  private var depth: Int = 0

  @BooleanBeanProperty
  var requireWellFormed: Boolean = false

  @BooleanBeanProperty
  var undeclareNamespaces: Boolean = false

  private var elementStack: List[String] = Nil
  private var namespaceStack: List[String] = Nil

  @BeanProperty
  lazy val traceListener: ContentHandlerProxyTraceListener =
    new ContentHandlerProxyTraceListener()

  @BeanProperty
  var currentLocation: Location = Loc.NONE

  def setUnderlyingContentHandler(handler: ContentHandler): Unit = {
    this.handler = handler
    handler match {
      case lexicalHandler: LexicalHandler => this.lexicalHandler = lexicalHandler
      case _ =>
    }
  }

  def getUnderlyingContentHandler: ContentHandler = handler

  def setLexicalHandler(handler: LexicalHandler): Unit = {
    lexicalHandler = handler
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
  }

  def getPipelineConfiguration: PipelineConfiguration = pipe

  def getConfiguration: Configuration = pipe.getConfiguration

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit =
    handler match {
      case transformerHandler: TransformerHandler =>
        transformerHandler.unparsedEntityDecl(name, publicID, systemID, "unknown")
      case _ =>
    }

  def setOutputProperties(details: Properties): Unit = {
    var prop: String = details.getProperty(SaxonOutputKeys.REQUIRE_WELL_FORMED)
    if (prop != null) {
      requireWellFormed = prop.==("yes")
    }
    prop = details.getProperty(SaxonOutputKeys.UNDECLARE_PREFIXES)
    if (prop != null) {
      undeclareNamespaces = prop.==("yes")
    }
  }

  def open(): Unit = {
    if (handler == null) {
      throw new IllegalStateException(
        "ContentHandlerProxy.open(): no underlying handler provided")
    }
    try {
      val locator: Locator = new ContentHandlerProxyLocator(this)
      handler.setDocumentLocator(locator)
      handler.startDocument()
    } catch {
      case err: SAXException => handleSAXException(err)

    }
    depth = 0
  }

  def close(): Unit = {
    if (depth >= 0) {
      try handler.endDocument()
      catch {
        case err: SAXException => handleSAXException(err)

      }
    }
    depth = -1
  }

  def startDocument(properties: Int): Unit = ()

  def endDocument(): Unit = ()

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    depth += 1
    if (depth <= 0 && requireWellFormed) {
      notifyNotWellFormed()
    }
    currentLocation = location.saveLocation
    namespaceStack ::= MARKER
    for (ns <- namespaces.asScala) {
      val prefix: String = ns.getPrefix
      if (prefix.==("xml")) {
        return
      }
      val uri: String = ns.getURI
      if (!undeclareNamespaces && uri.isEmpty && !prefix.isEmpty) {
        return
      }
      try {
        handler.startPrefixMapping(prefix, uri)
        namespaceStack ::= prefix
      } catch {
        case err: SAXException => handleSAXException(err)

      }
    }
    var atts2: Attributes = null
    attributes match {
      case atts: Attributes =>
        atts2 = atts
      case _ =>
        val aci: AttributeCollectionImpl =
          new AttributeCollectionImpl(getConfiguration, attributes.size)
        for (att <- attributes.iterator.asScala) {
          aci.addAttribute(att.getNodeName,
            BuiltInAtomicType.UNTYPED_ATOMIC,
            att.getValue,
            att.getLocation,
            att.getProperties)
        }
        atts2 = aci
    }
    if (depth > 0 || !requireWellFormed) {
      try {
        val uri: String = elemName.getURI
        val localName: String = elemName.getLocalPart
        val qname: String = elemName.getDisplayName
        handler.startElement(uri, localName, qname, atts2)
        elementStack ::= uri
        elementStack ::= localName
        elementStack ::= qname
      } catch {
        case e: SAXException => handleSAXException(e)

      }
    }
  }

  def endElement(): Unit = {
    if (depth > 0) {
      try {
        assert(elementStack.nonEmpty)
        val qname = elementStack.head
        elementStack = elementStack.tail
        val localName = elementStack.head
        elementStack = elementStack.tail
        val uri = elementStack.head
        elementStack = elementStack.tail
        handler.endElement(uri, localName, qname)
      } catch {
        case err: SAXException => handleSAXException(err)

      }
    }
    breakable {
      while (true) {
        val prefix: String = namespaceStack.head
        namespaceStack = namespaceStack.tail
        if (prefix == MARKER)
          break()
        try handler.endPrefixMapping(prefix)
        catch {
          case err: SAXException => handleSAXException(err)
        }
      }
    }
    depth -= 1
    if (requireWellFormed && depth <= 0)
      depth = java.lang.Integer.MIN_VALUE
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    currentLocation = locationId
    val disable = ReceiverOption.contains(properties, ReceiverOption.DISABLE_ESCAPING)
    if (disable)
      setEscaping(false)
    try
      if (depth <= 0 && requireWellFormed) {
        if (Whitespace.isWhite(chars)) {} else {
          notifyNotWellFormed()
        }
      } else {
        handler.characters(chars.toString.toCharArray, 0, chars.length)
      }
    catch {
      case err: SAXException =>
        handleSAXException(err)
    }
    if (disable)
      setEscaping(true)
  }

  def notifyNotWellFormed(): Unit = {
    val err = new XPathException(
      "The result tree cannot be supplied to the ContentHandler because it is not well-formed XML")
    err.setErrorCode(SaxonErrorCode.SXCH0002)
    throw err
  }

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    currentLocation = locationId
    try handler.processingInstruction(target, data.toString)
    catch {
      case err: SAXException => handleSAXException(err)
    }
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    currentLocation = locationId
    try if (lexicalHandler != null) {
      lexicalHandler.comment(chars.toString.toCharArray, 0, chars.length)
    } catch {
      case err: SAXException => handleSAXException(err)

    }
  }

  override def usesTypeAnnotations: Boolean = false

  private def setEscaping(escaping: Boolean): Unit =
    handler.processingInstruction(
      if (escaping)
        Result.PI_ENABLE_OUTPUT_ESCAPING
      else
        Result.PI_DISABLE_OUTPUT_ESCAPING,
      ""
    )

  private def handleSAXException(err: SAXException): Unit = {
    val nested: Exception = err.getException
    nested match {
      case e: XPathException =>
        throw e
      case _: SchemaException =>
        throw new XPathException(nested)
      case _ =>
        val de: XPathException = new XPathException(err)
        de.setErrorCode(SaxonErrorCode.SXCH0003)
        throw de
    }
  }

  def getSystemId: String = systemId
  def setSystemId(systemId: String): Unit = this.systemId = systemId
}
