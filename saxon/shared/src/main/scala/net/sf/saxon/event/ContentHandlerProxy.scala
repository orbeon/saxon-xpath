package net.sf.saxon.event

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.Logger

import net.sf.saxon.lib.SaxonOutputKeys

import net.sf.saxon.lib.TraceListener

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaException

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.AttributeCollectionImpl

import net.sf.saxon.value.Whitespace

import org.xml.sax.Attributes

import org.xml.sax.ContentHandler

import org.xml.sax.Locator

import org.xml.sax.SAXException

import org.xml.sax.ext.LexicalHandler

import javax.xml.transform.Result

import javax.xml.transform.sax.TransformerHandler

import java.util.Properties

import java.util.Stack
import scala.util.control.Breaks._
import ContentHandlerProxy._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._


object ContentHandlerProxy {

  private val MARKER: String = "##"

  class ContentHandlerProxyTraceListener extends TraceListener {

    @BeanProperty
    var contextItemStack: Stack[Item] = _

    def setOutputDestination(stream: Logger): Unit = ()

    def open(controller: Controller): Unit = {
      contextItemStack = new Stack()
    }

    def close(): Unit = {
      contextItemStack = null
    }

    def startCurrentItem(currentItem: Item): Unit = {
      if (contextItemStack == null) {
        contextItemStack = new Stack()
      }
      contextItemStack.push(currentItem)
    }

    def endCurrentItem(currentItem: Item): Unit = {
      contextItemStack.pop()
    }

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

  private var elementStack: Stack[String] = new Stack()

  private var namespaceStack: Stack[String] = new Stack()

  @BeanProperty
  lazy val traceListener: ContentHandlerProxyTraceListener =
    new ContentHandlerProxyTraceListener()

  @BeanProperty
  var currentLocation: Location = Loc.NONE

  def setUnderlyingContentHandler(handler: ContentHandler): Unit = {
    this.handler = handler
    if (handler.isInstanceOf[LexicalHandler]) {
      lexicalHandler = handler.asInstanceOf[LexicalHandler]
    }
  }

  def getUnderlyingContentHandler: ContentHandler = handler

  def setLexicalHandler(handler: LexicalHandler): Unit = {
    lexicalHandler = handler
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
  }

  def getPipelineConfiguration(): PipelineConfiguration = pipe

  def getConfiguration: Configuration = pipe.getConfiguration

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit = {
    if (handler.isInstanceOf[TransformerHandler]) {
      handler
        .asInstanceOf[TransformerHandler]
        .unparsedEntityDecl(name, publicID, systemID, "unknown")
    }
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
    currentLocation = location.saveLocation()
    namespaceStack.push(MARKER)
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
        namespaceStack.push(prefix)
      } catch {
        case err: SAXException => handleSAXException(err)

      }
    }
    var atts2: Attributes = null
    if (attributes.isInstanceOf[Attributes]) {
      atts2 = attributes.asInstanceOf[Attributes]
    } else {
      val aci: AttributeCollectionImpl =
        new AttributeCollectionImpl(getConfiguration, attributes.size)
      for (att <- attributes) {
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
        elementStack.push(uri)
        elementStack.push(localName)
        elementStack.push(qname)
      } catch {
        case e: SAXException => handleSAXException(e)

      }
    }
  }

  def endElement(): Unit = {
    if (depth > 0) {
      try {
        assert(!elementStack.isEmpty)
        val qname: String = elementStack.pop()
        val localName: String = elementStack.pop()
        val uri: String = elementStack.pop()
        handler.endElement(uri, localName, qname)
      } catch {
        case err: SAXException => handleSAXException(err)

      }
    }
    breakable {
      while (true) {
        val prefix: String = namespaceStack.pop()
        if (prefix == MARKER) {
          break()
        }
        try handler.endPrefixMapping(prefix)
        catch {
          case err: SAXException => handleSAXException(err)

        }
      }
    }
    depth -= 1
    if (requireWellFormed && depth <= 0) {
      depth = java.lang.Integer.MIN_VALUE
    }
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    currentLocation = locationId
    val disable: Boolean =
      ReceiverOption.contains(properties, ReceiverOption.DISABLE_ESCAPING)
    if (disable) {
      setEscaping(false)
    }
    try if (depth <= 0 && requireWellFormed) {
      if (Whitespace.isWhite(chars)) {} else {
        notifyNotWellFormed()
      }
    } else {
      handler.characters(chars.toString.toCharArray(), 0, chars.length)
    } catch {
      case err: SAXException => handleSAXException(err)

    }
    if (disable) {
      setEscaping(true)
    }
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
      lexicalHandler.comment(chars.toString.toCharArray(), 0, chars.length)
    } catch {
      case err: SAXException => handleSAXException(err)

    }
  }

  override def usesTypeAnnotations(): Boolean = false

  private def setEscaping(escaping: Boolean): Unit = {
    handler.processingInstruction(if (escaping)
      Result.PI_ENABLE_OUTPUT_ESCAPING
    else Result.PI_DISABLE_OUTPUT_ESCAPING,
      "")
  }

  private def handleSAXException(err: SAXException): Unit = {
    val nested: Exception = err.getException
    if (nested.isInstanceOf[XPathException]) {
      throw nested.asInstanceOf[XPathException]
    } else if (nested.isInstanceOf[SchemaException]) {
      throw new XPathException(nested)
    } else {
      val de: XPathException = new XPathException(err)
      de.setErrorCode(SaxonErrorCode.SXCH0003)
      throw de
    }
  }

  override def getSystemId: String = systemId

  override def setSystemId(systemId: String): Unit = this.systemId = systemId
}
