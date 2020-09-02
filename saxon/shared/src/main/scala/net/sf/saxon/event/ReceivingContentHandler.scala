////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.Feature

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.QuitParsingException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XmlProcessingException

import net.sf.saxon.tree.tiny.CharSlice

import net.sf.saxon.tree.tiny.CompressedWhitespace

import net.sf.saxon.value.Whitespace

import org.xml.sax._

import org.xml.sax.ext.Attributes2

import org.xml.sax.ext.LexicalHandler

import javax.xml.transform.Result

import java.net.URI

import java.net.URISyntaxException

import java.util._

import ReceivingContentHandler._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object ReceivingContentHandler {

  class LocalLocator(private var saxLocator: Locator) extends Location {

    var levelInEntity: Int = 0

    def getSystemId: String = saxLocator.getSystemId

    def getPublicId: String = saxLocator.getPublicId

    def getLineNumber: Int = saxLocator.getLineNumber

    def getColumnNumber(): Int = saxLocator.getColumnNumber

    def saveLocation(): Location =
      new Loc(getSystemId, getLineNumber, getColumnNumber)

  }

}

class ReceivingContentHandler
  extends ContentHandler
    with LexicalHandler
    with DTDHandler {

  private var pipe: PipelineConfiguration = _

  @BeanProperty
  var receiver: Receiver = _

  // true while processing the DTD
  private var inDTD: Boolean = false

  private var localLocator: LocalLocator = new LocalLocator(Loc.NONE)

  private var lineNumbering: Boolean = _

  private var lastTextNodeLocator: Location = _

  private var buffer: Array[Char] = new Array[Char](512)

  private var charsUsed: Int = 0

  private var slice: CharSlice = new CharSlice(buffer, 0, 0)

  private var namespaceStack: Stack[NamespaceMap] = new Stack()

  private var currentNamespaceMap: NamespaceMap = NamespaceMap.emptyMap

  private var ignoreIgnorable: Boolean = false

  private var retainDTDAttributeTypes: Boolean = false

  private var allowDisableOutputEscaping: Boolean = false

  private var escapingDisabled: Boolean = false

  private var afterStartTag: Boolean = true

  private var nameCache: HashMap[String, HashMap[String, NodeName]] =
    new HashMap(10)

  private var noNamespaceNameCache: HashMap[String, NodeName] = new HashMap(10)

  // Action to be taken with defaulted attributes. 0=process normally, -1=suppress, +1=mark as defaulted
  private var defaultedAttributesAction: Int = 0

  // Stack holding depth of nesting of elements within external entities; created on first use
  private var elementDepthWithinEntity: Stack[Integer] = _

  namespaceStack.push(currentNamespaceMap)

  def reset(): Unit = {
    pipe = null
    receiver = null
    ignoreIgnorable = false
    retainDTDAttributeTypes = false
    charsUsed = 0
    slice.setLength(0)
    namespaceStack = new Stack()
    currentNamespaceMap = NamespaceMap.emptyMap
    namespaceStack.push(currentNamespaceMap)
    localLocator = new LocalLocator(Loc.NONE)
    allowDisableOutputEscaping = false
    escapingDisabled = false
    lineNumbering = false
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    val config: Configuration = pipe.getConfiguration
    ignoreIgnorable = pipe.getParseOptions.getSpaceStrippingRule != NoElementsSpaceStrippingRule.getInstance
    retainDTDAttributeTypes =
      config.getBooleanProperty(Feature.RETAIN_DTD_ATTRIBUTE_TYPES)
    if (!pipe.getParseOptions.isExpandAttributeDefaults) {
      defaultedAttributesAction = -1
    } else if (config.getBooleanProperty(Feature.MARK_DEFAULTED_ATTRIBUTES)) {
      defaultedAttributesAction = +1
    }
    allowDisableOutputEscaping =
      config.getConfigurationProperty(Feature.USE_PI_DISABLE_OUTPUT_ESCAPING)
    lineNumbering = pipe.getParseOptions.isLineNumbering
  }

  def getPipelineConfiguration: PipelineConfiguration = pipe

  def getConfiguration: Configuration = pipe.getConfiguration

  def setIgnoreIgnorableWhitespace(ignore: Boolean): Unit = {
    ignoreIgnorable = ignore
  }

  def isIgnoringIgnorableWhitespace: Boolean = ignoreIgnorable

  def startDocument(): Unit = {
    //        System.err.println("ReceivingContentHandler#startDocument");
    try {
      charsUsed = 0
      currentNamespaceMap = NamespaceMap.emptyMap
      namespaceStack = new Stack()
      namespaceStack.push(currentNamespaceMap)
      receiver.setPipelineConfiguration(pipe)
      val systemId: String = localLocator.getSystemId
      if (systemId != null) {
        receiver.setSystemId(localLocator.getSystemId)
      }
      receiver.open()
      receiver.startDocument(ReceiverOption.NONE)
    } catch {
      case quit: QuitParsingException => {
        getPipelineConfiguration.getErrorReporter.report(
          new XmlProcessingException(quit).asWarning())
        throw new SAXException(quit)
      }

      case err: XPathException => throw new SAXException(err)

    }
  }

  def endDocument(): Unit = {
    // System.err.println("RCH: end document");
    try {
      flush(true)
      receiver.endDocument()
      receiver.close()
    } catch {
      case err: ValidationException => {
        err.setLocator(localLocator)
        throw new SAXException(err)
      }

      case err: QuitParsingException => {}

      case err: XPathException => {
        err.maybeSetLocation(localLocator)
        throw new SAXException(err)
      }

    }
  }

  def setDocumentLocator(locator: Locator): Unit = {
    localLocator = new LocalLocator(locator)
    if (!lineNumbering) {
      lastTextNodeLocator = localLocator
    }
  }

  def startPrefixMapping(prefix: String, uri: String): Unit = {
    //System.err.println("StartPrefixMapping " + prefix + "=" + uri);
    if (prefix.==("xmlns")) {
      // should never be reported, but it's been known to happen
      return
    }
    // the binding xmlns:xmlns="http://www.w3.org/2000/xmlns/"
    // the binding xmlns:xmlns="http://www.w3.org/2000/xmlns/"
    currentNamespaceMap = currentNamespaceMap.bind(prefix, uri)
  }

  def endPrefixMapping(prefix: String): Unit = ()

  //System.err.println("endPrefixMapping " + prefix);
  //System.err.println("endPrefixMapping " + prefix);

  /**
   * Receive notification of the beginning of an element.
   *
   * <p>The Parser will invoke this method at the beginning of every
   * element in the XML document; there will be a corresponding
   * {@link #endElement endElement} event for every startElement event
   * (even when the element is empty). All of the element's content will be
   * reported, in order, before the corresponding endElement
   * event.</p>
   *
   * <p>This event allows up to three name components for each
   * element:</p>
   *
   * <ol>
   * <li>the Namespace URI;</li>
   * <li>the local name; and</li>
   * <li>the qualified (prefixed) name.</li>
   * </ol>
   *
   * <p>Saxon expects all three of these to be provided.
   *
   * <p>The attribute list provided should contain only
   * attributes with explicit values (specified or defaulted):
   * #IMPLIED attributes should be omitted.  The attribute list
   * should not contain attributes used for Namespace declarations
   * (xmlns* attributes); if it does, Saxon will ignore them,
   * which may lead to unresolved namespace prefixes.</p>
   *
   * @param uri       the Namespace URI, or the empty string if the
   *                  element has no Namespace URI or if Namespace
   *                  processing is not being performed
   * @param localname the local name (without prefix), or the
   *                  empty string if Namespace processing is not being
   *                  performed
   * @param rawname   the qualified name (with prefix), or the
   *                  empty string if qualified names are not available
   * @param atts      the attributes attached to the element.  If
   *                  there are no attributes, it shall be an empty
   *                  Attributes object.  The value of this object after
   *                  startElement returns is undefined
   * @throws org.xml.sax.SAXException any SAX exception, possibly
   *                                  wrapping another exception
   * @see #endElement
   * @see org.xml.sax.Attributes
   * @see org.xml.sax.helpers.AttributesImpl
   */
  def startElement(uri: String,
                   localname: String,
                   rawname: String,
                   atts: Attributes): Unit = {
    try {
      flush(true)
      val options: Int = ReceiverOption.NAMESPACE_OK | ReceiverOption.ALL_NAMESPACES
      val elementName: NodeName = getNodeName(uri, localname, rawname)
      val attributes: AttributeMap = makeAttributeMap(atts, localLocator)
      receiver.startElement(elementName,
        Untyped.getInstance,
        attributes,
        currentNamespaceMap,
        localLocator,
        options)
      localLocator.levelInEntity += 1
      namespaceStack.push(currentNamespaceMap)
      afterStartTag = true
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(localLocator)
        throw new SAXException(err)
      }

    }
  }

  //System.err.println("ReceivingContentHandler#startElement " + localname + " (depth=" + namespaceStack.size() + ")");
  //for (int a=0; a<atts.getLength; a++) {
  //     System.err.println("  Attribute " + atts.getURI(a) + "/" + atts.getLocalName(a) + "/" + atts.getQName(a));
  //System.err.println("ReceivingContentHandler#startElement " + localname + " (depth=" + namespaceStack.size() + ")");
  //for (int a=0; a<atts.getLength; a++) {
  //     System.err.println("  Attribute " + atts.getURI(a) + "/" + atts.getLocalName(a) + "/" + atts.getQName(a));

  private def makeAttributeMap(atts: Attributes,
                               location: Location): AttributeMap = {
    val length: Int = atts.getLength
    val list: List[AttributeInfo] =
      new ArrayList[AttributeInfo](atts.getLength)
    for (a <- 0 until length) {
      var properties: Int = ReceiverOption.NAMESPACE_OK
      val value: String = atts.getValue(a)
      val qname: String = atts.getQName(a)
      if (qname.startsWith("xmlns") && (qname.length == 5 || qname.charAt(5) == ':')) {
        // we'll cross that bridge when we come to it.
        //continue
      }
      // We normally configure the parser so that it doesn't notify namespaces as attributes.
      // But when running as a TransformerHandler, we have no control over the feature settings
      // of the sender of the events. So we filter them out, just in case. There might be cases
      // where we ought not just to ignore them, but to handle them as namespace events, but
      // We normally configure the parser so that it doesn't notify namespaces as attributes.
      // But when running as a TransformerHandler, we have no control over the feature settings
      // of the sender of the events. So we filter them out, just in case. There might be cases
      // where we ought not just to ignore them, but to handle them as namespace events, but
      if (defaultedAttributesAction != 0 && atts.isInstanceOf[Attributes2] &&
        !atts.asInstanceOf[Attributes2].isSpecified(qname)) {
        if (defaultedAttributesAction == -1) {
          // suppress defaulted attributes
          //continue
        } else {
          // mark defaulted attributes
          properties |= ReceiverOption.DEFAULTED_VALUE
        }
      }
      val attCode: NodeName =
        getNodeName(atts.getURI(a), atts.getLocalName(a), atts.getQName(a))
      val `type`: String = atts.getType(a)
      var typeCode: SimpleType = BuiltInAtomicType.UNTYPED_ATOMIC
      if (retainDTDAttributeTypes) {
        `type` match {
          case "CDATA" => // common case, no action
          case "ID" => typeCode = BuiltInAtomicType.ID
          case "IDREF" => typeCode = BuiltInAtomicType.IDREF
          case "IDREFS" => typeCode = BuiltInListType.IDREFS
          case "NMTOKEN" => typeCode = BuiltInAtomicType.NMTOKEN
          case "NMTOKENS" => typeCode = BuiltInListType.NMTOKENS
          case "ENTITY" => typeCode = BuiltInAtomicType.ENTITY
          case "ENTITIES" => typeCode = BuiltInListType.ENTITIES

        }
      } else {
        `type` match {
          case "ID" => properties |= ReceiverOption.IS_ID
          case "IDREF" => properties |= ReceiverOption.IS_IDREF
          case "IDREFS" => properties |= ReceiverOption.IS_IDREF

        }
      }
      list.add(
        new AttributeInfo(attCode, typeCode, value, location, properties))
    }
    AttributeMap.fromList(list)
  }

  private def getNodeName(uri: String,
                          localname: String,
                          rawname: String): NodeName = {
    // If none is provided, we give up
    if (rawname.isEmpty) {
      throw new SAXException(
        "Saxon requires an XML parser that reports the QName of each element")
    }
    // has been configured to report the QName rather than the localname+URI
    if (localname.isEmpty) {
      throw new SAXException(
        "Parser configuration problem: namespace reporting is not enabled")
    }
    var map2: HashMap[String, NodeName] =
      if (uri.isEmpty) noNamespaceNameCache else nameCache.get(uri)
    if (map2 == null) {
      map2 = new HashMap(50)
      nameCache.put(uri, map2)
      if (uri.isEmpty) {
        noNamespaceNameCache = map2
      }
    }
    val n: NodeName = map2.get(rawname)
    // of them, it is there for all of them.
    if (n == null) {
      if (uri.isEmpty) {
        val qn: NoNamespaceName = new NoNamespaceName(localname)
        map2.put(rawname, qn)
        qn
      } else {
        val prefix: String = NameChecker.getPrefix(rawname)
        val qn: FingerprintedQName =
          new FingerprintedQName(prefix, uri, localname)
        map2.put(rawname, qn)
        qn
      }
    } else {
      n
    }
  }

  // System.err.println("URI=" + uri + " local=" + " raw=" + rawname);
  // The XML parser isn't required to report the rawname (qname), though all known parsers do.
  // It's also possible (especially when using a TransformerHandler) that the parser
  // Following code maintains a local cache to remember all the element names that have been
  // allocated, which reduces contention on the NamePool. It also avoids parsing the lexical QName
  // when the same name is used repeatedly. We also get a tiny improvement by avoiding the first hash
  // table lookup for names in the null namespace.
  // we use the rawname (qname) rather than the local name because we want to retain the prefix
  // Note that the NodeName objects generated do not contain a namecode or fingerprint; it will be generated
  // later if we are building a TinyTree, but not necessarily on other paths (e.g. an identity transformation).
  // The NodeName object is shared by all elements with the same name, so when the namecode is allocated to one
  // System.err.println("URI=" + uri + " local=" + " raw=" + rawname);
  // The XML parser isn't required to report the rawname (qname), though all known parsers do.
  // It's also possible (especially when using a TransformerHandler) that the parser
  // Following code maintains a local cache to remember all the element names that have been
  // allocated, which reduces contention on the NamePool. It also avoids parsing the lexical QName
  // when the same name is used repeatedly. We also get a tiny improvement by avoiding the first hash
  // table lookup for names in the null namespace.
  // we use the rawname (qname) rather than the local name because we want to retain the prefix
  // Note that the NodeName objects generated do not contain a namecode or fingerprint; it will be generated
  // later if we are building a TinyTree, but not necessarily on other paths (e.g. an identity transformation).
  // The NodeName object is shared by all elements with the same name, so when the namecode is allocated to one

  def endElement(uri: String, localname: String, rawname: String): Unit = {
    try {
      flush(!afterStartTag)
      localLocator.levelInEntity -= 1
      receiver.endElement()
    } catch {
      case err: ValidationException => {
        err.maybeSetLocation(localLocator)
        if (!err.hasBeenReported) {
          pipe.getErrorReporter.report(new XmlProcessingException(err))
        }
        err.setHasBeenReported(true)
        throw new SAXException(err)
      }

      case err: XPathException => {
        err.maybeSetLocation(localLocator)
        throw new SAXException(err)
      }

    }
    afterStartTag = false
    namespaceStack.pop()
    currentNamespaceMap = namespaceStack.peek()
  }

  def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    while (charsUsed + length > buffer.length) {
      buffer = Arrays.copyOf(buffer, buffer.length * 2)
      slice = new CharSlice(buffer, 0, 0)
    }
    System.arraycopy(ch, start, buffer, charsUsed, length)
    charsUsed += length
    if (lineNumbering) {
      lastTextNodeLocator = localLocator.saveLocation()
    }
  }

  // System.err.println("characters (" + length + ")");
  // need to concatenate chunks of text before we can decide whether a node is all-white
  // System.err.println("characters (" + length + ")");
  // need to concatenate chunks of text before we can decide whether a node is all-white

  def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    if (!ignoreIgnorable) {
      characters(ch, start, length)
    }
  }

  def processingInstruction(name: String, remainder: String): Unit = {
    flush(true)
    if (!inDTD) {
      if (name == null) {
        // trick used by the old James Clark xp parser to notify a comment
        comment(remainder.toCharArray(), 0, remainder.length)
      } else {
        // some parsers allow through PI names containing colons
        if (!NameChecker.isValidNCName(name)) {
          throw new SAXException(
            "Invalid processing instruction name (" + name + ')')
        }
        if (allowDisableOutputEscaping) {
          if (name == Result.PI_DISABLE_OUTPUT_ESCAPING) {
            //flush();
            escapingDisabled = true
            return
          } else if (name == Result.PI_ENABLE_OUTPUT_ESCAPING) {
            //flush();
            escapingDisabled = false
            return
          }
        }
        var data: CharSequence = null
        data =
          if (remainder == null) ""
          else Whitespace.removeLeadingWhitespace(remainder)
        receiver.processingInstruction(name,
          data,
          localLocator,
          ReceiverOption.NONE)
      }
    }
  }

  def comment(ch: Array[Char], start: Int, length: Int): Unit = {
    flush(true)
    if (!inDTD) {
      receiver.comment(new CharSlice(ch, start, length),
        localLocator,
        ReceiverOption.NONE)
    }
  }

  private def flush(compress: Boolean): Unit = {
    if (charsUsed > 0) {
      slice.setLength(charsUsed)
      val cs: CharSequence =
        if (compress) CompressedWhitespace.compress(slice) else slice
      receiver.characters(cs,
        lastTextNodeLocator,
        if (escapingDisabled) ReceiverOption.DISABLE_ESCAPING
        else ReceiverOption.WHOLE_TEXT_NODE)
      charsUsed = 0
      escapingDisabled = false
    }
  }

  def skippedEntity(name: String): Unit = ()

  def startDTD(name: String, publicId: String, systemId: String): Unit = {
    inDTD = true
  }

  def endDTD(): Unit = {
    inDTD = false
  }

  def startEntity(name: String): Unit = {
    if (elementDepthWithinEntity == null) {
      elementDepthWithinEntity = new Stack()
    }
    elementDepthWithinEntity.push(localLocator.levelInEntity)
    localLocator.levelInEntity = 0
  }

  def endEntity(name: String): Unit = {
    localLocator.levelInEntity = elementDepthWithinEntity.pop()
  }

  def startCDATA(): Unit = ()

  def endCDATA(): Unit = ()

  def notationDecl(name: String, publicId: String, systemId: String): Unit = ()

  def unparsedEntityDecl(name: String,
                         publicId: String,
                         systemId: String,
                         notationName: String): Unit = {
    var uri: String = systemId
    if (localLocator != null) {
      try {
        val suppliedURI: URI = new URI(systemId)
        if (!suppliedURI.isAbsolute) {
          val baseURI: String = localLocator.getSystemId
          if (baseURI != null) {
            // See bug 21679
            val absoluteURI: URI = new URI(baseURI).resolve(systemId)
            uri = absoluteURI.toString
          }
        }
      } catch {
        case err: URISyntaxException => {}

      }
    }
    receiver.setUnparsedEntity(name, uri, publicId)
  }

  // Some (non-conformant) SAX parsers report the systemId as written.
  // We need to turn it into an absolute URL.
  // Some (non-conformant) SAX parsers report the systemId as written.
  // We need to turn it into an absolute URL.

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * ReceivingContentHandler is a glue class that provides a standard SAX ContentHandler
 * interface to a Saxon Receiver. To achieve this it needs to map names supplied
 * as strings to numeric name codes, for which purpose it needs access to a name
 * pool. The class also performs the function of assembling adjacent text nodes.
 * <p>If the input stream contains the processing instructions assigned by JAXP to switch
 * disable-output-escaping on or off, these will be reflected in properties set in the corresponding
 * characters events. In this case adjacent text nodes will not be combined.</p>
 * <p>The {@code ReceivingContentHandler} is written on the assumption that it is receiving events
 * from a parser configured with {@code http://xml.org/sax/features/namespaces} set to true
 * and {@code http://xml.org/sax/features/namespace-prefixes} set to false.</p>
 * <p>When running as a {@code TransformerHandler}, we have no control over the feature settings
 * of the sender of the events, and if the events do not follow this pattern then the class may
 * fail in unpredictable ways.</p>
 *
 */
