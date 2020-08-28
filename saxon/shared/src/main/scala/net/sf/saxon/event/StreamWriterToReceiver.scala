package net.sf.saxon.event

import java.util

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib.StandardURIChecker
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.Untyped
import net.sf.saxon.om._
import net.sf.saxon.pull.NamespaceContextImpl
import net.sf.saxon.serialize.charcode.UTF16CharacterSet
import net.sf.saxon.trans.Err

import scala.jdk.CollectionConverters._
import javax.xml.namespace.NamespaceContext
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamWriter
import java.util._
import java.util.function.IntPredicate

import StreamWriterToReceiver._

object StreamWriterToReceiver {

  private var DEBUG: Boolean = false

  private class Triple {

    var prefix: String = _

    var uri: String = _

    var local: String = _

    var value: String = _

  }

  private class StartTag {

    var elementName: Triple = new Triple()

    var attributes: List[Triple] = new ArrayList()

    var namespaces: List[Triple] = new ArrayList()

  }

}

class StreamWriterToReceiver(receiver: Receiver) extends XMLStreamWriter {

  private var pendingTag: StartTag = null

  val pipe: PipelineConfiguration = receiver.getPipelineConfiguration

  private var inScopeNamespaces: NamespaceReducer = new NamespaceReducer(receiver)

  var lReceiver: Receiver = inScopeNamespaces

  private var config: Configuration = pipe.getConfiguration

  private var charChecker: IntPredicate = pipe.getConfiguration.getValidCharacterChecker

  private var isChecking: Boolean = false

  private var depth: Int = -1

  private var isEmptyElement: Boolean = _


  private var setPrefixes: Stack[List[NamespaceBinding]] = new Stack()

  private var rootNamespaceContext: javax.xml.namespace.NamespaceContext =
    new NamespaceContextImpl(new NamespaceResolver() {
      override def getURIForPrefix(prefix: String,
                                   useDefault: Boolean): String = null

      override def iteratePrefixes(): Iterator[String] = {
        var e: List[String] = Collections.emptyList()
        e.iterator()
      }
    })

  this.setPrefixes.push(new ArrayList())

  def setCheckValues(check: Boolean): Unit = {
    this.isChecking = check
  }

  def isCheckValues(): Boolean = this.isChecking

  private def flushStartTag(): Unit = {
    if (depth == -1) {
      writeStartDocument()
    }
    if (pendingTag != null) {
      completeTriple(pendingTag.elementName, isAttribute = false)
      for (t <- pendingTag.attributes.asScala) {
        completeTriple(t, isAttribute = true)
      }
      var elemName: NodeName = null
      elemName =
        if (pendingTag.elementName.uri.isEmpty)
          new NoNamespaceName(pendingTag.elementName.local)
        else
          new FingerprintedQName(pendingTag.elementName.prefix,
            pendingTag.elementName.uri,
            pendingTag.elementName.local)
      var nsMap: NamespaceMap = NamespaceMap.emptyMap
      if (!pendingTag.elementName.uri.isEmpty) {
        nsMap =
          nsMap.put(pendingTag.elementName.prefix, pendingTag.elementName.uri)
      }
      for (t <- pendingTag.namespaces.asScala) {
        if (t.prefix == null) {
          t.prefix = ""
        }
        if (t.uri == null) {
          t.uri = ""
        }
        if (!t.uri.isEmpty) {
          nsMap = nsMap.put(t.prefix, t.uri)
        }
      }
      var attributes: AttributeMap = EmptyAttributeMap.getInstance
      for (t <- pendingTag.attributes.asScala) {
        var attName: NodeName = null
        if (t.uri.isEmpty) {
          attName = new NoNamespaceName(t.local)
        } else {
          attName = new FingerprintedQName(t.prefix, t.uri, t.local)
          nsMap = nsMap.put(t.prefix, t.uri)
        }
        attributes = attributes.put(
          new AttributeInfo(attName,
            BuiltInAtomicType.UNTYPED_ATOMIC,
            t.value,
            Loc.NONE,
            ReceiverOption.NONE))
      }
      lReceiver.startElement(elemName,
        Untyped.getInstance,
        attributes,
        nsMap,
        Loc.NONE,
        ReceiverOption.NONE)
      pendingTag = null
      if (isEmptyElement) {
        isEmptyElement = false
        depth -= 1
        setPrefixes.pop()
        lReceiver.endElement()
      }
    }
  }

  private def completeTriple(t: Triple, isAttribute: Boolean): Unit = {
    if (t.local == null) {
      throw new XMLStreamException(
        "Local name of " + (if (isAttribute) "Attribute" else "Element") +
          " is missing")
    }
    if (isChecking && !isValidNCName(t.local)) {
      throw new XMLStreamException(
        "Local name of " + (if (isAttribute) "Attribute" else "Element") +
          Err.wrap(t.local) +
          " is invalid")
    }
    if (t.prefix == null) {
      t.prefix = ""
    }
    if (t.uri == null) {
      t.uri = ""
    }
    if (isChecking && !t.uri.isEmpty && isInvalidURI(t.uri)) {
      throw new XMLStreamException(
        "Namespace URI " + Err.wrap(t.local) + " is invalid")
    }
    if (t.prefix.isEmpty && !t.uri.isEmpty) {
      t.prefix = getPrefixForUri(t.uri)
    }
  }

  private def getDefaultNamespace(): String =
    pendingTag.namespaces.asScala
      .find(t => t.prefix == null || t.prefix.isEmpty)
      .map(_.uri)
      .getOrElse(inScopeNamespaces.getURIForPrefix("", useDefault = true))

  private def getUriForPrefix(prefix: String): String =
    pendingTag.namespaces.asScala
      .find(prefix == _.prefix)
      .map(_.uri)
      .getOrElse(inScopeNamespaces.getURIForPrefix(prefix, useDefault = false))

  private def getPrefixForUri(uri: String): String = {
    for (t <- pendingTag.namespaces.asScala if uri == t.uri) {
      if (t.prefix == null) "" else t.prefix
    }
    val setPrefix: String = getPrefix(uri)
    if (setPrefix != null) {
      return setPrefix
    }
    val prefixes: Iterator[String] = inScopeNamespaces.iteratePrefixes()
    while (prefixes.hasNext) {
      val p: String = prefixes.next()
      if (inScopeNamespaces.getURIForPrefix(p, useDefault = false) == uri) {
        p
      }
    }
    ""
  }

  def writeStartElement(localName: String): Unit = {
    if (DEBUG) {
      System.err.println("StartElement " + localName)
    }
    checkNonNull(localName)
    setPrefixes.push(new ArrayList())
    flushStartTag()
    depth += 1
    pendingTag = new StartTag()
    pendingTag.elementName.local = localName
  }

  def writeStartElement(namespaceURI: String, localName: String): Unit = {
    if (DEBUG) {
      System.err.println("StartElement Q{" + namespaceURI + "}" + localName)
    }
    checkNonNull(namespaceURI)
    checkNonNull(localName)
    setPrefixes.push(new ArrayList())
    flushStartTag()
    depth += 1
    pendingTag = new StartTag()
    pendingTag.elementName.local = localName
    pendingTag.elementName.uri = namespaceURI
  }

  def writeStartElement(prefix: String,
                        localName: String,
                        namespaceURI: String): Unit = {
    if (DEBUG) {
      System.err.println(
        "StartElement " + prefix + "=Q{" + namespaceURI + "}" +
          localName)
    }
    checkNonNull(prefix)
    checkNonNull(localName)
    checkNonNull(namespaceURI)
    setPrefixes.push(new ArrayList())
    flushStartTag()
    depth += 1
    pendingTag = new StartTag()
    pendingTag.elementName.local = localName
    pendingTag.elementName.uri = namespaceURI
    pendingTag.elementName.prefix = prefix
  }

  def writeEmptyElement(namespaceURI: String, localName: String): Unit = {
    checkNonNull(namespaceURI)
    checkNonNull(localName)
    flushStartTag()
    writeStartElement(namespaceURI, localName)
    isEmptyElement = true
  }

  def writeEmptyElement(prefix: String,
                        localName: String,
                        namespaceURI: String): Unit = {
    checkNonNull(prefix)
    checkNonNull(localName)
    checkNonNull(namespaceURI)
    flushStartTag()
    writeStartElement(prefix, localName, namespaceURI)
    isEmptyElement = true
  }

  def writeEmptyElement(localName: String): Unit = {
    checkNonNull(localName)
    flushStartTag()
    writeStartElement(localName)
    isEmptyElement = true
  }

  def writeEndElement(): Unit = {
    if (DEBUG) {
      System.err.println("EndElement" + depth)
    }
    if (depth <= 0) {
      throw new IllegalStateException(
        "writeEndElement with no matching writeStartElement")
    }
    flushStartTag()
    setPrefixes.pop()
    lReceiver.endElement()
    depth -= 1
  }

  def writeEndDocument(): Unit = {
    if (depth == -1) {
      throw new IllegalStateException(
        "writeEndDocument with no matching writeStartDocument")
    }
    flushStartTag()
    while (depth > 0) writeEndElement()
    lReceiver.endDocument()
    depth = -1
  }

  def close(): Unit = {
    if (depth >= 0) {
      writeEndDocument()
    }
    lReceiver.close()
  }

  def flush(): Unit = ()

  def writeAttribute(localName: String, value: String): Unit = {
    checkNonNull(localName)
    checkNonNull(value)
    if (pendingTag == null) {
      throw new IllegalStateException(
        "Cannot write attribute when not in a start tag")
    }
    val t: Triple = new Triple()
    t.local = localName
    t.value = value
    pendingTag.attributes.add(t)
  }

  def writeAttribute(prefix: String,
                     namespaceURI: String,
                     localName: String,
                     value: String): Unit = {
    checkNonNull(prefix)
    checkNonNull(namespaceURI)
    checkNonNull(localName)
    checkNonNull(value)
    if (pendingTag == null) {
      throw new IllegalStateException(
        "Cannot write attribute when not in a start tag")
    }
    val t: Triple = new Triple()
    t.prefix = prefix
    t.uri = namespaceURI
    t.local = localName
    t.value = value
    pendingTag.attributes.add(t)
  }

  def writeAttribute(namespaceURI: String,
                     localName: String,
                     value: String): Unit = {
    checkNonNull(namespaceURI)
    checkNonNull(localName)
    checkNonNull(value)
    val t: Triple = new Triple()
    t.uri = namespaceURI
    t.local = localName
    t.value = value
    pendingTag.attributes.add(t)
  }

  def writeNamespace(prefix: String, namespaceURI: String): Unit = {
    if (prefix == null || prefix.==("") || prefix.==("xmlns")) {
      writeDefaultNamespace(namespaceURI)
    } else {
      checkNonNull(namespaceURI)
      if (pendingTag == null) {
        throw new IllegalStateException(
          "Cannot write namespace when not in a start tag")
      }
      val t: Triple = new Triple()
      t.uri = namespaceURI
      t.prefix = prefix
      pendingTag.namespaces.add(t)
    }
  }

  def writeDefaultNamespace(namespaceURI: String): Unit = {
    checkNonNull(namespaceURI)
    if (pendingTag == null) {
      throw new IllegalStateException(
        "Cannot write namespace when not in a start tag")
    }
    val t: Triple = new Triple()
    t.uri = namespaceURI
    pendingTag.namespaces.add(t)
  }

  def writeComment(data: String): Unit = {
    var strData = data
    flushStartTag()
    if (strData == null) {
      strData = ""
    }
    if (!isValidChars(strData)) {
      throw new IllegalArgumentException(
        "Invalid XML character in comment: " + strData)
    }
    if (isChecking && strData.contains("--")) {
      throw new IllegalArgumentException("Comment contains '--'")
    }
    lReceiver.comment(strData, Loc.NONE, ReceiverOption.NONE)
  }

  def writeProcessingInstruction(target: String): Unit = {
    writeProcessingInstruction(target, "")
  }

  def writeProcessingInstruction(target: String, data: String): Unit = {
    checkNonNull(target)
    checkNonNull(data)
    flushStartTag()
    if (isChecking) {
      if (!isValidNCName(target) || "xml".equalsIgnoreCase(target)) {
        throw new IllegalArgumentException("Invalid PITarget: " + target)
      }
      if (!isValidChars(data)) {
        throw new IllegalArgumentException(
          "Invalid character in PI data: " + data)
      }
    }
    lReceiver.processingInstruction(target, data, Loc.NONE, ReceiverOption.NONE)
  }

  def writeCData(data: String): Unit = {
    checkNonNull(data)
    flushStartTag()
    writeCharacters(data)
  }

  def writeDTD(dtd: String): Unit = ()

  def writeEntityRef(name: String): Unit = {
    throw new UnsupportedOperationException("writeEntityRef")
  }

  def writeStartDocument(): Unit = {
    writeStartDocument("utf-8", "1.0")
  }

  def writeStartDocument(version: String): Unit = {
    writeStartDocument("utf-8", version)
  }

  def writeStartDocument(encoding: String, version: String): Unit = {
    var encode = encoding
    var vers = version
    if (encode == null) {
      encode = "utf-8"
    }
    if (vers == null) {
      vers = "1.0"
    }
    if (depth != -1) {
      throw new IllegalStateException(
        "writeStartDocument must be the first call")
    }
    lReceiver.open()
    lReceiver.startDocument(ReceiverOption.NONE)
    depth = 0
  }

  def writeCharacters(text: String): Unit = {
    checkNonNull(text)
    flushStartTag()
    if (!isValidChars(text)) {
      throw new IllegalArgumentException("illegal XML character: " + text)
    }
    lReceiver.characters(text, Loc.NONE, ReceiverOption.NONE)
  }

  def writeCharacters(text: Array[Char], start: Int, len: Int): Unit = {
    checkNonNull(text)
    writeCharacters(new String(text, start, len))
  }

  def getPrefix(uri: String): String = {
    var i: Int = setPrefixes.size - 1
    while (i >= 0) {
      val bindings: List[NamespaceBinding] = setPrefixes.get(i)
      var j: Int = bindings.size - 1
      while (j >= 0) {
        val binding: NamespaceBinding = bindings.get(j)
        if (binding.getURI == uri) {
          binding.getPrefix
        }
        {
          j -= 1; j + 1
        }
      }
      {
        i -= 1; i + 1
      }
    }
    if (rootNamespaceContext != null) {
      rootNamespaceContext.getPrefix(uri)
    }
    null
  }

  def setPrefix(prefix: String, uri: String): Unit = {
    checkNonNull(prefix)
    var URI = uri
    if (URI == null) {
      URI = ""
    }
    if (isInvalidURI(URI)) {
      throw new IllegalArgumentException("Invalid namespace URI: " + URI)
    }
    if ("" != prefix && !isValidNCName(prefix)) {
      throw new IllegalArgumentException("Invalid namespace prefix: " + prefix)
    }
    setPrefixes.peek().add(new NamespaceBinding(prefix, URI))
  }

  def setDefaultNamespace(uri: String): Unit = {
    setPrefix("", uri)
  }

  def setNamespaceContext(
                           context: javax.xml.namespace.NamespaceContext): Unit = {
    if (depth > 0) {
      throw new IllegalStateException(
        "setNamespaceContext may only be called at the start of the document")
    }
    rootNamespaceContext = context
  }

  def getNamespaceContext(): javax.xml.namespace.NamespaceContext =
    new NamespaceContext() {
      val rootNamespaceContext: NamespaceContext =
        StreamWriterToReceiver.this.rootNamespaceContext

      val bindings: Map[String, String] = new HashMap()

      for (list <- setPrefixes.asScala; binding <- list.asScala) {
        bindings.put(binding.getPrefix, binding.getURI)
      }

      def getNamespaceURI(prefix: String): String = {
        val uri: String = bindings.get(prefix)
        if (uri != null) {
          return uri
        }
        rootNamespaceContext.getNamespaceURI(prefix)
      }

      def getPrefix(namespaceURI: String): String = {
        for ((key, value) <- bindings.asScala) {
          if (value == namespaceURI) key
        }
        rootNamespaceContext.getPrefix(namespaceURI)
      }

      def getPrefixes(namespaceURI: String): Iterator[String] = {
        val prefixes: List[String] = new ArrayList[String]()
        for ((key, value) <- bindings.asScala if value == namespaceURI) {
          prefixes.add(key)
        }
        val root = rootNamespaceContext.getPrefixes(namespaceURI).asInstanceOf[util.Iterator[String]]
        while (root.hasNext)
          prefixes.add(root.next())
        prefixes.iterator()
      }
    }

  def getProperty(name: String): Any =
    if (name.==("javax.xml.stream.isRepairingNamespaces")) lReceiver.isInstanceOf[NamespaceReducer]
    else throw new IllegalArgumentException(name)

  private def isValidNCName(name: String): Boolean =
    !isChecking || NameChecker.isValidNCName(name)

  private def isValidChars(text: String): Boolean =
    !isChecking ||
      (UTF16CharacterSet.firstInvalidChar(text, charChecker) ==
        -1)

  private def isInvalidURI(uri: String): Boolean =
    isChecking && !StandardURIChecker.getInstance.isValidURI(uri)

  private def checkNonNull(value: AnyRef): Unit = {
    if (value == null) {
      throw new NullPointerException()
    }
  }

}
