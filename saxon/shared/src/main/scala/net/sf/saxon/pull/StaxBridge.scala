package net.sf.saxon.pull

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Untyped

import net.sf.saxon.om._

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XmlProcessingIncident

import net.sf.saxon.tree.tiny.CharSlice

import net.sf.saxon.tree.tiny.CompressedWhitespace

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Whitespace

import javax.xml.namespace.QName

import javax.xml.stream._

import javax.xml.stream.events.EntityDeclaration

import java.io.InputStream

import java.lang.reflect.InvocationTargetException

import java.lang.reflect.Method

import java.net.URI

import java.net.URISyntaxException

import java.util.ArrayList

import java.util.HashMap

import java.util.List

import java.util.Stack

import scala.beans.{BeanProperty}

import net.sf.saxon.pull.PullProvider.Event._

import scala.jdk.CollectionConverters._

class StaxBridge extends PullProvider {

  private var reader: XMLStreamReader = _

  @BeanProperty
  var attributes: AttributeMap = _

  private var pipe: PipelineConfiguration = _

  private var namePool: NamePool = _

  private var nameCache: HashMap[String, NodeName] = new HashMap()

  private var namespaceStack: Stack[NamespaceMap] = new Stack()

  private var unparsedEntities: List[_] = null

  var currentEvent: Event = START_OF_INPUT

  var depth: Int = 0

  var ignoreIgnorable: Boolean = false

  namespaceStack.push(NamespaceMap.emptyMap)

  def setInputStream(systemId: String, inputStream: InputStream): Unit = {
    val factory: XMLInputFactory = XMLInputFactory.newInstance()
    factory.setXMLReporter(new StaxErrorReporter())
    reader = factory.createXMLStreamReader(systemId, inputStream)
  }

  def setXMLStreamReader(reader: XMLStreamReader): Unit = {
    this.reader = reader
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = new PipelineConfiguration(pipe)
    this.namePool = pipe.getConfiguration.getNamePool
    ignoreIgnorable = pipe.getConfiguration.getParseOptions.getSpaceStrippingRule !=
      NoElementsSpaceStrippingRule.getInstance
  }

  def getPipelineConfiguration(): PipelineConfiguration = pipe

  def getXMLStreamReader(): XMLStreamReader = reader

  def getNamePool(): NamePool = pipe.getConfiguration.getNamePool

  def next(): Event = {
    if (currentEvent == START_OF_INPUT) {
      currentEvent = START_DOCUMENT
      return currentEvent
    }
    if (currentEvent == END_OF_INPUT || currentEvent == END_DOCUMENT) {
      reader.close()
      return END_OF_INPUT
    }
    try if (reader.hasNext) {
      val event: Int = reader.next()
      currentEvent = translate(event)
      if (currentEvent == START_ELEMENT) {
        var nsMap: NamespaceMap = namespaceStack.peek()
        val n: Int = reader.getNamespaceCount
        for (i <- 0 until n) {
          val prefix: String = reader.getNamespacePrefix(i)
          val uri: String = reader.getNamespaceURI(i)
          nsMap = nsMap.bind(if (prefix == null) "" else prefix,
            if (uri == null) "" else uri)
        }
        namespaceStack.push(nsMap)
        val attCount: Int = reader.getAttributeCount
        if (attCount == 0) {
          attributes = EmptyAttributeMap.getInstance
        } else {
          val attList: List[AttributeInfo] = new ArrayList[AttributeInfo]()
          val pool: NamePool = getNamePool
          for (i <- 0 until attCount) {
            val name: QName = reader.getAttributeName(i)
            val fName: FingerprintedQName = new FingerprintedQName(
              name.getPrefix,
              name.getNamespaceURI,
              name.getLocalPart,
              pool)
            val value: String = reader.getAttributeValue(i)
            val att: AttributeInfo = new AttributeInfo(
              fName,
              BuiltInAtomicType.UNTYPED_ATOMIC,
              value,
              Loc.NONE,
              0)
            attList.add(att)
          }
          attributes = AttributeMap.fromList(attList)
        }
      } else if (currentEvent == END_ELEMENT) {
        namespaceStack.pop()
      }
    } else {
      currentEvent = END_OF_INPUT
    } catch {
      case e: XMLStreamException => {
        var message: String = e.getMessage
        if (message.startsWith("ParseError at")) {
          val c: Int = message.indexOf("\nMessage: ")
          if (c > 0) {
            message = message.substring(c + 10)
          }
        }
        val err =
          new XPathException("Error reported by XML parser: " + message, e)
        err.setErrorCode(SaxonErrorCode.SXXP0003)
        err.setLocator(translateLocation(e.getLocation))
        throw err
      }

    }
    currentEvent
  }

  private def translate(event: Int): Event = event match {
    case XMLStreamConstants.ATTRIBUTE => ATTRIBUTE
    case XMLStreamConstants.CDATA => TEXT
    case XMLStreamConstants.CHARACTERS =>
      if (depth == 0 && reader.isWhiteSpace) {
        next()
      } else {
        TEXT
      }
    case XMLStreamConstants.COMMENT => COMMENT
    case XMLStreamConstants.DTD =>
      unparsedEntities =
        reader.getProperty("javax.xml.stream.entities").asInstanceOf[List[_]]
      next()
    case XMLStreamConstants.END_DOCUMENT => END_DOCUMENT
    case XMLStreamConstants.END_ELEMENT => {
      depth -= 1
    }
      END_ELEMENT
    case XMLStreamConstants.ENTITY_DECLARATION => next()
    case XMLStreamConstants.ENTITY_REFERENCE => next()
    case XMLStreamConstants.NAMESPACE => NAMESPACE
    case XMLStreamConstants.NOTATION_DECLARATION => next()
    case XMLStreamConstants.PROCESSING_INSTRUCTION =>
      PROCESSING_INSTRUCTION
    case XMLStreamConstants.SPACE =>
      if (depth == 0) {
        next()
      } else if (ignoreIgnorable) {
        next()
      } else {
        TEXT
      }
    case XMLStreamConstants.START_DOCUMENT => next()
    case XMLStreamConstants.START_ELEMENT => {
      depth += 1;
    }
      START_ELEMENT
    case _ => throw new IllegalStateException("Unknown StAX event " + event)

  }

  def current(): Event = currentEvent

  def getNamespaceDeclarations(): Array[NamespaceBinding] = {
    val n: Int = reader.getNamespaceCount
    if (n == 0) {
      NamespaceBinding.EMPTY_ARRAY
    } else {
      val bindings: Array[NamespaceBinding] = Array.ofDim[NamespaceBinding](n)
      for (i <- 0 until n) {
        var prefix: String = reader.getNamespacePrefix(i)
        if (prefix == null) {
          prefix = ""
        }
        var uri: String = reader.getNamespaceURI(i)
        if (uri == null) {
          uri = ""
        }
        bindings(i) = new NamespaceBinding(prefix, uri)
      }
      bindings
    }
  }

  def skipToMatchingEnd(): Event = currentEvent match {
    case START_DOCUMENT =>
      currentEvent = END_DOCUMENT
      currentEvent
    case START_ELEMENT => {
      var skipDepth: Int = 0
      while (reader.hasNext) {
        val event: Int = reader.next()
        if (event == XMLStreamConstants.START_ELEMENT) {
          {
            skipDepth += 1;
            skipDepth - 1
          }
        } else if (event == XMLStreamConstants.END_ELEMENT) {
          if ( {
            skipDepth -= 1;
            skipDepth + 1
          } == 0) {
            currentEvent = END_ELEMENT
            currentEvent
          }
        }
      }
    }
      throw new IllegalStateException(
        "Element start has no matching element end")
    case _ =>
      throw new IllegalStateException(
        "Cannot call skipToMatchingEnd() except when at start of element or document")

  }

  def close(): Unit = {
    try reader.close()
    catch {
      case e: XMLStreamException => {}

    }
  }

  def getNodeName(): NodeName =
    if (currentEvent == START_ELEMENT || currentEvent == END_ELEMENT) {
      val local: String = reader.getLocalName
      val uri: String = reader.getNamespaceURI
      var cached: NodeName = nameCache.get(local)
      if (cached != null && cached.hasURI(if (uri == null) "" else uri) &&
        cached.getPrefix == reader.getPrefix) {
        cached
      } else {
        val fp: Int = namePool.allocateFingerprint(uri, local)
        cached =
          if (uri == null) new NoNamespaceName(local, fp)
          else new FingerprintedQName(reader.getPrefix, uri, local, fp)
        nameCache.put(local, cached)
        cached
      }
    } else if (currentEvent == PROCESSING_INSTRUCTION) {
      val local: String = reader.getPITarget
      new NoNamespaceName(local)
    } else {
      throw new IllegalStateException()
    }

  def getStringValue(): CharSequence = currentEvent match {
    case TEXT =>
      var cs: CharSlice = new CharSlice(reader.getTextCharacters,
        reader.getTextStart,
        reader.getTextLength)
      CompressedWhitespace.compress(cs)
    case COMMENT =>
      new CharSlice(reader.getTextCharacters,
        reader.getTextStart,
        reader.getTextLength)
    case PROCESSING_INSTRUCTION =>
      var s: String = reader.getPIData
      Whitespace.removeLeadingWhitespace(s)
    case START_ELEMENT =>
      var combinedText: FastStringBuffer = null
      var depth: Int = 0
      while (reader.hasNext) {
        val event: Int = reader.next()
        if (event == XMLStreamConstants.CHARACTERS) {
          if (combinedText == null) {
            combinedText = new FastStringBuffer(FastStringBuffer.C64)
          }
          combinedText.append(reader.getTextCharacters,
            reader.getTextStart,
            reader.getTextLength)
        } else if (event == XMLStreamConstants.START_ELEMENT) {
          {
            depth += 1
          }
        } else if (event == XMLStreamConstants.END_ELEMENT) {
          if ( {
            depth -= 1
            depth
          } == 0) {
            currentEvent = END_ELEMENT
            if (combinedText != null) {
              combinedText.condense()
            } else {
              ""
            }
          }
        }
      }
      null
    case _ =>
      throw new IllegalStateException(
        "getStringValue() called when current event is " + currentEvent)

  }

  def getAtomicValue(): AtomicValue = throw new IllegalStateException()

  def getSchemaType(): SchemaType =
    if (currentEvent == START_ELEMENT) {
      Untyped.getInstance
    } else if (currentEvent == ATTRIBUTE) {
      BuiltInAtomicType.UNTYPED_ATOMIC
    } else {
      null
    }

  def getSourceLocator(): net.sf.saxon.s9api.Location =
    translateLocation(reader.getLocation)

  private def translateLocation(location: Location): Loc =
    if (location == null) {
      Loc.NONE
    } else {
      new Loc(location.getSystemId,
        location.getLineNumber,
        location.getColumnNumber)
    }

  def getUnparsedEntities(): List[UnparsedEntity] = {
    if (unparsedEntities == null) {
      return null
    }
    val list: List[UnparsedEntity] =
      new ArrayList[UnparsedEntity](unparsedEntities.size)
    for (ent <- unparsedEntities.asScala) {
      var name: String = null
      var systemId: String = null
      var publicId: String = null
      var baseURI: String = null
      if (ent.isInstanceOf[EntityDeclaration]) {
        val ed: EntityDeclaration = ent.asInstanceOf[EntityDeclaration]
        name = ed.getName
        systemId = ed.getSystemId
        publicId = ed.getPublicId
        baseURI = ed.getBaseURI
      } else if (ent.getClass.getName.==("com.ctc.wstx.ent.UnparsedExtEntity")) {
        try {
          val woodstoxClass: Class[_] = ent.getClass
          val noArgClasses: Array[Class[_]] = Array.ofDim[Class[_]](0)
          val noArgs: Array[Any] = Array.ofDim[Any](0)
          var method: Method = woodstoxClass.getMethod("getName", noArgClasses: _*)
          name = method.invoke(ent, noArgs).asInstanceOf[String]
          method = woodstoxClass.getMethod("getSystemId", noArgClasses: _*)
          systemId = method.invoke(ent, noArgs).asInstanceOf[String]
          method = woodstoxClass.getMethod("getPublicId", noArgClasses: _*)
          publicId = method.invoke(ent, noArgs).asInstanceOf[String]
          method = woodstoxClass.getMethod("getBaseURI", noArgClasses: _*)
          baseURI = method.invoke(ent, noArgs).asInstanceOf[String]
        } catch {
          case e@(_: NoSuchMethodException | _: IllegalAccessException |
                  _: InvocationTargetException) => {}

        }
      }
      if (name != null) {
        if (baseURI != null && systemId != null) {
          try systemId = new URI(baseURI).resolve(systemId).toString
          catch {
            case err: URISyntaxException => {}

          }
        }
        val ue: UnparsedEntity = new UnparsedEntity()
        ue.setName(name)
        ue.setSystemId(systemId)
        ue.setPublicId(publicId)
        ue.setBaseURI(baseURI)
        list.add(ue)
      }
    }
    list
  }

  private class StaxErrorReporter extends XMLReporter {

    def report(message: String,
               errorType: String,
               relatedInformation: AnyRef,
               location: Location): Unit = {
      val err: XmlProcessingIncident = new XmlProcessingIncident(
        "Error reported by XML parser: " + message + " (" + errorType +
          ')')
      err.setLocation(translateLocation(location))
      pipe.getErrorReporter.report(err)
    }

  }

}
