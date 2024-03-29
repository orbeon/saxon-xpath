package org.orbeon.saxon.event

import org.orbeon.saxon.event.RegularSequenceChecker.State._
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.{NamespaceConstant, ParseOptions, Validation}
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, SimpleType, Type}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.{HostLanguage, Location}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.{CharSequenceConsumer, Orphan}
import org.orbeon.saxon.value.{AtomicValue, ExternalObject}

import java.{util => ju}
import javax.xml.transform.Result


/**
 * This class is used for generating complex content, that is, the content of an
 * element or document node. It enforces the rules on the order of events within
 * complex content (attributes and namespaces must come first), and it implements
 * part of the namespace fixup rules, in particular, it ensures that there is a
 * namespace node for the namespace used in the element name and in each attribute
 * name.
 *
 * The same ComplexContentOutputter may be used for generating an entire XML
 * document; it is not necessary to create a new outputter for each element node.
 *
 * From Saxon 9.9, the ComplexContentOutputter does not combine top-level events.
 * Unless nested within a startDocument/endDocument or startElement/endElement pair,
 * items such as atomic values, text nodes, attribute nodes, maps and arrays are
 * passed through unchanged to the output. It is typically the responsibility
 * of the Destination object to decide how to combine top-level events (whether
 * to build a single document, whether to insert item separators, etc).
 *
 * From Saxon 10.0, the handling of namespaces changes. Unlike other receivers,
 * the `ComplexContentOutputter` can receive individual namespace events
 * as part of the complex content of an element node. The class is now fully
 * responsible for namespace fixup and for namespace inheritance. The mechanism
 * for namespace inheritance is changed; we are now maintaining all the in-scope
 * namespaces for an element rather than a set of deltas, so namespace inheritance
 * (rather than disinheritance) now requires concrete action.
 */
object ComplexContentOutputter {

  def makeComplexContentReceiver(receiver: Receiver,
                                 options: ParseOptions): ComplexContentOutputter = {
    var rec = receiver
    val systemId = receiver.getSystemId
    val validate = options != null &&
      options.getSchemaValidationMode != Validation.PRESERVE
    if (validate) {
      val config = rec.getPipelineConfiguration.getConfiguration
      rec = config.getDocumentValidator(rec, systemId, options, null)
    }
    val result = new ComplexContentOutputter(rec)
    result.setSystemId(systemId)
    result
  }
}

class ComplexContentOutputter(next: Receiver) extends Outputter with Receiver with Result {

  private var nextReceiver: Receiver = _
  private var pendingStartTag: NodeName = null
  private var level: Int = -1
  private var currentLevelIsDocument: Array[Boolean] = new Array[Boolean](20)
  private val pendingAttributes: ju.List[AttributeInfo] = new ju.ArrayList()
  private var pendingNSMap: NamespaceMap = _
  private var inheritedNamespaces: List[NamespaceMap] = Nil
  private var currentSimpleType: SchemaType = null
  private var startElementProperties: Int = _
  private var startElementLocationId: Location = Loc.NONE
  private var state: RegularSequenceChecker.State.State = Initial
  private var previousAtomic: Boolean = false
  val pipe: PipelineConfiguration = next.getPipelineConfiguration
  private var hostLanguage: HostLanguage.HostLanguage = pipe.getHostLanguage
  this.pipelineConfiguration = pipe

  this.setReceiver(next)

  ju.Objects.requireNonNull(pipe)

  inheritedNamespaces ::= NamespaceMap.emptyMap

  override def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    if (pipelineConfiguration != pipe) {
      pipelineConfiguration = pipe
      if (nextReceiver != null) {
        nextReceiver.setPipelineConfiguration(pipe)
      }
    }
  }

  override def setSystemId(systemId: String): Unit = {
    super.setSystemId(systemId)
    nextReceiver.setSystemId(systemId)
  }

  def setHostLanguage(language: HostLanguage.HostLanguage): Unit =
    hostLanguage = language

  def setReceiver(receiver: Receiver): Unit =
    this.nextReceiver = receiver

  def getReceiver: Receiver = nextReceiver

  override def open(): Unit = {
    nextReceiver.open()
    previousAtomic = false
    state = Open
  }

  def startDocument(properties: Int): Unit = {
    level += 1
    if (level == 0)
      nextReceiver.startDocument(properties)
    else if (state == StartTag)
      startContent()
    previousAtomic = false
    if (currentLevelIsDocument.length < level + 1)
      currentLevelIsDocument = ju.Arrays.copyOf(currentLevelIsDocument, level * 2)
    currentLevelIsDocument(level) = true
    state = Content
  }

  def endDocument(): Unit = {
    if (level == 0)
      nextReceiver.endDocument()
    previousAtomic = false
    level -= 1
    state = if (level < 0) Open else Content
  }

  override def setUnparsedEntity(
    name     : String,
    systemID : String,
    publicID : String
  ): Unit =
    nextReceiver.setUnparsedEntity(name, systemID, publicID)

  def characters(
    s          : CharSequence,
    locationId : Location,
    properties : Int
  ): Unit = {
    if (level >= 0) {
      previousAtomic = false
      if (s == null)
        return
      val len = s.length
      if (len == 0)
        return
      if (state == StartTag)
        startContent()
    }
    nextReceiver.characters(s, locationId, properties)
  }

  def startElement(
    elemName   : NodeName,
    typeCode   : SchemaType,
    location   : Location,
    properties : Int
  ): Unit = {
    level += 1
    if (state == StartTag)
      startContent()
    startElementProperties = properties
    startElementLocationId = location.saveLocation
    pendingAttributes.clear()
    pendingNSMap = NamespaceMap.emptyMap
    pendingStartTag = elemName
    currentSimpleType = typeCode
    previousAtomic = false
    if (currentLevelIsDocument.length < level + 1) {
      currentLevelIsDocument = ju.Arrays.copyOf(currentLevelIsDocument, level * 2)
    }
    currentLevelIsDocument(level) = false
    state = StartTag
  }

  def namespace(prefix: String, namespaceUri: String, properties: Int): Unit = {
    ju.Objects.requireNonNull(prefix)
    ju.Objects.requireNonNull(namespaceUri)
    if (ReceiverOption.contains(properties, ReceiverOption.NAMESPACE_OK)) {
      pendingNSMap = pendingNSMap.put(prefix, namespaceUri)
    } else if (level >= 0) {
      if (state != StartTag) {
        throw NoOpenStartTagException.makeNoOpenStartTagException(
          Type.NAMESPACE,
          prefix,
          hostLanguage,
          currentLevelIsDocument(level),
          startElementLocationId)
      }
      val elementIsInNullNamespace: Boolean = pendingStartTag.hasURI("")
      if (prefix.isEmpty && namespaceUri.nonEmpty) {
        if (elementIsInNullNamespace) {
          val err = new XPathException(
            "Cannot output a namespace node for the default namespace (" +
              namespaceUri +
              ") when the element is in no namespace")
          err.setErrorCode(
            if (hostLanguage == HostLanguage.XSLT) "XTDE0440" else "XQDY0102")
          throw err
        }
      }
      val rejectDuplicates: Boolean =
        ReceiverOption.contains(properties, ReceiverOption.REJECT_DUPLICATES)
      if (rejectDuplicates) {
        val uri: String = pendingNSMap.getURI(prefix)
        if (uri != null && uri != namespaceUri) {
          val err = new XPathException(
            "Cannot create two namespace nodes with the same prefix " +
              "mapped to different URIs (prefix=\"" +
              prefix +
              "\", URIs=(" +
              uri +
              "\", \"" +
              namespaceUri +
              "\")")
          err.setErrorCode(
            if (hostLanguage == HostLanguage.XSLT) "XTDE0430" else "XQDY0102")
          throw err
        }
      }
      pendingNSMap = pendingNSMap.put(prefix, namespaceUri)
    } else {
      val orphan: Orphan = new Orphan(getConfiguration)
      orphan.setNodeKind(Type.NAMESPACE)
      orphan.setNodeName(new NoNamespaceName(prefix))
      orphan.setStringValue(namespaceUri)
      nextReceiver.append(orphan, Loc.NONE, properties)
    }
    previousAtomic = false
  }

  override def namespaces(bindings: NamespaceBindingSet, properties: Int): Unit =
    bindings match {
      case map: NamespaceMap if ReceiverOption.contains(properties, ReceiverOption.NAMESPACE_OK) && pendingNSMap.isEmpty =>
        pendingNSMap = map
      case _ =>
        super.namespaces(bindings, properties)
    }

  def attribute(
    attName    : NodeName,
    typeCode   : SimpleType,
    value      : CharSequence,
    locationId : Location,
    properties : Int
  ): Unit = {
    if (level >= 0 && state != StartTag) {
      val err =
        NoOpenStartTagException.makeNoOpenStartTagException(
          Type.ATTRIBUTE,
          attName.getDisplayName,
          hostLanguage,
          currentLevelIsDocument(level),
          startElementLocationId)
      err.setLocator(locationId)
      throw err
    }
    val attInfo = new AttributeInfo(attName,
      typeCode,
      value.toString,
      locationId,
      properties
    )
    if (level >= 0 && ! ReceiverOption.contains(properties, ReceiverOption.NOT_A_DUPLICATE)) {
      for (a <- 0 until pendingAttributes.size
           if pendingAttributes.get(a).getNodeName == attName) {
        if (hostLanguage == HostLanguage.XSLT) {
          pendingAttributes.set(a, attInfo)
          return
        } else {
          val err = new XPathException(
            "Cannot create an element having two attributes with the same name: " +
              Err.wrap(attName.getDisplayName, Err.ATTRIBUTE))
          err.setErrorCode("XQDY0025")
          throw err
        }
      }
    }
    if (level == 0 && typeCode != BuiltInAtomicType.UNTYPED_ATOMIC &&
      currentLevelIsDocument(0)) {
      if (typeCode.isNamespaceSensitive) {
        val err = new XPathException(
          "Cannot copy attributes whose type is namespace-sensitive (QName or NOTATION): " +
            Err.wrap(attName.getDisplayName, Err.ATTRIBUTE))
        err.setErrorCode(
          if (hostLanguage == HostLanguage.XSLT) "XTTE0950" else "XQTY0086")
        throw err
      }
    }
    if (level < 0) {
      val orphan = new Orphan(getConfiguration)
      orphan.setNodeKind(Type.ATTRIBUTE)
      orphan.setNodeName(attName)
      orphan.setTypeAnnotation(typeCode)
      orphan.setStringValue(value)
      nextReceiver.append(orphan, locationId, properties)
    }
    pendingAttributes.add(attInfo)
    previousAtomic = false
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    var nameSpacMap = namespaces
    if (state == StartTag)
      startContent()
    level += 1
    startElementLocationId = location.saveLocation
    if (currentLevelIsDocument.length < level + 1)
      currentLevelIsDocument = ju.Arrays.copyOf(currentLevelIsDocument, level * 2)
    currentLevelIsDocument(level) = false
    if (elemName.hasURI("") && namespaces.getDefaultNamespace.nonEmpty)
      nameSpacMap = nameSpacMap.remove("")
    val inherit = ! ReceiverOption.contains(
      properties,
      ReceiverOption.DISINHERIT_NAMESPACES)
    var ns2: NamespaceMap = null
    if (inherit) {
      val inherited = inheritedNamespaces.head
      ns2 = inherited.putAll(nameSpacMap)
      if (inherited.getDefaultNamespace.nonEmpty && elemName.getURI.isEmpty) {
        ns2 = ns2.remove("")
      }
      if (ReceiverOption.contains(
        properties,
        ReceiverOption.BEQUEATH_INHERITED_NAMESPACES_ONLY)) {
        inheritedNamespaces ::= inherited
      } else {
        inheritedNamespaces ::= ns2
      }
    } else {
      ns2 = nameSpacMap
      inheritedNamespaces ::= NamespaceMap.emptyMap
    }
    val refuseInheritedNamespaces: Boolean =
      ReceiverOption.contains(properties, ReceiverOption.REFUSE_NAMESPACES)
    val ns3: NamespaceMap = if (refuseInheritedNamespaces) nameSpacMap else ns2
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      ns3,
      location,
      properties)
    state = Content
  }

  private def checkProposedPrefix(nodeName: NodeName, seq: Int): NodeName = {
    val nodePrefix: String = nodeName.getPrefix
    val nodeURI: String = nodeName.getURI
    if (nodeURI.isEmpty) {
      nodeName
    } else {
      val uri = pendingNSMap.getURI(nodePrefix)
      if (uri == null) {
        pendingNSMap = pendingNSMap.put(nodePrefix, nodeURI)
        nodeName
      } else if (nodeURI == uri) {
        nodeName
      } else {
        val newPrefix         = getSubstitutePrefix(nodePrefix, nodeURI, seq)
        val newName: NodeName =
          new FingerprintedQName(newPrefix, nodeURI, nodeName.getLocalPart)
        pendingNSMap = pendingNSMap.put(newPrefix, nodeURI)
        newName
      }
    }
  }

  private def getSubstitutePrefix(prefix: String,
                                  uri: String,
                                  seq: Int): String = {
    if (uri == NamespaceConstant.XML)
      "xml"
    else
      prefix + '_' + seq
  }

  def endElement(): Unit = {
    if (state == StartTag)
      startContent()
    else
      pendingStartTag = null
    nextReceiver.endElement()
    level -= 1
    previousAtomic = false
    state = if (level < 0) Open else Content
    inheritedNamespaces = inheritedNamespaces.tail
  }

  def comment(comment: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    if (level >= 0) {
      if (state == StartTag)
        startContent()
      previousAtomic = false
    }
    nextReceiver.comment(comment, locationId, properties)
  }

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    if (level >= 0) {
      if (state == StartTag)
        startContent()
      previousAtomic = false
    }
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit =
    if (level >= 0)
      decompose(item, locationId, copyNamespaces)
    else
      nextReceiver.append(item, locationId, copyNamespaces)

  override def getStringReceiver(asTextNode: Boolean): CharSequenceConsumer =
    if (level >= 0) {
      new CharSequenceConsumer() {
        override def open(): Unit = {
          if (previousAtomic && !asTextNode) {
            ComplexContentOutputter.this.characters(" ",
              Loc.NONE,
              ReceiverOption.NONE)
          }
        }

        override def cat(chars: CharSequence): CharSequenceConsumer = {
          ComplexContentOutputter.this.characters(chars,
            Loc.NONE,
            ReceiverOption.NONE)
          this
        }

        override def close(): Unit = {
          previousAtomic = !asTextNode
        }
      }
    } else {
      super.getStringReceiver(asTextNode)
    }

  override def close(): Unit = {
    nextReceiver.close()
    previousAtomic = false
    state = Final
  }

  override def startContent(): Unit = {
    if (state != StartTag)
      return
    val elcode = checkProposedPrefix(pendingStartTag, 0)
    val props  = startElementProperties | ReceiverOption.NAMESPACE_OK
    for (a <- 0 until pendingAttributes.size) {
      val oldName = pendingAttributes.get(a).getNodeName
      if (! oldName.hasURI("")) {
        val newName = checkProposedPrefix(oldName, a + 1)
        if (newName != oldName) {
          val newInfo = pendingAttributes.get(a).withNodeName(newName)
          pendingAttributes.set(a, newInfo)
        }
      }
    }
    val inherited =
      if (inheritedNamespaces.isEmpty)
        NamespaceMap.emptyMap
      else
        inheritedNamespaces.head
    if (!ReceiverOption.contains(startElementProperties,
      ReceiverOption.REFUSE_NAMESPACES)) {
      pendingNSMap = inherited.putAll(pendingNSMap)
    }
    if (pendingStartTag.hasURI("") && pendingNSMap.getDefaultNamespace.nonEmpty)
      pendingNSMap = pendingNSMap.remove("")
    val attributes = AttributeMap.fromList(pendingAttributes)
    nextReceiver.startElement(elcode,
      currentSimpleType,
      attributes,
      pendingNSMap,
      startElementLocationId,
      props)
    val inherit = !ReceiverOption.contains(
      startElementProperties,
      ReceiverOption.DISINHERIT_NAMESPACES)
    inheritedNamespaces ::= (if (inherit) pendingNSMap else inherited)
    pendingAttributes.clear()
    pendingNSMap = NamespaceMap.emptyMap
    previousAtomic = false
    state = Content
  }

  override def usesTypeAnnotations: Boolean = nextReceiver.usesTypeAnnotations

  def flatten(array: ArrayItem,
              locationId: Location,
              copyNamespaces: Int): Unit =
    for (member <- array.members)
      member
        .iterate()
        .forEachOrFail(it => append(it, locationId, copyNamespaces))

  def decompose(item: Item,
                locationId: Location,
                copyNamespaces: Int): Unit = {
    if (item != null) {
      if (item.isInstanceOf[AtomicValue] || item.isInstanceOf[ExternalObject[_]]) {
        if (previousAtomic)
          characters(" ", locationId, ReceiverOption.NONE)
        characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
        previousAtomic = true
      } else item match {
        case arrayItem: ArrayItem =>
          flatten(arrayItem, locationId, copyNamespaces)
        case _: Function =>
          val thing     = if (item.isInstanceOf[MapItem]) "map" else "function item"
          val errorCode = getErrorCodeForDecomposingFunctionItems
          if (errorCode.startsWith("SENR")) {
            throw new XPathException(
              "Cannot serialize a " + thing + " using this output method",
              errorCode,
              locationId)
          } else {
            throw new XPathException(
              "Cannot add a " + thing + " to an XDM node tree",
              errorCode,
              locationId)
          }
        case _ =>
          val node = item.asInstanceOf[NodeInfo]
          node.getNodeKind match {
            case Type.TEXT =>
              var options: Int = ReceiverOption.NONE
              node match {
                case orphan: Orphan if orphan.isDisableOutputEscaping =>
                  options = ReceiverOption.DISABLE_ESCAPING
                case _ =>
              }
              characters(item.getStringValueCS, locationId, options)
            case Type.ATTRIBUTE =>
              if (node.getSchemaType
                .asInstanceOf[SimpleType]
                .isNamespaceSensitive) {
                val err = new XPathException(
                  "Cannot copy attributes whose type is namespace-sensitive (QName or NOTATION): " +
                    Err.wrap(node.getDisplayName, Err.ATTRIBUTE))
                err.setErrorCode(
                  if (getPipelineConfiguration.isXSLT) "XTTE0950"
                  else "XQTY0086")
                throw err
              }
              attribute(NameOfNode.makeName(node),
                node.getSchemaType.asInstanceOf[SimpleType],
                node.getStringValue,
                locationId,
                ReceiverOption.NONE)
            case Type.NAMESPACE =>
              namespace(node.getLocalPart,
                node.getStringValue,
                ReceiverOption.NONE)
            case Type.DOCUMENT =>
              startDocument(ReceiverOption.NONE)
              for (child <- node.children) {
                append(child, locationId, copyNamespaces)
              }
              endDocument()
            case _ =>
              var copyOptions = CopyOptions.TYPE_ANNOTATIONS
              if (ReceiverOption.contains(copyNamespaces, ReceiverOption.ALL_NAMESPACES)) {
                copyOptions |= CopyOptions.ALL_NAMESPACES
              }
              item.asInstanceOf[NodeInfo].copy(this, copyOptions, locationId)

          }
          previousAtomic = false
      }
    }
  }

  def getErrorCodeForDecomposingFunctionItems: String =
    if (getPipelineConfiguration.isXSLT) "XTDE0450" else "XQTY0105"
}
