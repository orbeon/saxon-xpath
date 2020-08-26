


package net.sf.saxon.query

import net.sf.saxon.event.ComplexContentOutputter
import net.sf.saxon.event.Receiver
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.event.SequenceReceiver
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.ObjectValue


object SequenceWrapper {
  val RESULT_NS = QueryResult.RESULT_NS
}

class SequenceWrapper(var destination: Receiver) extends SequenceReceiver(destination.getPipelineConfiguration) {
  private var out: ComplexContentOutputter = _

  out = new ComplexContentOutputter(destination)

  private var depth: Int = 0

  private var resultDocument: FingerprintedQName = _

  private var resultElement: FingerprintedQName = _

  private var resultAttribute: FingerprintedQName = _

  private var resultText: FingerprintedQName = _

  private var resultComment: FingerprintedQName = _

  private var resultPI: FingerprintedQName = _

  private var resultNamespace: FingerprintedQName = _

  private var resultAtomicValue: FingerprintedQName = _

  private var resultFunction: FingerprintedQName = _

  private var resultArray: FingerprintedQName = _

  private var resultMap: FingerprintedQName = _

  private var resultExternalValue: FingerprintedQName = _

  private var xsiType: FingerprintedQName = _

  private var namespaces: NamespaceMap = _

  def getDestination = out

  @throws[XPathException]
  private def startWrapper(name: NodeName) = {
    out.startElement(name, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.namespace("", "", ReceiverOption.NONE)
    out.startContent()
  }

  @throws[XPathException]
  private def endWrapper(): Unit = out.endElement()

  @throws[XPathException]
  override def open(): Unit = {
    val resultSequence = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "sequence")
    resultDocument = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "document")
    resultElement = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "element")
    resultAttribute = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "attribute")
    resultText = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "text")
    resultComment = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "comment")
    resultPI = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "processing-instruction")
    resultNamespace = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "namespace")
    resultAtomicValue = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "atomic-value")
    resultFunction = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "function")
    resultArray = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "array")
    resultMap = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "map")
    resultExternalValue = new FingerprintedQName("result", SequenceWrapper.RESULT_NS, "external-object")
    xsiType = new FingerprintedQName("xsi", NamespaceConstant.SCHEMA_INSTANCE, "type")
    out.open()
    out.startDocument(ReceiverOption.NONE)
    namespaces = NamespaceMap.emptyMap.put("result", SequenceWrapper.RESULT_NS).put("xs", NamespaceConstant.SCHEMA).put("xsi", NamespaceConstant.SCHEMA_INSTANCE)
    startWrapper(resultSequence)
  }


  @throws[XPathException]
  override def startDocument(properties: Int): Unit = {
    startWrapper(resultDocument)
    depth += 1
  }


  @throws[XPathException]
  override def endDocument(): Unit = {
    endWrapper()
    depth -= 1
  }


  @throws[XPathException]
  override def startElement(elemName: NodeName, `type`: SchemaType, attributes: AttributeMap, namespaces: NamespaceMap, location: Location, properties: Int): Unit = {
    if ( {
      depth += 1;
      depth - 1
    } == 0) startWrapper(resultElement)
    out.startElement(elemName, `type`, location, properties)
    out.namespace("", "", properties)

    for (att <- attributes) {
      out.attribute(att.getNodeName, att.getType, att.getValue, att.getLocation, att.getProperties)
    }
    out.startContent()
  }


  @throws[XPathException]
  override def endElement(): Unit = {
    out.endElement()
    if ( {
      depth -= 1;
      depth
    } == 0) endWrapper()
  }


  @throws[XPathException]
  override def characters(chars: CharSequence, locationId: Location, properties: Int): Unit = if (depth == 0) {
    startWrapper(resultText)
    out.characters(chars, locationId, properties)
    endWrapper()
  }
  else out.characters(chars, locationId, properties)


  @throws[XPathException]
  override def comment(chars: CharSequence, locationId: Location, properties: Int): Unit = if (depth == 0) {
    startWrapper(resultComment)
    out.comment(chars, locationId, properties)
    endWrapper()
  }
  else out.comment(chars, locationId, properties)


  @throws[XPathException]
  override def processingInstruction(target: String, data: CharSequence, locationId: Location, properties: Int) = if (depth == 0) {
    startWrapper(resultPI)
    out.processingInstruction(target, data, locationId, properties)
    endWrapper()
  }
  else out.processingInstruction(target, data, locationId, properties)


  @throws[XPathException]
  override def append(item: Item, locationId: Location, copyNamespaces: Int) = if (item.isInstanceOf[AtomicValue]) {
    val pool = getNamePool
    out.startElement(resultAtomicValue, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.namespace("", "", ReceiverOption.NONE)
    val `type` = item.asInstanceOf[AtomicValue].getItemType
    val name = `type`.getStructuredQName
    var prefix = name.getPrefix
    val localName = name.getLocalPart
    val uri = name.getURI
    if (prefix.isEmpty) {
      prefix = pool.suggestPrefixForURI(uri)
      if (prefix == null) prefix = "p" + uri.hashCode
    }
    val displayName = prefix + ':' + localName
    out.namespace("", "", ReceiverOption.NONE)
    out.namespace(prefix, uri, ReceiverOption.NONE)
    out.attribute(xsiType, BuiltInAtomicType.UNTYPED_ATOMIC, displayName, locationId, ReceiverOption.NONE)
    out.startContent()
    out.characters(item.getStringValue, locationId, ReceiverOption.NONE)
    out.endElement()
  }
  else if (item.isInstanceOf[NodeInfo]) {
    val node = item.asInstanceOf[NodeInfo]
    val kind = node.getNodeKind
    if (kind == Type.ATTRIBUTE) attribute(NameOfNode.makeName(node), node.getSchemaType.asInstanceOf[SimpleType], node.getStringValueCS, Loc.NONE, 0)
    else if (kind == Type.NAMESPACE) namespace(new NamespaceBinding(node.getLocalPart, node.getStringValue), 0)
    else item.asInstanceOf[NodeInfo].copy(this, CopyOptions.ALL_NAMESPACES | CopyOptions.TYPE_ANNOTATIONS, locationId)
  }
  else if (item.isInstanceOf[Function]) if (item.isInstanceOf[MapItem]) {
    out.startElement(resultMap, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.startContent()
    out.characters(item.toShortString, locationId, ReceiverOption.NONE)
    out.endElement()
  }
  else if (item.isInstanceOf[ArrayItem]) {
    out.startElement(resultArray, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.startContent()
    out.characters(item.toShortString, locationId, ReceiverOption.NONE)
    out.endElement()
  }
  else {
    out.startElement(resultFunction, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.startContent()
    out.characters(item.asInstanceOf[Function].getDescription, locationId, ReceiverOption.NONE)
    out.endElement()
  }
  else if (item.isInstanceOf[ObjectValue[_]]) {
    val obj = item.asInstanceOf[ObjectValue[_]].getObject
    out.startElement(resultExternalValue, Untyped.getInstance, Loc.NONE, ReceiverOption.NONE)
    out.attribute(new NoNamespaceName("class"), BuiltInAtomicType.UNTYPED_ATOMIC, obj.getClass.getName, Loc.NONE, ReceiverOption.NONE)
    out.startContent()
    out.characters(obj.toString, locationId, ReceiverOption.NONE)
    out.endElement()
  }


  @throws[XPathException]
  override def close() = {
    endWrapper()
    out.endDocument()
    out.close()
  }


  override def usesTypeAnnotations = true


  @throws[XPathException]
  private def attribute(attName: NodeName, typeCode: SimpleType, value: CharSequence, locationId: Location, properties: Int) = {
    val atts = SingletonAttributeMap.of(new AttributeInfo(attName, typeCode, value.toString, locationId, properties))
    var ns = NamespaceMap.emptyMap
    if (!attName.hasURI("")) ns = ns.put(attName.getPrefix, attName.getURI)
    out.startElement(resultAttribute, Untyped.getInstance, atts, ns, Loc.NONE, 0)
    out.startContent()
    out.endElement()
  }


  @throws[XPathException]
  private def namespace(namespaceBindings: NamespaceBindingSet, properties: Int) = {
    var ns = NamespaceMap.emptyMap
    ns = ns.addAll(namespaceBindings)
    out.startElement(resultNamespace, Untyped.getInstance, EmptyAttributeMap.getInstance, ns, Loc.NONE, 0)
    out.startContent()
    out.endElement()
  }
}