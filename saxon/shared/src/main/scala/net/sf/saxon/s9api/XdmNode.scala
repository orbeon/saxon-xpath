package net.sf.saxon.s9api

import net.sf.saxon.model.Type
import net.sf.saxon.om.AtomicSequence
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.pattern.NameTest
import net.sf.saxon.query.QueryResult
import net.sf.saxon.s9api.streams.Steps
import net.sf.saxon.s9api.streams.XdmStream
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.wrapper.VirtualNode
import javax.xml.transform.Source
import java.net.URI
import java.util.function.Predicate
import java.util.stream.Stream

import net.sf.saxon.s9api.Axis.Axis
import net.sf.saxon.utils.Configuration
import XdmNodeKind._

class XdmNode extends XdmItem {

  def this(node: NodeInfo) = {
    this()
    this.setValue(node)
  }

  def getNodeKind: XdmNodeKind = getUnderlyingNode.getNodeKind match {
    case Type.DOCUMENT => XdmNodeKind.DOCUMENT
    case Type.ELEMENT => XdmNodeKind.ELEMENT
    case Type.ATTRIBUTE => XdmNodeKind.ATTRIBUTE
    case Type.TEXT => XdmNodeKind.TEXT
    case Type.COMMENT => XdmNodeKind.COMMENT
    case Type.PROCESSING_INSTRUCTION => XdmNodeKind.PROCESSING_INSTRUCTION
    case Type.NAMESPACE => XdmNodeKind.NAMESPACE
    case _ => throw new IllegalStateException("nodeKind")

  }

  def getProcessor: Processor = {
    val config: Configuration = getUnderlyingNode.getConfiguration
    val originator: AnyRef = config.getProcessor
    if (originator.isInstanceOf[Processor]) {
      originator.asInstanceOf[Processor]
    } else {
      new Processor(config)
    }
  }

  override def getUnderlyingValue: NodeInfo =
    super.getUnderlyingValue.asInstanceOf[NodeInfo]

  def getNodeName: QName = {
    val n: NodeInfo = getUnderlyingNode
    n.getNodeKind match {
      case Type.DOCUMENT | Type.TEXT | Type.COMMENT => null
      case Type.PROCESSING_INSTRUCTION | Type.NAMESPACE =>
        if (n.getLocalPart.isEmpty) null else new QName(new StructuredQName("", "", n.getLocalPart))
      case Type.ELEMENT | Type.ATTRIBUTE => new QName(n.getPrefix, n.getURI, n.getLocalPart)
      case _ => null

    }
  }

  def getTypedValue: XdmValue = {
    val v: AtomicSequence = getUnderlyingNode.atomize()
    XdmValue.wrap(v)
  }

  def getLineNumber: Int = getUnderlyingNode.getLineNumber

  def getColumnNumber: Int = getUnderlyingNode.getColumnNumber

  def asSource(): Source = getUnderlyingNode

  def children: java.lang.Iterable[XdmNode] =
    select(Steps.child()).asListOfNodes()

  def children(localName: String): java.lang.Iterable[XdmNode] =
    select(Steps.child(localName)).asListOfNodes()

  def children(uri: String, localName: String): java.lang.Iterable[XdmNode] =
    select(Steps.child(uri, localName)).asListOfNodes()

  def children(filter: Predicate[_ >: XdmNode]): java.lang.Iterable[XdmNode] =
    select(Steps.child(filter)).asListOfNodes()

  def axisIterator(axis: Axis): XdmSequenceIterator[XdmNode] = {
    val base: AxisIterator = getUnderlyingNode.iterateAxis(axis.getAxisNumber)
    XdmSequenceIterator.ofNodes(base)
  }

  def axisIterator(axis: Axis, name: QName): XdmSequenceIterator[XdmNode] = {
    var kind: Int = 0
    axis match {
      case Axis.ATTRIBUTE => kind = Type.ATTRIBUTE
      case Axis.NAMESPACE => kind = Type.NAMESPACE
      case _ => kind = Type.ELEMENT

    }
    val node: NodeInfo = getUnderlyingNode
    val test: NameTest = new NameTest(kind,
      name.getNamespaceURI,
      name.getLocalName,
      node.getConfiguration.getNamePool)
    val base: AxisIterator = node.iterateAxis(axis.getAxisNumber, test)
    XdmSequenceIterator.ofNodes(base)
  }

  def getParent: XdmNode = {
    val p: NodeInfo = getUnderlyingNode.getParent
    if (p == null) null else XdmValue.wrap(p).asInstanceOf[XdmNode]
  }

  def getRoot: XdmNode = {
    val p: NodeInfo = getUnderlyingNode.getRoot
    if (p == null) null else XdmValue.wrap(p).asInstanceOf[XdmNode]
  }

  def getAttributeValue(name: QName): String = {
    val node: NodeInfo = getUnderlyingNode
    node.getAttributeValue(name.getNamespaceURI, name.getLocalName)
  }

  def attribute(name: String): String =
    getUnderlyingNode.getAttributeValue("", name)

  def getBaseURI: URI = {
    val uri: String = getUnderlyingNode.getBaseURI
    if (uri == null) {
      return null
    }
    new URI(uri)
  }

  def getDocumentURI: URI = {
    val systemId: String = getUnderlyingNode.getSystemId
    if (systemId == null || systemId.isEmpty) null else new URI(systemId)
  }

  override def hashCode: Int = getUnderlyingNode.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: XdmNode => getUnderlyingNode == other.getUnderlyingNode
    case _ => false

  }

  override def toString: String = {
    val node: NodeInfo = getUnderlyingNode
    if (node.getNodeKind == Type.ATTRIBUTE) {
      val `val`: String = node.getStringValue
        .replace("&", "&amp;")
        .replace("\"", "&quot;")
        .replace("<", "&lt;")
      node.getDisplayName + "=\"" + `val` + '"'
    } else if (node.getNodeKind == Type.NAMESPACE) {
      val `val`: String = node.getStringValue
        .replace("&", "&amp;")
        .replace("\"", "&quot;")
        .replace("<", "&lt;")
      var name: String = node.getDisplayName
      name = if (name.==("")) "xmlns" else "xmlns:" + name
      return name + "=\"" + `val` + '"'
    } else if (node.getNodeKind == Type.TEXT) {
      node.getStringValue
        .replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace("]]>", "]]&gt;")
    }
    QueryResult.serialize(node).trim()
  }

  def getUnderlyingNode: NodeInfo = getUnderlyingValue

  def getExternalNode: AnyRef = {
    val saxonNode: NodeInfo = getUnderlyingNode
    if (saxonNode.isInstanceOf[VirtualNode]) {
      val externalNode: AnyRef =
        saxonNode.asInstanceOf[VirtualNode].getRealNode
      if (externalNode.isInstanceOf[NodeInfo]) null else externalNode
    } else {
      null
    }
  }

  def nodeiterator: XdmSequenceIterator[XdmNode] = XdmSequenceIterator.ofNode(this)

  override def stream(): XdmStream[XdmNode] = new XdmStream(Stream.of(this))

}
