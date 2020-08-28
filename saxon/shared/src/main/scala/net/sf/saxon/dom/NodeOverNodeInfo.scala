package net.sf.saxon.dom

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.CodepointCollator

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.functions.DeepEqual

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.iter.SingletonIterator

import org.w3c.dom._

import java.util.ArrayList

import java.util.List

import NodeOverNodeInfo._

object NodeOverNodeInfo {

  def wrap(node: NodeInfo): NodeOverNodeInfo = {
    var n: NodeOverNodeInfo = null
    if (node == null) {
      return null
    }
    node.getNodeKind match {
      case Type.DOCUMENT => n = new DocumentOverNodeInfo()
      case Type.ELEMENT => n = new ElementOverNodeInfo()
      case Type.ATTRIBUTE => n = new AttrOverNodeInfo()
      case Type.TEXT | Type.COMMENT => n = new TextOverNodeInfo()
      case Type.PROCESSING_INSTRUCTION => n = new PIOverNodeInfo()
      case Type.NAMESPACE => n = new AttrOverNodeInfo()
      case _ => return null

    }
    n.node = node
    n
  }

  def disallowUpdate(): Unit = {
    throw new org.w3c.dom.DOMException(
      DOMException.NO_MODIFICATION_ALLOWED_ERR,
      "The Saxon DOM implementation cannot be updated")
  }

}

abstract class NodeOverNodeInfo extends Node {

  var node: NodeInfo = _

  def getUnderlyingNodeInfo(): NodeInfo = node

  def isSameNode(other: Node): Boolean =
    other.isInstanceOf[NodeOverNodeInfo] && node == other
      .asInstanceOf[NodeOverNodeInfo]
      .node

  override def equals(obj: Any): Boolean = obj match {
    case obj: Node => isSameNode(obj)
    case _ => false

  }

  override def hashCode(): Int = node.hashCode

  def getBaseURI(): String = node.getBaseURI

  def getNodeName(): String = node.getNodeKind match {
    case Type.DOCUMENT => "#document"
    case Type.ELEMENT => node.getDisplayName
    case Type.ATTRIBUTE => node.getDisplayName
    case Type.TEXT => "#text"
    case Type.COMMENT => "#comment"
    case Type.PROCESSING_INSTRUCTION => node.getLocalPart
    case Type.NAMESPACE =>
      if (node.getLocalPart.isEmpty) {
        "xmlns"
      } else {
        "xmlns:" + node.getLocalPart
      }
    case _ => "#unknown"

  }

  def getLocalName(): String = node.getNodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE => node.getLocalPart
    case Type.DOCUMENT | Type.TEXT | Type.COMMENT |
         Type.PROCESSING_INSTRUCTION =>
      null
    case Type.NAMESPACE =>
      if (node.getLocalPart.isEmpty) {
        "xmlns"
      } else {
        node.getLocalPart
      }
    case _ => null

  }

  def hasChildNodes(): Boolean = node.hasChildNodes()

  def hasAttributes(): Boolean = true

  def getNodeType(): Short = {
    val kind: Short = node.getNodeKind.toShort
    if (kind == Type.NAMESPACE) {
      Type.ATTRIBUTE
    } else {
      kind
    }
  }

  def getParentNode(): Node = wrap(node.getParent)

  def getPreviousSibling(): Node =
    wrap(node.iterateAxis(AxisInfo.PRECEDING_SIBLING).next())

  def getNextSibling(): Node =
    wrap(node.iterateAxis(AxisInfo.FOLLOWING_SIBLING).next())

  def getFirstChild(): Node = wrap(node.iterateAxis(AxisInfo.CHILD).next())

  def getLastChild(): Node = {
    val children: AxisIterator = node.iterateAxis(AxisInfo.CHILD)
    var last: NodeInfo = null
    while (true) {
      val next: NodeInfo = children.next()
      if (next == null) {
        return wrap(last)
      } else {
        last = next
      }
    }
    null
  }

  def getNodeValue(): String = node.getNodeKind match {
    case Type.DOCUMENT | Type.ELEMENT => null
    case Type.ATTRIBUTE | Type.TEXT | Type.COMMENT |
         Type.PROCESSING_INSTRUCTION | Type.NAMESPACE =>
      node.getStringValue
    case _ => null

  }

  def setNodeValue(nodeValue: String): Unit = {
    disallowUpdate()
  }

  def getChildNodes(): NodeList = {
    val nodes: List[Node] = new ArrayList[Node](10)
    for (child <- node.children()) {
      nodes.add(NodeOverNodeInfo.wrap(child))
    }
    new DOMNodeList(nodes)
  }

  def getAttributes(): NamedNodeMap = null

  def getOwnerDocument(): Document = wrap(node.getRoot).asInstanceOf[Document]

  def insertBefore(newChild: Node, refChild: Node): Node = {
    disallowUpdate()
    null
  }

  def replaceChild(newChild: Node, oldChild: Node): Node = {
    disallowUpdate()
    null
  }

  def removeChild(oldChild: Node): Node = {
    disallowUpdate()
    null
  }

  def appendChild(newChild: Node): Node = {
    disallowUpdate()
    null
  }

  def cloneNode(deep: Boolean): Node = {
    disallowUpdate()
    null
  }

  def normalize(): Unit = ()

  def isSupported(feature: String, version: String): Boolean =
    (feature.equalsIgnoreCase("XML") || feature.equalsIgnoreCase("Core")) &&
      (version == null || version.isEmpty || version.==("3.0") ||
        version.==("2.0") ||
        version.==("1.0"))

  def getNamespaceURI(): String = {
    if (node.getNodeKind == Type.NAMESPACE) {
      NamespaceConstant.XMLNS
    }
    val uri: String = node.getURI
    if ("" == uri) null else uri
  }

  def getPrefix(): String = {
    if (node.getNodeKind == Type.NAMESPACE) {
      if (node.getLocalPart.isEmpty) {
        return null
      } else {
        return "xmlns"
      }
    }
    val p: String = node.getPrefix
    if ("" == p) null else p
  }

  def setPrefix(prefix: String): Unit = {
    disallowUpdate()
  }

  def compareDocumentPosition(other: Node): Short = {
    val DOCUMENT_POSITION_DISCONNECTED: Short = 0x01
    val DOCUMENT_POSITION_PRECEDING: Short = 0x02
    val DOCUMENT_POSITION_FOLLOWING: Short = 0x04
    val DOCUMENT_POSITION_CONTAINS: Short = 0x08
    val DOCUMENT_POSITION_CONTAINED_BY: Short = 0x10
    if (!(other.isInstanceOf[NodeOverNodeInfo])) {
      return DOCUMENT_POSITION_DISCONNECTED
    }
    val c: Int = node.compareOrder(other.asInstanceOf[NodeOverNodeInfo].node)
    if (c == 0) {
      0.toShort
    } else if (c == -1) {
      var result: Short = DOCUMENT_POSITION_FOLLOWING
      val d: Short = compareDocumentPosition(other.getParentNode)
      if (d == 0 || (d & DOCUMENT_POSITION_CONTAINED_BY) != 0) {
        result = (result | DOCUMENT_POSITION_CONTAINED_BY).toShort
      }
      result
    } else if (c == +1) {
      var result: Short = DOCUMENT_POSITION_PRECEDING
      val d: Short = getParentNode.compareDocumentPosition(other)
      if (d == 0 || (d & DOCUMENT_POSITION_CONTAINS) != 0) {
        result = (result | DOCUMENT_POSITION_CONTAINS).toShort
      }
      result
    } else {
      throw new AssertionError()
    }
  }

  def getTextContent(): String =
    if (node.getNodeKind == Type.DOCUMENT) {
      null
    } else {
      node.getStringValue
    }

  def setTextContent(textContent: String): Unit = {
    disallowUpdate()
  }

  def lookupPrefix(namespaceURI: String): String =
    if (node.getNodeKind == Type.DOCUMENT) {
      null
    } else if (node.getNodeKind == Type.ELEMENT) {
      val iter: AxisIterator = node.iterateAxis(AxisInfo.NAMESPACE)
      var ns: NodeInfo = null
      while (({
        ns = iter.next()
        ns
      }) != null) if (ns.getStringValue == namespaceURI) {
        ns.getLocalPart
      }
      null
    } else {
      getParentNode.lookupPrefix(namespaceURI)
    }

  def isDefaultNamespace(namespaceURI: String): Boolean =
    namespaceURI == lookupNamespaceURI("")

  def lookupNamespaceURI(prefix: String): String =
    if (node.getNodeKind == Type.DOCUMENT) {
      null
    } else if (node.getNodeKind == Type.ELEMENT) {
      val iter: AxisIterator = node.iterateAxis(AxisInfo.NAMESPACE)
      var ns: NodeInfo = null
      while (({
        ns = iter.next()
        ns
      }) != null) if (ns.getLocalPart == prefix) {
        ns.getStringValue
      }
      null
    } else {
      getParentNode.lookupNamespaceURI(prefix)
    }

  def isEqualNode(arg: Node): Boolean = {
    if (!(arg.isInstanceOf[NodeOverNodeInfo])) {
      throw new IllegalArgumentException(
        "Other Node must wrap a Saxon NodeInfo")
    }
    try {
      val context: XPathContext = node.getConfiguration.getConversionContext
      DeepEqual.deepEqual(
        SingletonIterator.makeIterator(node),
        SingletonIterator.makeIterator(
          arg.asInstanceOf[NodeOverNodeInfo].node),
        new GenericAtomicComparer(CodepointCollator.getInstance, context),
        context,
        DeepEqual.INCLUDE_PREFIXES | DeepEqual.INCLUDE_COMMENTS |
          DeepEqual.COMPARE_STRING_VALUES |
          DeepEqual.INCLUDE_PROCESSING_INSTRUCTIONS
      )
    } catch {
      case err: XPathException => false

    }
  }

  def getFeature(feature: String, version: String): AnyRef = null

  def setUserData(key: String,
                  data: AnyRef,
                  handler: UserDataHandler): AnyRef = {
    disallowUpdate()
    null
  }

  def getUserData(key: String): AnyRef = null

}
