package net.sf.saxon.dom

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.Type
import net.sf.saxon.model.UType
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.NamespaceBinding
import net.sf.saxon.om.NamespaceMap
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.pattern.AnyNodeTest
import net.sf.saxon.pattern.NameTest
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.tree.util.Navigator
import net.sf.saxon.tree.util.SteppingNavigator
import net.sf.saxon.tree.util.SteppingNode
import net.sf.saxon.tree.wrapper.AbstractNodeWrapper
import net.sf.saxon.tree.wrapper.SiblingCountingNode
import org.jetbrains.annotations.NotNull
import org.w3c.dom._
import java.util.ArrayList
import java.util.Arrays
import java.util.EnumSet
import java.util.function.Predicate

import DOMNodeWrapper._

import scala.util.control.Breaks._
import net.sf.saxon.om.SequenceIterator.Property._

import scala.collection.mutable

object DOMNodeWrapper {

  def makeWrapper(node: Node,
                  docWrapper: DocumentWrapper): DOMNodeWrapper = {
    if (node == null) {
      throw new NullPointerException(
        "NodeWrapper#makeWrapper: Node must not be null")
    }
    if (docWrapper == null) {
      throw new NullPointerException(
        "NodeWrapper#makeWrapper: DocumentWrapper must not be null")
    }
    makeWrapper(node, docWrapper, null, -1)
  }

  def makeWrapper(node: Node,
                  docWrapper: DocumentWrapper,
                  parent: DOMNodeWrapper,
                  index: Int): DOMNodeWrapper = {
    var wrapper: DOMNodeWrapper = null
    node.getNodeType match {
      case Node.DOCUMENT_NODE | Node.DOCUMENT_FRAGMENT_NODE =>
        wrapper = docWrapper.getRootNode.asInstanceOf[DOMNodeWrapper]
        if (wrapper == null) {
          wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
          wrapper.nodeKind = Type.DOCUMENT
        }
      case Node.ELEMENT_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.ELEMENT
      case Node.ATTRIBUTE_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.ATTRIBUTE
      case Node.TEXT_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.TEXT
      case Node.CDATA_SECTION_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.TEXT
      case Node.COMMENT_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.COMMENT
      case Node.PROCESSING_INSTRUCTION_NODE =>
        wrapper = new DOMNodeWrapper(node, docWrapper, parent, index)
        wrapper.nodeKind = Type.PROCESSING_INSTRUCTION
      case Node.ENTITY_REFERENCE_NODE =>
        throw new IllegalStateException(
          "DOM contains entity reference nodes, which Saxon does not support. The DOM should be built using the expandEntityReferences() option")
      case _ =>
        throw new IllegalArgumentException(
          "Unsupported node type in DOM! " + node.getNodeType +
            " instance " +
            node)

    }
    wrapper.treeInfo = docWrapper
    wrapper
  }

  private def emptyIfNull(s: String): String = if (s == null) "" else s

  def expandStringValue(list: NodeList, sb: FastStringBuffer): Unit = {
    val len: Int = list.getLength
    for (i <- 0 until len) {
      val child: Node = list.item(i)
      child.getNodeType match {
        case Node.ELEMENT_NODE => expandStringValue(child.getChildNodes, sb)
        case Node.COMMENT_NODE | Node.PROCESSING_INSTRUCTION_NODE =>
        case Node.DOCUMENT_TYPE_NODE =>
        case _ => sb.append(emptyIfNull(child.getNodeValue))

      }
    }
  }

  def getLocalName(node: Node): String = {
    val s: String = node.getLocalName
    if (s == null) {
      val n: String = node.getNodeName
      val colon: Int = n.indexOf(':')
      if (colon >= 0) {
        n.substring(colon + 1)
      }
      n
    } else {
      s
    }
  }

  private def getElementURI(element: Element): String = {
    val uri: String = element.getNamespaceURI
    if (uri != null) {
      uri
    }
    val displayName: String = element.getNodeName
    val colon: Int = displayName.indexOf(':')
    val attName: String =
      if (colon < 0) "xmlns" else "xmlns:" + displayName.substring(0, colon)
    if (attName.==("xmlns:xml")) {
      NamespaceConstant.XML
    }
    var node: Node = element
    do {
      if (node.asInstanceOf[Element].hasAttribute(attName)) {
        node.asInstanceOf[Element].getAttribute(attName)
      }
      node = node.getParentNode
    } while (node != null && node.getNodeType == Node.ELEMENT_NODE);
    if (colon < 0) {
      ""
    } else {
      throw new IllegalStateException(
        "Undeclared namespace prefix in element name " + displayName +
          " in DOM input")
    }
  }

  private def getAttributeURI(attr: Attr): String = {
    val uri: String = attr.getNamespaceURI
    if (uri != null) {
      uri
    }
    val displayName: String = attr.getNodeName
    val colon: Int = displayName.indexOf(':')
    if (colon < 0) {
      ""
    }
    val attName: String = "xmlns:" + displayName.substring(0, colon)
    if (attName.==("xmlns:xml")) {
      NamespaceConstant.XML
    }
    var node: Node = attr.getOwnerElement
    do {
      if (node.asInstanceOf[Element].hasAttribute(attName)) {
        node.asInstanceOf[Element].getAttribute(attName)
      }
      node = node.getParentNode
    } while (node != null && node.getNodeType == Node.ELEMENT_NODE);
    throw new IllegalStateException(
      "Undeclared namespace prefix in attribute name " + displayName +
        " in DOM input")
  }

  private def getSuccessorNode(start: Node, anchor: Node): Node = {
    if (start.hasChildNodes()) return start.getFirstChild

    if (anchor != null && start.isSameNode(anchor)) return null
    var p: Node = start
    while (true) {
      val s: Node = p.getNextSibling
      if (s != null) return s
      p = p.getParentNode
      if (p == null || (anchor != null && p.isSameNode(anchor))) {
        return null
      }
    }
    null
  }

}

class DOMNodeWrapper(var node: Node,
                     var docWrapper: DocumentWrapper,
                     var parent: DOMNodeWrapper,
                     var index: Int)
  extends AbstractNodeWrapper
    with SiblingCountingNode
    with SteppingNode[DOMNodeWrapper] {

  val enumSet = Set[Property]()

  var nodeKind: Short = _

  var span: Int = 1

  private var localNamespaces: Array[NamespaceBinding] = null

  private var inScopeNamespaces: NamespaceMap = null

  override def getTreeInfo(): DocumentWrapper = treeInfo.asInstanceOf[DocumentWrapper]

  def getUnderlyingNode(): Node = node

  def getNodeKind(): Int = nodeKind

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[DOMNodeWrapper])) {
      return false
    }
    if (docWrapper.domLevel3) {
      docWrapper.docNode.synchronized {
        node.isSameNode(other.asInstanceOf[DOMNodeWrapper].node)
      }
    } else {
      val ow: DOMNodeWrapper = other.asInstanceOf[DOMNodeWrapper]
      getNodeKind == ow.getNodeKind && equalOrNull(getLocalPart,
        ow.getLocalPart) &&
        getSiblingPosition == ow.getSiblingPosition &&
        getParent == ow.getParent
    }
  }

  private def equalOrNull(a: String, b: String): Boolean =
    if (a == null) b == null else a == b

  def compareOrder(other: NodeInfo): Int = {
    if (other.isInstanceOf[DOMNodeWrapper] && docWrapper.domLevel3) {
      if (equals(other)) return 0
      try docWrapper.docNode.synchronized {
        val relationship: Short =
          node.compareDocumentPosition(other.asInstanceOf[DOMNodeWrapper].node)
        if ((relationship &
          (Node.DOCUMENT_POSITION_PRECEDING | Node.DOCUMENT_POSITION_CONTAINS)) !=
          0) {
          +1
        } else if ((relationship &
          (Node.DOCUMENT_POSITION_FOLLOWING | Node.DOCUMENT_POSITION_CONTAINED_BY)) !=
          0) {
          -1
        }
      } catch {
        case e: DOMException => {}

      }
    }
    if (other.isInstanceOf[SiblingCountingNode]) {
      Navigator.compareOrder(this, other.asInstanceOf[SiblingCountingNode])
    } else {
      -other.compareOrder(this)
    }
  }

  def getStringValueCS(): CharSequence = docWrapper.docNode.synchronized {
    nodeKind match {
      case Type.DOCUMENT | Type.ELEMENT =>
        var children1: NodeList = node.getChildNodes
        var sb1: FastStringBuffer = new FastStringBuffer(16)
        expandStringValue(children1, sb1)
        sb1
      case Type.ATTRIBUTE => emptyIfNull(node.asInstanceOf[Attr].getValue)
      case Type.TEXT =>
        if (span == 1) {
          emptyIfNull(node.getNodeValue)
        } else {
          val fsb: FastStringBuffer =
            new FastStringBuffer(FastStringBuffer.C64)
          var textNode: Node = node
          for (i <- 0 until span) {
            fsb.append(emptyIfNull(textNode.getNodeValue))
            textNode = textNode.getNextSibling
          }
          fsb.condense()
        }
      case Type.COMMENT | Type.PROCESSING_INSTRUCTION =>
        emptyIfNull(node.getNodeValue)
      case _ => ""

    }
  }

  def getLocalPart(): String = docWrapper.docNode.synchronized {
    getNodeKind match {
      case Type.ELEMENT | Type.ATTRIBUTE => getLocalName(node)
      case Type.PROCESSING_INSTRUCTION => node.getNodeName
      case _ => ""

    }
  }

  def getURI(): String = docWrapper.docNode.synchronized {
    if (nodeKind == Type.ELEMENT) {
      getElementURI(node.asInstanceOf[Element])
    } else if (nodeKind == Type.ATTRIBUTE) {
      getAttributeURI(node.asInstanceOf[Attr])
    }
    ""
  }

  def getPrefix(): String = docWrapper.docNode.synchronized {
    val kind: Int = getNodeKind
    if (kind == Type.ELEMENT || kind == Type.ATTRIBUTE) {
      val name: String = node.getNodeName
      val colon: Int = name.indexOf(':')
      if (colon < 0) {
        ""
      } else {
        name.substring(0, colon)
      }
    }
    ""
  }

  override def getDisplayName(): String = nodeKind match {
    case Type.ELEMENT | Type.ATTRIBUTE | Type.PROCESSING_INSTRUCTION =>
      docWrapper.docNode.synchronized {
        node.getNodeName
      }
    case _ => ""

  }

  def getParent(): DOMNodeWrapper = {
    if (parent == null) {
      docWrapper.docNode.synchronized {
        getNodeKind match {
          case Type.ATTRIBUTE =>
            parent =
              makeWrapper(node.asInstanceOf[Attr].getOwnerElement, docWrapper)
          case _ =>
            var p: Node = node.getParentNode
            if (p == null) {
              return null
            } else {
              parent = makeWrapper(p, docWrapper)
            }

        }
      }
    }
    parent
  }

  def getSiblingPosition(): Int = {
    if (index == -1) {
      docWrapper.docNode.synchronized {
        nodeKind match {
          case Type.ELEMENT | Type.TEXT | Type.COMMENT |
               Type.PROCESSING_INSTRUCTION =>
            var ix: Int = 0
            var start: Node = node
            while (true) {
              start = start.getPreviousSibling
              if (start == null) {
                index = ix
                return ix
              }
              {
                ix += 1;
              }
            }
          case Type.ATTRIBUTE =>
            var ix = 0
            var iter: AxisIterator = parent.iterateAxis(AxisInfo.ATTRIBUTE)
            while (true) {
              val n: NodeInfo = iter.next()
              if (n == null || Navigator.haveSameName(this, n)) {
                index = ix
                return ix
              }
              {
                ix += 1;
              }
            }
          case Type.NAMESPACE =>
            var ix = 0
            var iter = parent.iterateAxis(AxisInfo.NAMESPACE)
            while (true) {
              val n: NodeInfo = iter.next()
              if (n == null || Navigator.haveSameName(this, n)) {
                index = ix
                return ix
              }
              {
                ix += 1;
              }
            }
          case _ =>
            index = 0
            index

        }
      }
    }
    index
  }

  override def iterateAttributes(
                                  nodeTest: Predicate[_ >: NodeInfo]): AxisIterator = {
    var iter: AxisIterator = new AttributeEnumeration(this)
    if (nodeTest != AnyNodeTest.getInstance) {
      iter = new Navigator.AxisFilter(iter, nodeTest)
    }
    iter
  }

  private def isElementOnly(nodeTest: Predicate[_ >: NodeInfo]): Boolean =
    nodeTest.isInstanceOf[NodeTest] &&
      nodeTest.asInstanceOf[NodeTest].getUType == UType.ELEMENT

  override def iterateChildren(
                                nodeTest: Predicate[_ >: NodeInfo]): AxisIterator = {
    val elementOnly: Boolean = isElementOnly(nodeTest)
    var iter: AxisIterator = new Navigator.EmptyTextFilter(
      new ChildEnumeration(this, true, true, elementOnly))
    if (nodeTest != AnyNodeTest.getInstance) {
      iter = new Navigator.AxisFilter(iter, nodeTest)
    }
    iter
  }

  override def iterateSiblings(nodeTest: Predicate[_ >: NodeInfo],
                               forwards: Boolean): AxisIterator = {
    val elementOnly: Boolean = isElementOnly(nodeTest)
    var iter: AxisIterator = new Navigator.EmptyTextFilter(
      new ChildEnumeration(this, false, forwards, elementOnly))
    if (nodeTest != AnyNodeTest.getInstance) {
      iter = new Navigator.AxisFilter(iter, nodeTest)
    }
    iter
  }

  override def iterateDescendants(
                                   nodeTest: Predicate[_ >: NodeInfo],
                                   includeSelf: Boolean): AxisIterator =
    new SteppingNavigator.DescendantAxisIterator(this, includeSelf, nodeTest)

  override def getAttributeValue(uri: String, local: String): String = {
    val test: NameTest = new NameTest(Type.ATTRIBUTE, uri, local, getNamePool)
    val iterator: AxisIterator = iterateAxis(AxisInfo.ATTRIBUTE, test)
    val attribute: NodeInfo = iterator.next()
    if (attribute == null) {
      null
    } else {
      attribute.getStringValue
    }
  }

  override def getRoot(): NodeInfo = docWrapper.getRootNode

  override def hasChildNodes(): Boolean = docWrapper.docNode.synchronized {
    node.getNodeType != Node.ATTRIBUTE_NODE && node.hasChildNodes()
  }

  def generateId(buffer: FastStringBuffer): Unit = {
    Navigator.appendSequentialKey(this, buffer, true)
  }

  override def getDeclaredNamespaces(
                                      buffer: Array[NamespaceBinding]): Array[NamespaceBinding] =
    docWrapper.docNode.synchronized {
      if (node.getNodeType == Node.ELEMENT_NODE) {
        if (localNamespaces != null) {
          return localNamespaces
        }
        val elem: Element = node.asInstanceOf[Element]
        val atts: NamedNodeMap = elem.getAttributes
        if (atts == null) {
          localNamespaces = NamespaceBinding.EMPTY_ARRAY
          NamespaceBinding.EMPTY_ARRAY
        }
        var count: Int = 0
        val attsLen: Int = atts.getLength
        for (i <- 0 until attsLen) {
          val att: Attr = atts.item(i).asInstanceOf[Attr]
          val attName: String = att.getName
          if (attName.==("xmlns")) {
            count += 1
          } else if (attName.startsWith("xmlns:")) {
            count += 1
          }
        }
        if (count == 0) {
          localNamespaces = NamespaceBinding.EMPTY_ARRAY
          NamespaceBinding.EMPTY_ARRAY
        } else {
          val result: Array[NamespaceBinding] =
            if (buffer == null || count > buffer.length)
              Array.ofDim[NamespaceBinding](count)
            else buffer
          var n: Int = 0
          for (i <- 0 until attsLen) {
            val att: Attr = atts.item(i).asInstanceOf[Attr]
            val attName: String = att.getName
            if (attName.==("xmlns")) {
              val prefix: String = ""
              val uri: String = att.getValue
              n += 1
              result(n) = new NamespaceBinding(prefix, uri)
            } else if (attName.startsWith("xmlns:")) {
              val prefix: String = attName.substring(6)
              val uri: String = att.getValue
              n += 1
              result(n) = new NamespaceBinding(prefix, uri)
            }
          }
          if (count < result.length) {
            result(count) = null
          }
          localNamespaces = Arrays.copyOf(result, result.length)
          result
        }
      } else {
        null
      }
    }

  override def getAllNamespaces(): NamespaceMap =
    if (getNodeKind == Type.ELEMENT) {
      if (inScopeNamespaces != null) {
        inScopeNamespaces
      } else {
        val parent: NodeInfo = getParent
        var nsMap: NamespaceMap =
          if (parent != null && parent.getNodeKind == Type.ELEMENT)
            parent.getAllNamespaces
          else NamespaceMap.emptyMap
        val elem: Element = node.asInstanceOf[Element]
        val atts: NamedNodeMap = elem.getAttributes
        if (atts != null) {
          val attsLen: Int = atts.getLength
          for (i <- 0 until attsLen) {
            val att: Attr = atts.item(i).asInstanceOf[Attr]
            val attName: String = att.getName
            if (attName.startsWith("xmlns")) {
              if (attName.length == 5) {
                nsMap = nsMap.bind("", att.getValue)
              } else if (attName.charAt(5) == ':') {
                nsMap = nsMap.bind(attName.substring(6), att.getValue)
              }
            }
          }
        }
        inScopeNamespaces = nsMap
        inScopeNamespaces
      }
    } else {
      null
    }

  override def isId(): Boolean = docWrapper.docNode.synchronized {
    (node.isInstanceOf[Attr]) && node.asInstanceOf[Attr].isId
  }

  def getNextSibling(): DOMNodeWrapper = docWrapper.docNode.synchronized {
    var currNode: Node = node
    for (i <- 0 until span) {
      currNode = currNode.getNextSibling
    }
    if (currNode != null) {
      val `type`: Short = currNode.getNodeType
      if (`type` == Node.DOCUMENT_TYPE_NODE) {
        currNode = currNode.getNextSibling
      } else if (`type` == Node.TEXT_NODE || `type` == Node.CDATA_SECTION_NODE) {
        return spannedWrapper(currNode)
      }
      return makeWrapper(currNode, docWrapper)
    }
    null
  }

  @NotNull
  private def spannedWrapper(currNode: Node): DOMNodeWrapper = {
    var currText: Node = currNode
    var thisSpan: Int = 1
    breakable {
      while (true) {
        currText = currText.getNextSibling
        if (currText != null &&
          (currText.getNodeType == Node.TEXT_NODE || currText.getNodeType == Node.CDATA_SECTION_NODE)) {
          thisSpan += 1
        } else {
          break
        }
      }
    }
    val spannedText: DOMNodeWrapper = makeWrapper(currNode, docWrapper)
    spannedText.span = thisSpan
    spannedText
  }

  def getFirstChild(): DOMNodeWrapper = docWrapper.docNode.synchronized {
    var currNode: Node = node.getFirstChild
    if (currNode != null) {
      if (currNode.getNodeType == Node.DOCUMENT_TYPE_NODE) {
        currNode = currNode.getNextSibling
      }
      if (currNode.getNodeType == Node.TEXT_NODE || currNode.getNodeType == Node.CDATA_SECTION_NODE) {
        return spannedWrapper(currNode)
      }
      return makeWrapper(currNode, docWrapper)
    }
    null
  }

  def getPreviousSibling(): DOMNodeWrapper = docWrapper.docNode.synchronized {
    var currNode: Node = node.getPreviousSibling
    if (currNode != null) {
      val `type`: Short = currNode.getNodeType
      if (`type` == Node.DOCUMENT_TYPE_NODE) {
        return null
      } else if (`type` == Node.TEXT_NODE || `type` == Node.CDATA_SECTION_NODE) {
        var span: Int = 1
        breakable {
          while (true) {
            val prev: Node = currNode.getPreviousSibling
            if (prev != null &&
              (prev.getNodeType == Node.TEXT_NODE || prev.getNodeType == Node.CDATA_SECTION_NODE)) {
              span += 1
              currNode = prev
            } else {
              break
            }
          }
        }
        val wrapper: DOMNodeWrapper = makeWrapper(currNode, docWrapper)
        wrapper.span = span
        return wrapper
      }
      return makeWrapper(currNode, docWrapper)
    }
    null
  }

  def getSuccessorElement(anchor: DOMNodeWrapper,
                          uri: String,
                          local: String): DOMNodeWrapper =
    docWrapper.docNode.synchronized {
      val stop: Node = if (anchor == null) null else anchor.node
      var next: Node = node
      do next = getSuccessorNode(next, stop) while (next != null &&
        !(next.getNodeType == Node.ELEMENT_NODE && (local == null || local == getLocalName(
          next)) &&
          (uri == null || uri == getElementURI(next.asInstanceOf[Element]))));
      if (next == null) {
        null
      } else {
        makeWrapper(next, docWrapper)
      }
    }

  private class AttributeEnumeration extends AxisIterator with LookaheadIterator {

    private var attList: ArrayList[Node] = new ArrayList(10)

    private var ix: Int = 0

    private var start: DOMNodeWrapper = _

    private var current: DOMNodeWrapper = _

    def this(start: DOMNodeWrapper) {
      this()
      start.docWrapper.docNode.synchronized {
        this.start = start
        val atts: NamedNodeMap = start.node.getAttributes
        if (atts != null) {
          val attsLen: Int = atts.getLength
          for (i <- 0 until attsLen) {
            val name: String = atts.item(i).getNodeName
            if (!(name.startsWith("xmlns") && (name.length == 5 || name.charAt(5) == ':'))) {
              attList.add(atts.item(i))
            }
          }
        }
        ix = 0
      }
    }


    def hasNext(): Boolean = ix < attList.size

    def next(): NodeInfo = {
      if (ix >= attList.size) return null
      current = makeWrapper(attList.get(ix), docWrapper, start, ix)
      ix += 1
      current
    }

    override def getProperties(): Set[Property] = {
      enumSet(LAST_POSITION_FINDER)
      enumSet
    }


  }

  private class ChildEnumeration
    extends AxisIterator
      with LookaheadIterator {

    private var start: DOMNodeWrapper = _

    private var commonParent: DOMNodeWrapper = _

    private var downwards: Boolean = _

    private var forwards: Boolean = _

    private var elementsOnly: Boolean = _

    var childNodes: NodeList = _

    private var childNodesLength: Int = _

    private var ix: Int = _

    private var currentSpan: Int = _

    def this(start: DOMNodeWrapper,
             downwards: Boolean,
             forwards: Boolean,
             elementsOnly: Boolean) {
      this()
      start.docWrapper.docNode.synchronized {
        this.start = start
        this.downwards = downwards
        this.forwards = forwards
        this.elementsOnly = elementsOnly
        currentSpan = 1
        commonParent = if (downwards) start else start.getParent
        childNodes = commonParent.node.getChildNodes
        childNodesLength = childNodes.getLength
        if (downwards) {
          currentSpan = 1
          ix = if (forwards) -1 else childNodesLength
        } else {
          ix = start.getSiblingPosition
          currentSpan = start.span
        }
      }
    }


    private def skipPrecedingTextNodes(): Int = {
      var count: Int = 0
      breakable {
        while (ix >= count) {
          val node: Node = childNodes.item(ix - count)
          val kind: Short = node.getNodeType
          if (kind == Node.TEXT_NODE || kind == Node.CDATA_SECTION_NODE) {
            count += 1
          } else {
            break
          }
        }
      }
      if (count == 0) 1 else count
    }

    private def skipFollowingTextNodes(): Int = {
      var count: Int = 0
      var pos: Int = ix
      val len: Int = childNodesLength
      breakable {
        while (pos < len) {
          val node: Node = childNodes.item(pos)
          val kind: Short = node.getNodeType
          if (kind == Node.TEXT_NODE || kind == Node.CDATA_SECTION_NODE) {
            pos += 1
            count += 1
          } else break
        }
      }
      if (count == 0) 1 else count
    }

    def hasNext(): Boolean =
      if (forwards) {
        ix + currentSpan < childNodesLength
      } else {
        ix > 0
      }

    def next(): NodeInfo = start.docWrapper.docNode.synchronized {
      while (true) if (forwards) {
        ix += currentSpan
        breakable {
          if (ix >= childNodesLength) {
            return null
          } else {
            currentSpan = skipFollowingTextNodes()
            val currentDomNode: Node = childNodes.item(ix)
            currentDomNode.getNodeType match {
              case Node.DOCUMENT_TYPE_NODE =>
              case Node.ELEMENT_NODE => break
              case _ =>
                if (elementsOnly) {
                } else {
                  break
                }

            }
            val wrapper: DOMNodeWrapper =
              makeWrapper(currentDomNode, docWrapper, commonParent, ix)
            wrapper.span = currentSpan
            return wrapper
          }
        }
      } else {
        ix -= 1
        breakable {
          if (ix < 0) {
            return null
          } else {
            currentSpan = skipPrecedingTextNodes()
            ix -= currentSpan - 1
            val currentDomNode: Node = childNodes.item(ix)
            currentDomNode.getNodeType match {
              case Node.DOCUMENT_TYPE_NODE =>
              case Node.ELEMENT_NODE => break
              case _ =>
                if (elementsOnly) {

                } else {
                  break
                }

            }
            val wrapper: DOMNodeWrapper =
              makeWrapper(currentDomNode, docWrapper, commonParent, ix)
            wrapper.span = currentSpan
            return wrapper
          }
        }
      }
      null
    }

    override def getProperties(): Set[Property] = {
      val enumSet = Set[Property]()
      enumSet(LOOKAHEAD)
      enumSet
    }

  }

}
