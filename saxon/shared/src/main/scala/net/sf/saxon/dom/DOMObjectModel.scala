package net.sf.saxon.dom

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event._

import net.sf.saxon.expr.JPConverter

import net.sf.saxon.expr.PJConverter

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.ExternalObjectModel

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.wrapper.VirtualNode

import net.sf.saxon.value.SequenceExtent

import org.w3c.dom.Document

import org.w3c.dom.DocumentFragment

import org.w3c.dom.Node

import org.w3c.dom.NodeList

import javax.xml.parsers.DocumentBuilder

import javax.xml.parsers.DocumentBuilderFactory

import javax.xml.parsers.ParserConfigurationException

import javax.xml.transform.Result

import javax.xml.transform.Source

import javax.xml.transform.dom.DOMResult

import javax.xml.transform.dom.DOMSource

import javax.xml.xpath.XPathConstants

import java.util.ArrayList

import java.util.HashSet

import java.util.List

import DOMObjectModel._

object DOMObjectModel {

  private var THE_INSTANCE: DOMObjectModel = new DOMObjectModel()

  private var factory: DocumentBuilderFactory = null

  def getInstance(): DOMObjectModel = THE_INSTANCE

  def sendDOMSource(source: DOMSource, receiver: Receiver): Unit = {
    val startNode: Node = source.getNode
    if (startNode == null) {
      receiver.open()
      receiver.startDocument(ReceiverOption.NONE)
      receiver.endDocument()
      receiver.close()
    } else {
      val driver: DOMSender = new DOMSender(startNode, receiver)
      driver.setSystemId(source.getSystemId)
      receiver.open()
      driver.send()
      receiver.close()
    }
  }

  def convertXPathValueToObject(value: Sequence, target: Class[_]): AnyRef = {
    val requireDOM: Boolean = classOf[Node].isAssignableFrom(target) || (target == classOf[
      NodeList]) ||
      (target.isArray &&
        classOf[Node].isAssignableFrom(target.getComponentType))
    val allowDOM: Boolean = target == classOf[AnyRef] || target
      .isAssignableFrom(classOf[ArrayList[AnyRef]]) ||
      target.isAssignableFrom(classOf[HashSet[AnyRef]]) ||
      (target.isArray && target.getComponentType == classOf[AnyRef])
    if (!(requireDOM || allowDOM)) {
     return  null
    }
    val nodes: List[Node] = new ArrayList[Node](20)
    val iter: SequenceIterator = value.iterate()
    var item: Item = null
    while ((item = iter.next()) != null) {
      if (item.isInstanceOf[VirtualNode]) {
        val o: AnyRef = item.asInstanceOf[VirtualNode].getRealNode
        if (o.isInstanceOf[Node]) {
          nodes.add(o.asInstanceOf[Node])
          //continue
        }
      }
      if (requireDOM) {
        if (item.isInstanceOf[NodeInfo]) {
          nodes.add(NodeOverNodeInfo.wrap(item.asInstanceOf[NodeInfo]))
        } else {
          throw new XPathException(
            "Cannot convert XPath value to Java object: required class is " +
              target.getName +
              "; supplied value has type " +
              Type.displayTypeName(item))
        }
      } else {
       return  null
      }
    }
    if (nodes.isEmpty && !requireDOM) {
      return null
    }
    if (classOf[Node].isAssignableFrom(target)) {
      if (nodes.size != 1) {
        throw new XPathException(
          "Cannot convert XPath value to Java object: requires a single DOM Node" +
            "but supplied value contains " +
            nodes.size +
            " nodes")
      }
      nodes.get(0)
    } else if (target == classOf[NodeList]) {
       new DOMNodeList(nodes)
    } else if (target.isArray && target.getComponentType == classOf[Node]) {
      nodes.toArray(Array.ofDim[Node](0))
    } else if (target.isAssignableFrom(classOf[ArrayList[AnyRef]])) {
      nodes
    } else if (target.isAssignableFrom(classOf[HashSet[AnyRef]])) {
      new HashSet(nodes)
    } else {
      null
    }
  }

}

class DOMObjectModel extends TreeModel with ExternalObjectModel {

  def getDocumentClassName(): String = "org.w3c.dom.Document"

  def getIdentifyingURI(): String = XPathConstants.DOM_OBJECT_MODEL

  override def getName(): String = "DOM"

  def getPJConverter(targetClass: Class[_]): PJConverter =
    if (classOf[Node].isAssignableFrom(targetClass) &&
      !classOf[NodeOverNodeInfo].isAssignableFrom(targetClass)) {
      new PJConverter() {
        def convert(value: Sequence,
                    targetClass: Class[_],
                    context: XPathContext): AnyRef =
          convertXPathValueToObject(value, targetClass)
      }
    } else if (classOf[NodeList] == targetClass) {
      new PJConverter() {
        def convert(value: Sequence,
                    targetClass: Class[_],
                    context: XPathContext): AnyRef =
          convertXPathValueToObject(value, targetClass)
      }
    } else {
      null
    }

  def getJPConverter(sourceClass: Class[_],
                     config: Configuration): JPConverter =
    if (classOf[Node].isAssignableFrom(sourceClass) &&
      !classOf[NodeOverNodeInfo].isAssignableFrom(sourceClass)) {
      new JPConverter() {
        def convert(obj: AnyRef, context: XPathContext): Sequence =
          wrapOrUnwrapNode(obj.asInstanceOf[Node], context.getConfiguration)

        def getItemType(): ItemType = AnyNodeTest.getInstance
      }
    } else if (classOf[NodeList].isAssignableFrom(sourceClass)) {
      new JPConverter() {
        def convert(obj: AnyRef, context: XPathContext): Sequence = {
          val config: Configuration = context.getConfiguration
          val list: NodeList = obj.asInstanceOf[NodeList]
          val len: Int = list.getLength
          val nodes: Array[NodeInfo] = Array.ofDim[NodeInfo](len)
          for (i <- 0 until len) {
            nodes(i) = wrapOrUnwrapNode(list.item(i), config)
          }
          new SequenceExtent(nodes.asInstanceOf[Array[Item]])
        }

        def getItemType(): ItemType = AnyNodeTest.getInstance

        override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
      }
    } else if (classOf[DOMSource].isAssignableFrom(sourceClass)) {
      new JPConverter() {
        def convert(obj: AnyRef, context: XPathContext): Sequence =
          unravel(obj.asInstanceOf[DOMSource], context.getConfiguration)

        def getItemType(): ItemType = AnyNodeTest.getInstance
      }
    } else if (classOf[DocumentWrapper] == sourceClass) {
      new JPConverter() {
        def convert(obj: AnyRef, context: XPathContext): Sequence =
          obj.asInstanceOf[DocumentWrapper].getRootNode

        def getItemType(): ItemType = AnyNodeTest.getInstance
      }
    } else {
      null
    }

  def getNodeListCreator(node: AnyRef): PJConverter =
    if (node == null || node.isInstanceOf[Node] || node
      .isInstanceOf[DOMSource] ||
      node.isInstanceOf[DocumentWrapper] ||
      (node.isInstanceOf[VirtualNode] &&
        node.asInstanceOf[VirtualNode].getRealNode.isInstanceOf[Node])) {
      new PJConverter() {
        def convert(value: Sequence,
                    targetClass: Class[_],
                    context: XPathContext): AnyRef =
          convertXPathValueToObject(value, classOf[NodeList])
      }
    } else {
      null
    }

  def getDocumentBuilder(result: Result): Receiver = {
    if (result.isInstanceOf[DOMResult]) {
      val emitter: DOMWriter = new DOMWriter()
      val root: Node = result.asInstanceOf[DOMResult].getNode
      if (root.isInstanceOf[NodeOverNodeInfo] &&
        !(root
          .asInstanceOf[NodeOverNodeInfo]
          .getUnderlyingNodeInfo
          .isInstanceOf[MutableNodeInfo])) {
        throw new XPathException(
          "Supplied DOMResult is a non-mutable Saxon implementation")
      }
      val nextSibling: Node = result.asInstanceOf[DOMResult].getNextSibling
      if (root == null) {
        if (factory == null) {
          factory = DocumentBuilderFactory.newInstance()
        }
        val docBuilder: DocumentBuilder = factory.newDocumentBuilder()
        val out: Document = docBuilder.newDocument()
        result.asInstanceOf[DOMResult].setNode(out)
        emitter.setNode(out)
      } else {
        emitter.setNode(root)
        emitter.setNextSibling(nextSibling)
      }
      emitter
    }
    null
  }

  override def makeBuilder(pipe: PipelineConfiguration): Builder = {
    val dw: DOMWriter = new DOMWriter()
    dw.setPipelineConfiguration(pipe)
    dw
  }

  def sendSource(source: Source, receiver: Receiver): Boolean = {
    if (source.isInstanceOf[DOMSource]) {
      sendDOMSource(source.asInstanceOf[DOMSource], receiver)
      true
    }
    false
  }

  def wrap(node: Node, config: Configuration): NodeInfo = {
    var dom: Document = null
    dom =
      if (node.getNodeType == Node.DOCUMENT_NODE) node.asInstanceOf[Document]
      else node.getOwnerDocument
    val docWrapper: DocumentWrapper =
      new DocumentWrapper(dom, node.getBaseURI, config)
    docWrapper.wrap(node)
  }

  def copy(node: Node, model: TreeModel, config: Configuration): NodeInfo = {
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    val builder: Builder = model.makeBuilder(pipe)
    builder.open()
    Sender.send(new DOMSource(node), builder, null)
    val result: NodeInfo = builder.getCurrentRoot
    builder.close()
    result
  }

  def unravel(source: Source, config: Configuration): NodeInfo = {
    if (source.isInstanceOf[DOMSource]) {
      val dsnode: Node = source.asInstanceOf[DOMSource].getNode
      if (!(dsnode.isInstanceOf[NodeOverNodeInfo])) {
        var dom: Document = null
        dom =
          if (dsnode.getNodeType == Node.DOCUMENT_NODE)
            dsnode.asInstanceOf[Document]
          else dsnode.getOwnerDocument
        val docWrapper: DocumentWrapper =
          new DocumentWrapper(dom, source.getSystemId, config)
        docWrapper.wrap(dsnode)
      }
    }
    null
  }

  private def wrapOrUnwrapNode(node: Node, config: Configuration): NodeInfo =
    if (node.isInstanceOf[NodeOverNodeInfo]) {
      node.asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo
    } else {
      val doc: TreeInfo = wrapDocument(node, "", config)
      wrapNode(doc, node)
    }

  private def wrapDocument(node: AnyRef,
                           baseURI: String,
                           config: Configuration): TreeInfo = {
    if (node.isInstanceOf[DocumentOverNodeInfo]) {
      node
        .asInstanceOf[DocumentOverNodeInfo]
        .getUnderlyingNodeInfo
        .asInstanceOf[TreeInfo]
    }
    if (node.isInstanceOf[NodeOverNodeInfo]) {
      node.asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo.getTreeInfo
    }
    if (node.isInstanceOf[org.w3c.dom.Node]) {
      if (node.asInstanceOf[Node].getNodeType == Node.DOCUMENT_NODE) {
        val doc: Document = node.asInstanceOf[org.w3c.dom.Document]
        new DocumentWrapper(doc, baseURI, config)
      } else if (node
        .asInstanceOf[Node]
        .getNodeType == Node.DOCUMENT_FRAGMENT_NODE) {
        val doc: DocumentFragment =
          node.asInstanceOf[org.w3c.dom.DocumentFragment]
        new DocumentWrapper(doc, baseURI, config)
      } else {
        val doc: Document =
          node.asInstanceOf[org.w3c.dom.Node].getOwnerDocument
        new DocumentWrapper(doc, baseURI, config)
      }
    }
    throw new IllegalArgumentException("Unknown node class " + node.getClass)
  }

  private def wrapNode(document: TreeInfo, node: AnyRef): NodeInfo =
    document.asInstanceOf[DocumentWrapper].wrap(node.asInstanceOf[Node])

}
