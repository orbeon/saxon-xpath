package net.sf.saxon.dom

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.TreeInfo

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.tree.iter.AxisIterator

import org.w3c.dom._

import java.util.ArrayList

import java.util.List
import NodeOverNodeInfo._
import scala.util.control.Breaks._

object DocumentOverNodeInfo {

   def getElementsByTagName(node: NodeInfo,
                                     tagname: String): NodeList = {
     val allElements: AxisIterator = node.iterateAxis(AxisInfo.DESCENDANT)
     val nodes: List[Node] = new ArrayList[Node](100)
     breakable {
       while (true) {
         val next: NodeInfo = allElements.next()
         if (next == null) {
           break
         }
         if (next.getNodeKind == Type.ELEMENT) {
           if (tagname.==("*") || tagname == next.getDisplayName) {
             nodes.add(NodeOverNodeInfo.wrap(next))
           }
         }
       }
   }
    new DOMNodeList(nodes)
  }

  def getElementsByTagNameNS(node: NodeInfo,
                             namespaceURI: String,
                             localName: String): NodeList = {
    val ns: String = if (namespaceURI == null) "" else namespaceURI
    val allElements: AxisIterator = node.iterateAxis(AxisInfo.DESCENDANT)
    val nodes: List[Node] = new ArrayList[Node](100)
    breakable {
      while (true) {
        val next: NodeInfo = allElements.next()
        if (next == null) {
          break
        }
        if (next.getNodeKind == Type.ELEMENT) {
          if ((ns.==("*") || ns == next.getURI) &&
            (localName.==("*") || localName == next.getLocalPart)) {
            nodes.add(NodeOverNodeInfo.wrap(next))
          }
        }
      }
    }
    new DOMNodeList(nodes)
  }

}

class DocumentOverNodeInfo extends NodeOverNodeInfo with Document {

  def getDoctype(): DocumentType = null

  def getImplementation(): DOMImplementation = new DOMImplementationImpl()

  def createElement(tagName: String): Element = {
    disallowUpdate()
    null
  }

  def createDocumentFragment(): DocumentFragment = null

  def createTextNode(data: String): Text = null

  def createComment(data: String): Comment = null

  def createCDATASection(data: String): CDATASection = {
    disallowUpdate()
    null
  }

  def createProcessingInstruction(target: String,
                                  data: String): ProcessingInstruction = {
    disallowUpdate()
    null
  }

  def createAttribute(name: String): Attr = {
    disallowUpdate()
    null
  }

  def createEntityReference(name: String): EntityReference = {
    disallowUpdate()
    null
  }

  def getElementsByTagName(tagname: String): NodeList =
    DocumentOverNodeInfo.getElementsByTagName(node, tagname)

  def getDocumentElement(): Element = {
    val root: NodeInfo = node.getRoot
    if (root == null) {
      null
    }
    val children: AxisIterator =
      root.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    wrap(children.next()).asInstanceOf[Element]
  }

  def importNode(importedNode: Node, deep: Boolean): Node = {
    disallowUpdate()
    null
  }

  def createElementNS(namespaceURI: String, qualifiedName: String): Element = {
    disallowUpdate()
    null
  }

  def createAttributeNS(namespaceURI: String, qualifiedName: String): Attr = {
    disallowUpdate()
    null
  }

  def getElementsByTagNameNS(namespaceURI: String,
                             localName: String): NodeList =
    DocumentOverNodeInfo.getElementsByTagNameNS(node, namespaceURI, localName)

  def getElementById(elementId: String): Element = {
    val doc: TreeInfo = node.getTreeInfo
    if (doc == null) {
      null
    }
    wrap(doc.selectID(elementId, false)).asInstanceOf[Element]
  }

  def getInputEncoding(): String = null

  def getXmlEncoding(): String = null

  def getXmlStandalone(): Boolean = false

  def setXmlStandalone(xmlStandalone: Boolean): Unit = {
    disallowUpdate()
  }

  def getXmlVersion(): String = "1.0"

  def setXmlVersion(xmlVersion: String): Unit = {
    disallowUpdate()
  }

  def getStrictErrorChecking(): Boolean = false

  def setStrictErrorChecking(strictErrorChecking: Boolean): Unit = {}

  def getDocumentURI(): String = node.getSystemId

  def setDocumentURI(documentURI: String): Unit = {
    disallowUpdate()
  }

  def adoptNode(source: Node): Node = {
    disallowUpdate()
    null
  }

  def getDomConfig(): DOMConfiguration = null

  def normalizeDocument(): Unit = {
    disallowUpdate()
  }

  def renameNode(n: Node, namespaceURI: String, qualifiedName: String): Node = {
    disallowUpdate()
    null
  }

}
