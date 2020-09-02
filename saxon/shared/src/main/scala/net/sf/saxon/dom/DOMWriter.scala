package net.sf.saxon.dom

import net.sf.saxon.event.Builder

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import org.w3c.dom._

import javax.xml.parsers.DocumentBuilderFactory

import javax.xml.parsers.ParserConfigurationException

import java.util.Stack

class DOMWriter extends Builder {

  var pipeConf: PipelineConfiguration = _

  private var currentNode: Node = _

  private var document: Document = _

  private var nextSibling: Node = _

  private var level: Int = 0

  private var canNormalize: Boolean = true

  var systemIdStr: String = _

  private var nsStack: Stack[NamespaceMap] = new Stack()

  nsStack.push(NamespaceMap.emptyMap)

  override def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipeConf = pipe
    config = pipe.getConfiguration
  }

  override def getPipelineConfiguration: PipelineConfiguration = pipeConf

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit = ()

  override def open(): Unit = ()

  override def close(): Unit = ()

  def startDocument(properties: Int): Unit = {
    if (document == null) {
      val factory: DocumentBuilderFactory =
        DocumentBuilderFactory.newInstance()
      factory.setNamespaceAware(true)
      document = factory.newDocumentBuilder().newDocument()
      currentNode = document
    }
  }

  def endDocument(): Unit = ()

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    val qname: String = elemName.getDisplayName
    val uri: String = elemName.getURI
    val element: Element =
      document.createElementNS(if ("" == uri) null else uri, qname)
    if (nextSibling != null && level == 0) {
      currentNode.insertBefore(element, nextSibling)
    } else {
      currentNode.appendChild(element)
    }
    currentNode = element
    val parentNamespaces: NamespaceMap = nsStack.peek()
    if (namespaces != parentNamespaces) {
      val declarations: Array[NamespaceBinding] =
        namespaces.getDifferences(parentNamespaces, addUndeclarations = false)
      for (ns <- declarations) {
        val prefix: String = ns.getPrefix
        val nsuri: String = ns.getURI
        if (nsuri != NamespaceConstant.XML) {
          if (prefix.isEmpty) {
            element.setAttributeNS(NamespaceConstant.XMLNS, "xmlns", nsuri)
          } else {
            element.setAttributeNS(NamespaceConstant.XMLNS,
              "xmlns:" + prefix,
              nsuri)
          }
        }
      }
    }
    nsStack.push(namespaces)
    for (att <- attributes) {
      val attName: NodeName = att.getNodeName
      val atturi: String = attName.getURI
      element.setAttributeNS(if ("" == atturi) null else atturi,
        attName.getDisplayName,
        att.getValue)
      if (attName == StandardNames.XML_ID_NAME ||
        ReceiverOption.contains(properties, ReceiverOption.IS_ID) ||
        attName.hasURI(NamespaceConstant.XML) && attName.getLocalPart.==(
          "id")) {
        val localName: String = attName.getLocalPart
        element.setIdAttributeNS(if ("" == atturi) null else atturi,
          localName,
          true)
      }
    }
    level += 1
  }

  def endElement(): Unit = {
    nsStack.pop()
    if (canNormalize) {
      try currentNode.normalize()
      catch {
        case err: Throwable => canNormalize = false

      }
    }
    currentNode = currentNode.getParentNode
    level -= 1
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    if (level == 0 && nextSibling == null && Whitespace.isWhite(chars)) {
      return
    }
    val text: Text = document.createTextNode(chars.toString)
    if (nextSibling != null && level == 0) {
      currentNode.insertBefore(text, nextSibling)
    } else {
      currentNode.appendChild(text)
    }
  }

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    val pi: ProcessingInstruction =
      document.createProcessingInstruction(target, data.toString)
    if (nextSibling != null && level == 0) {
      currentNode.insertBefore(pi, nextSibling)
    } else {
      currentNode.appendChild(pi)
    }
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    val comment: Comment = document.createComment(chars.toString)
    if (nextSibling != null && level == 0) {
      currentNode.insertBefore(comment, nextSibling)
    } else {
      currentNode.appendChild(comment)
    }
  }

 override def usesTypeAnnotations: Boolean = false

  def setNode(node: Node): Unit = {
    if (node == null) {
      return
    }
    currentNode = node
    if (node.getNodeType == Node.DOCUMENT_NODE) {
      document = node.asInstanceOf[Document]
    } else {
      document = currentNode.getOwnerDocument
      if (document == null) {
        document = new DocumentOverNodeInfo()
      }
    }
  }

  def setNextSibling(nextSibling: Node): Unit = {
    this.nextSibling = nextSibling
  }

  override def getCurrentRoot(): NodeInfo =
    new DocumentWrapper(document, systemIdStr, config).getRootNode

   def getDOMDocumentNode: Document = document

}
