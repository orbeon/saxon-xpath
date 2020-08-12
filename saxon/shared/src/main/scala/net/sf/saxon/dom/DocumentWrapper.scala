package net.sf.saxon.dom

import net.sf.saxon.utils.Configuration

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.GenericTreeInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.tree.iter.AxisIterator

import org.w3c.dom._

import java.net.URI

import java.net.URISyntaxException

import java.util._

class DocumentWrapper(doc: Node, baseURI: String, config: Configuration) extends GenericTreeInfo(config) {

  var domLevel3: Boolean = true

  val docNode: Node = doc

  private var idIndex: Map[String, NodeInfo] = null

  if (doc.getNodeType != Node.DOCUMENT_NODE && doc.getNodeType != Node.DOCUMENT_FRAGMENT_NODE) {
    throw new IllegalArgumentException(
      "Node must be a DOM Document or DocumentFragment")
  }

  if (config.getExternalObjectModel(doc.getClass) == null) {
    throw new IllegalArgumentException(
      "Node class " + doc.getClass.getName + " is not recognized in this Saxon configuration")
  }

  this.setRootNode(wrap(doc))

  this.systemId = baseURI

  def wrap(node: Node): DOMNodeWrapper = DOMNodeWrapper.makeWrapper(node, this)

  def setDOMLevel(level: Int): Unit = {
    if (!(level == 2 || level == 3)) {
      throw new IllegalArgumentException("DOM Level must be 2 or 3")
    }
    domLevel3 = level == 3
  }

  def getDOMLevel(): Int = if (domLevel3) 3 else 2

  override def selectID(id: String, getParent: Boolean): NodeInfo =
    docNode.synchronized {
      val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
      if (node.isInstanceOf[Document]) {
        val el: Node = node.asInstanceOf[Document].getElementById(id)
        if (el != null) {
          wrap(el)
        }
      }
      if (idIndex != null) {
        idIndex.get(id)
      } else {
        idIndex = new HashMap()
        val iter: AxisIterator =
          getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
        var e: NodeInfo = null
        while ((e = iter.next()) != null) {
          val xmlId: String = e.getAttributeValue(NamespaceConstant.XML, "id")
          if (xmlId != null) {
            idIndex.put(xmlId, e)
          }
        }
        idIndex.get(id)
      }
    }

  override def getUnparsedEntityNames(): Iterator[String] = docNode.synchronized {
    val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
    if (node.isInstanceOf[Document]) {
      val docType: DocumentType = node.asInstanceOf[Document].getDoctype
      if (docType == null) {
        val ls: List[String] = Collections.emptyList()
        ls.iterator()
      }
      val map: NamedNodeMap = docType.getEntities
      if (map == null) {
        val ls: List[String] = Collections.emptyList()
        ls.iterator()
      }
      val names: List[String] = new ArrayList[String](map.getLength)
      for (i <- 0 until map.getLength) {
        val e: Entity = map.item(i).asInstanceOf[Entity]
        if (e.getNotationName != null) {
          names.add(e.getLocalName)
        }
      }
      names.iterator()
    } else {
      null
    }
  }

  override def getUnparsedEntity(name: String): Array[String] = docNode.synchronized {
    val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
    if (node.isInstanceOf[Document]) {
      val docType: DocumentType = node.asInstanceOf[Document].getDoctype
      if (docType == null) {
        null
      }
      val map: NamedNodeMap = docType.getEntities
      if (map == null) {
        null
      }
      val entity: Entity = map.getNamedItem(name).asInstanceOf[Entity]
      if (entity == null || entity.getNotationName == null) {
        null
      }
      var systemId: String = entity.getSystemId
      try {
        var systemIdURI: URI = new URI(systemId)
        if (!systemIdURI.isAbsolute) {
          val base: String = getRootNode.getBaseURI
          if (base != null) {
            systemIdURI = new URI(base).resolve(systemIdURI)
            systemId = systemIdURI.toString
          } else {}
        }
      } catch {
        case err: URISyntaxException => {}

      }
      Array(systemId, entity.getPublicId)
    } else {
      null
    }
  }

}
