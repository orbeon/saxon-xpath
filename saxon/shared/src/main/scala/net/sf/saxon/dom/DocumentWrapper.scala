package net.sf.saxon.dom

import java.net.{URI, URISyntaxException}
import java.util
import java.util._

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.om.{AxisInfo, GenericTreeInfo, NodeInfo}
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.utils.Configuration
import org.w3c.dom._

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

  def getDOMLevel: Int = if (domLevel3) 3 else 2

  override def selectID(id: String, getParent: Boolean): NodeInfo =
    docNode.synchronized {
      val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
      node match {
        case document: Document =>
          val el = document.getElementById(id)
          if (el != null) {
            return wrap(el)
          }
        case _ =>
      }
      if (idIndex != null) {
        idIndex.get(id)
      } else {
        idIndex = new HashMap()
        val iter = getRootNode.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
        var e: NodeInfo = null
        while ({
          e = iter.next()
          e
        } != null) {
          val xmlId = e.getAttributeValue(NamespaceConstant.XML, "id")
          if (xmlId != null)
            idIndex.put(xmlId, e)
        }
        idIndex.get(id)
      }
    }

  override def getUnparsedEntityNames: Iterator[String] = docNode.synchronized {
    val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
    node match {
      case document: Document =>
        val docType = document.getDoctype
        if (docType == null) {
          val ls: util.List[String] = Collections.emptyList()
          ls.iterator()
        }
        val map = docType.getEntities
        if (map == null) {
          val ls: util.List[String] = Collections.emptyList()
          ls.iterator()
        }
        val names = new util.ArrayList[String](map.getLength)
        for (i <- 0 until map.getLength) {
          val e = map.item(i).asInstanceOf[Entity]
          if (e.getNotationName != null)
            names.add(e.getLocalName)
        }
        names.iterator()
      case _ =>
        null
    }
  }

  override def getUnparsedEntity(name: String): Array[String] = docNode.synchronized {
    val node: Node = getRootNode.asInstanceOf[DOMNodeWrapper].node
    node match {
      case document: Document =>
        val docType = document.getDoctype
        if (docType == null)
          return null
        val map = docType.getEntities
        if (map == null)
          return null
        val entity = map.getNamedItem(name).asInstanceOf[Entity]
        if (entity == null || entity.getNotationName == null)
          return null
        var systemId = entity.getSystemId
        try {
          var systemIdURI = new URI(systemId)
          if (! systemIdURI.isAbsolute) {
            val base = getRootNode.getBaseURI
            if (base != null) {
              systemIdURI = new URI(base).resolve(systemIdURI)
              systemId = systemIdURI.toString
            } else {

            }
          }
        } catch {
          case _: URISyntaxException =>
        }
        Array(systemId, entity.getPublicId)
      case _ =>
        null
    }
  }
}
