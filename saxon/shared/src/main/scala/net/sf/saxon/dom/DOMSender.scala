package net.sf.saxon.dom

import net.sf.saxon.event.NamespaceReducer

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.Type

import net.sf.saxon.model.Untyped

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import org.w3c.dom.CharacterData

import org.w3c.dom._

import java.net.URI

import java.net.URISyntaxException

import java.util.ArrayList

import java.util.List

import java.util.Stack

import scala.beans.{BeanProperty, BooleanBeanProperty}


class DOMSender(startNode: Node, receiver: Receiver) {

  private var varReceiver: Receiver = new NamespaceReducer(receiver)

   var root: Node = startNode

   var systemId: String = _

  private var namespaces: Stack[NamespaceMap] = new Stack()

  @BeanProperty
  var currentNode: Node = _

  if (startNode == null) {
    throw new NullPointerException("startNode")
  }

  if (receiver == null) {
    throw new NullPointerException("receiver")
  }

  namespaces.push(NamespaceMap.emptyMap)

  def setSystemId(systemId: String): Unit = {
    this.systemId = systemId
  }

  def send(): Unit = {
    receiver.setSystemId(systemId)
    val loc: Location = Loc.NONE
    root.getNodeType match {
      case Node.DOCUMENT_NODE | Node.DOCUMENT_FRAGMENT_NODE =>
        receiver.startDocument(ReceiverOption.NONE)
        sendUnparsedEntities()
        walkNode(root)
        receiver.endDocument()
      case Node.ELEMENT_NODE => sendElement(root.asInstanceOf[Element])
      case Node.TEXT_NODE | Node.CDATA_SECTION_NODE =>
        receiver.characters(root.asInstanceOf[CharacterData].getData,
          loc,
          ReceiverOption.NONE)
      case Node.COMMENT_NODE =>
        receiver.comment(root.asInstanceOf[Comment].getData,
          loc,
          ReceiverOption.NONE)
      case Node.PROCESSING_INSTRUCTION_NODE =>
        receiver.processingInstruction(
          root.asInstanceOf[ProcessingInstruction].getTarget,
          root.asInstanceOf[ProcessingInstruction].getData,
          loc,
          ReceiverOption.NONE)
      case _ =>
        throw new IllegalStateException(
          "DOMSender: unsupported kind of start node (" + root.getNodeType +
            ")")

    }
  }

  private def sendUnparsedEntities(): Unit = {
    if (root.isInstanceOf[Document]) {
      val docType: DocumentType = root.asInstanceOf[Document].getDoctype
      if (docType != null) {
        val map: NamedNodeMap = docType.getEntities
        if (map != null) {
          for (i <- 0 until map.getLength) {
            val e: Entity = map.item(i).asInstanceOf[Entity]
            if (e.getNotationName != null) {
              val name: String = e.getNodeName
              var systemId: String = e.getSystemId
              try {
                var systemIdURI: URI = new URI(systemId)
                if (!systemIdURI.isAbsolute) {
                  val base: String = root.getBaseURI
                  if (base != null) {
                    systemIdURI = new URI(base).resolve(systemIdURI)
                    systemId = systemIdURI.toString
                  } else {}
                }
              } catch {
                case err: URISyntaxException => {}

              }
              val publicId: String = e.getPublicId
              receiver.setUnparsedEntity(name, systemId, publicId)
            }
          }
        }
      }
    }
  }

  private def sendElement(startNode: Element): Unit = {
    val ancestors: List[Element] = new ArrayList[Element]()
    var inScopeNamespaces: NamespaceMap = NamespaceMap.emptyMap
    var parent: Node = startNode
    while (parent != null && parent.getNodeType == Type.ELEMENT) {
      ancestors.add(parent.asInstanceOf[Element])
      parent = parent.getParentNode
    }
    var i: Int = ancestors.size - 1
    while (i >= 0) {
      inScopeNamespaces = inScopeNamespaces.applyDifferences(
        gatherNamespaces(ancestors.get(i)))
        i += 1
    }
    namespaces.push(inScopeNamespaces)
    outputElement(startNode, hasNamespaceDeclarations = true)
    namespaces.pop()
  }

  private def getNodeName(name: String, useDefaultNS: Boolean): NodeName = {
    val colon: Int = name.indexOf(':')
    if (colon < 0) {
      if (useDefaultNS) {
        val uri: String = getUriForPrefix("")
        if (!uri.isEmpty) {
          new FingerprintedQName("", uri, name)
        }
      }
      new NoNamespaceName(name)
    } else {
      val prefix: String = name.substring(0, colon)
      val uri: String = getUriForPrefix(prefix)
      if (uri == null) {
        throw new IllegalStateException(
          "Prefix " + prefix + " is not bound to any namespace")
      }
      new FingerprintedQName(prefix, uri, name.substring(colon + 1))
    }
  }

  private def walkNode(node: Node): Unit = {
    val loc: Location = Loc.NONE
    if (node.hasChildNodes) {
      val nit: NodeList = node.getChildNodes
      val len = nit.getLength
      for (i <- 0 until len) {
        val child: Node = nit.item(i)
        currentNode = child
        child.getNodeType match {
          case Node.DOCUMENT_NODE | Node.DOCUMENT_FRAGMENT_NODE =>
          case Node.ELEMENT_NODE =>
            var element: Element = child.asInstanceOf[Element]
            var parentNamespaces: NamespaceMap = namespaces.peek()
            val childNamespaces: NamespaceMap = parentNamespaces.applyDifferences(gatherNamespaces(element))
            namespaces.push(childNamespaces)
            outputElement(element, !childNamespaces.isEmpty)
            namespaces.pop()
          case Node.ATTRIBUTE_NODE =>
          case Node.PROCESSING_INSTRUCTION_NODE =>
            receiver.processingInstruction(
              child.asInstanceOf[ProcessingInstruction].getTarget,
              child.asInstanceOf[ProcessingInstruction].getData,
              loc,
              ReceiverOption.NONE)
          case Node.COMMENT_NODE => {
            val text: String = child.asInstanceOf[Comment].getData
            if (text != null) {
              receiver.comment(text, loc, ReceiverOption.NONE)
            }
          }
          case Node.TEXT_NODE | Node.CDATA_SECTION_NODE => {
            val text: String = child.asInstanceOf[CharacterData].getData
            if (text != null) {
              receiver.characters(text, loc, ReceiverOption.NONE)
            }
          }
          case Node.ENTITY_REFERENCE_NODE => walkNode(child)
          case _ =>

        }
      }
    }
  }

  private def outputElement(element: Element,
                            hasNamespaceDeclarations: Boolean): Unit = {
    val name: NodeName = getNodeName(element.getTagName, useDefaultNS = true)
    val loc: Location = new Loc(systemId, -1, -1)
    var attributes: AttributeMap = EmptyAttributeMap.getInstance
    val atts: NamedNodeMap = element.getAttributes
    if (atts != null) {
      val len = atts.getLength
      for (a2 <- 0 until len) {
        val att: Attr = atts.item(a2).asInstanceOf[Attr]
        val props: Int =
          if (att.isId) ReceiverOption.IS_ID else ReceiverOption.NONE
        val attname: String = att.getName
        if (hasNamespaceDeclarations && attname.startsWith("xmlns") &&
          ((attname.length == 5 || attname.charAt(5) == ':'))) {} else {
          val attNodeName: NodeName = getNodeName(attname, useDefaultNS = false)
          attributes = attributes.put(
            new AttributeInfo(attNodeName,
              BuiltInAtomicType.UNTYPED_ATOMIC,
              att.getValue,
              loc,
              props))
        }
      }
    }
    receiver.startElement(name,
      Untyped.getInstance,
      attributes,
      namespaces.peek(),
      loc,
      ReceiverOption.NONE)
    walkNode(element)
    receiver.endElement()
  }

  private def getUriForPrefix(prefix: String): String = {
    if (prefix.==("xml")) {
      NamespaceConstant.XML
    }
    val uri: String = namespaces.peek().getURI(prefix)
    if (uri == null) "" else uri
  }

  private def gatherNamespaces(element: Element): NamespaceDeltaMap = {
    var result: NamespaceDeltaMap = NamespaceDeltaMap.emptyMap()
    try {
      var prefix: String = element.getPrefix
      var uri: String = element.getNamespaceURI
      if (prefix == null) {
        prefix = ""
      }
      if (uri == null) {
        uri = ""
      }
      result = result.put(prefix, uri)
    } catch {
      case err: Throwable => {}

    }
    val atts: NamedNodeMap = element.getAttributes
    if (atts == null) {
      return result
    }
    val alen: Int = atts.getLength
    for (a1 <- 0 until alen) {
      val att: Attr = atts.item(a1).asInstanceOf[Attr]
      val attname: String = att.getName
      val possibleNamespace: Boolean = attname.startsWith("xmlns")
      if (possibleNamespace && attname.length == 5) {
        val uri: String = att.getValue
        result = result.put("", uri)
      } else if (possibleNamespace && attname.startsWith("xmlns:")) {
        val prefix: String = attname.substring(6)
        val uri: String = att.getValue
        result = result.put(prefix, uri)
      } else if (attname.indexOf(':') >= 0) {
        try {
          val prefix: String = att.getPrefix
          val uri: String = att.getNamespaceURI
          result = result.put(prefix, uri)
        } catch {
          case err: Throwable => {}

        }
      }
    }
    result
  }

}
