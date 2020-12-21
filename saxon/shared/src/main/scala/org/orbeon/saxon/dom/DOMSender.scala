//package org.orbeon.saxon.dom
//
//import java.net.{URI, URISyntaxException}
//import java.{util => ju}
//
//import org.orbeon.saxon.event.{Receiver, ReceiverOption}
//import org.orbeon.saxon.expr.parser.Loc
//import org.orbeon.saxon.lib.NamespaceConstant
//import org.orbeon.saxon.model.{BuiltInAtomicType, Type, Untyped}
//import org.orbeon.saxon.om._
//import org.orbeon.saxon.s9api.Location
//import org.w3c.dom.{CharacterData, _}
//
//import scala.beans.BeanProperty
//
//
//class DOMSender(startNode: Node, receiver: Receiver) {
//
//  val root: Node = startNode
//
//  var systemId: String = _
//
//  private var namespaces: List[NamespaceMap] = Nil
//
//  @BeanProperty
//  var currentNode: Node = _
//
//  if (startNode == null)
//    throw new NullPointerException("startNode")
//
//  if (receiver == null)
//    throw new NullPointerException("receiver")
//
//  namespaces::= NamespaceMap.emptyMap
//
//  def setSystemId(systemId: String): Unit =
//    this.systemId = systemId
//
//  def send(): Unit = {
//    receiver.setSystemId(systemId)
//    val loc: Location = Loc.NONE
//    root.getNodeType match {
//      case Node.DOCUMENT_NODE | Node.DOCUMENT_FRAGMENT_NODE =>
//        receiver.startDocument(ReceiverOption.NONE)
//        sendUnparsedEntities()
//        walkNode(root)
//        receiver.endDocument()
//      case Node.ELEMENT_NODE => sendElement(root.asInstanceOf[Element])
//      case Node.TEXT_NODE | Node.CDATA_SECTION_NODE =>
//        receiver.characters(root.asInstanceOf[CharacterData].getData,
//          loc,
//          ReceiverOption.NONE)
//      case Node.COMMENT_NODE =>
//        receiver.comment(root.asInstanceOf[Comment].getData,
//          loc,
//          ReceiverOption.NONE)
//      case Node.PROCESSING_INSTRUCTION_NODE =>
//        receiver.processingInstruction(
//          root.asInstanceOf[ProcessingInstruction].getTarget,
//          root.asInstanceOf[ProcessingInstruction].getData,
//          loc,
//          ReceiverOption.NONE)
//      case _ =>
//        throw new IllegalStateException(
//          "DOMSender: unsupported kind of start node (" + root.getNodeType +
//            ")")
//
//    }
//  }
//
//  private def sendUnparsedEntities(): Unit = {
//    root match {
//      case document: Document =>
//        val docType = document.getDoctype
//        if (docType != null) {
//          val map = docType.getEntities
//          if (map != null) {
//            for (i <- 0 until map.getLength) {
//              val e: Entity = map.item(i).asInstanceOf[Entity]
//              if (e.getNotationName != null) {
//                val name = e.getNodeName
//                var systemId = e.getSystemId
//                try {
//                  var systemIdURI = new URI(systemId)
//                  if (! systemIdURI.isAbsolute) {
//                    val base = root.getBaseURI
//                    if (base != null) {
//                      systemIdURI = new URI(base).resolve(systemIdURI)
//                      systemId = systemIdURI.toString
//                    }
//                  }
//                } catch {
//                  case _: URISyntaxException =>
//                }
//                val publicId = e.getPublicId
//                receiver.setUnparsedEntity(name, systemId, publicId)
//              }
//            }
//          }
//        }
//      case _ =>
//    }
//  }
//
//  private def sendElement(startNode: Element): Unit = {
//    val ancestors: ju.List[Element] = new ju.ArrayList[Element]
//    var inScopeNamespaces: NamespaceMap = NamespaceMap.emptyMap
//    var parent: Node = startNode
//    while (parent != null && parent.getNodeType == Type.ELEMENT) {
//      ancestors.add(parent.asInstanceOf[Element])
//      parent = parent.getParentNode
//    }
//    var i: Int = ancestors.size - 1
//    while (i >= 0) {
//      inScopeNamespaces = inScopeNamespaces.applyDifferences(
//        gatherNamespaces(ancestors.get(i)))
//        i += 1
//    }
//    namespaces ::= inScopeNamespaces
//    outputElement(startNode, hasNamespaceDeclarations = true)
//    namespaces = namespaces.tail
//  }
//
//  private def getNodeName(name: String, useDefaultNS: Boolean): NodeName = {
//    val colon: Int = name.indexOf(':')
//    if (colon < 0) {
//      if (useDefaultNS) {
//        val uri: String = getUriForPrefix("")
//        if (!uri.isEmpty) {
//          new FingerprintedQName("", uri, name)
//        }
//      }
//      new NoNamespaceName(name)
//    } else {
//      val prefix: String = name.substring(0, colon)
//      val uri: String = getUriForPrefix(prefix)
//      if (uri == null) {
//        throw new IllegalStateException(
//          "Prefix " + prefix + " is not bound to any namespace")
//      }
//      new FingerprintedQName(prefix, uri, name.substring(colon + 1))
//    }
//  }
//
//  private def walkNode(node: Node): Unit = {
//    val loc: Location = Loc.NONE
//    if (node.hasChildNodes) {
//      val nit: NodeList = node.getChildNodes
//      val len = nit.getLength
//      for (i <- 0 until len) {
//        val child: Node = nit.item(i)
//        currentNode = child
//        child.getNodeType match {
//          case Node.DOCUMENT_NODE | Node.DOCUMENT_FRAGMENT_NODE =>
//          case Node.ELEMENT_NODE =>
//            val element: Element = child.asInstanceOf[Element]
//            val parentNamespaces: NamespaceMap = namespaces.head
//            val childNamespaces: NamespaceMap = parentNamespaces.applyDifferences(gatherNamespaces(element))
//            namespaces ::= childNamespaces
//            outputElement(element, !childNamespaces.isEmpty)
//            namespaces = namespaces.tail
//          case Node.ATTRIBUTE_NODE =>
//          case Node.PROCESSING_INSTRUCTION_NODE =>
//            receiver.processingInstruction(
//              child.asInstanceOf[ProcessingInstruction].getTarget,
//              child.asInstanceOf[ProcessingInstruction].getData,
//              loc,
//              ReceiverOption.NONE)
//          case Node.COMMENT_NODE =>
//            val text = child.asInstanceOf[Comment].getData
//            if (text != null)
//              receiver.comment(text, loc, ReceiverOption.NONE)
//          case Node.TEXT_NODE | Node.CDATA_SECTION_NODE =>
//            val text = child.asInstanceOf[CharacterData].getData
//            if (text != null)
//              receiver.characters(text, loc, ReceiverOption.NONE)
//          case Node.ENTITY_REFERENCE_NODE => walkNode(child)
//          case _ =>
//
//        }
//      }
//    }
//  }
//
//  private def outputElement(element: Element,
//                            hasNamespaceDeclarations: Boolean): Unit = {
//    val name: NodeName = getNodeName(element.getTagName, useDefaultNS = true)
//    val loc: Location = new Loc(systemId, -1, -1)
//    var attributes: AttributeMap = EmptyAttributeMap.getInstance
//    val atts: NamedNodeMap = element.getAttributes
//    if (atts != null) {
//      val len = atts.getLength
//      for (a2 <- 0 until len) {
//        val att: Attr = atts.item(a2).asInstanceOf[Attr]
//        val props: Int =
//          if (att.isId) ReceiverOption.IS_ID else ReceiverOption.NONE
//        val attname: String = att.getName
//        if (hasNamespaceDeclarations && attname.startsWith("xmlns") &&
//          (attname.length == 5 || attname.charAt(5) == ':')) {} else {
//          val attNodeName: NodeName = getNodeName(attname, useDefaultNS = false)
//          attributes = attributes.put(
//            new AttributeInfo(attNodeName,
//              BuiltInAtomicType.UNTYPED_ATOMIC,
//              att.getValue,
//              loc,
//              props))
//        }
//      }
//    }
//    receiver.startElement(name,
//      Untyped.getInstance,
//      attributes,
//      namespaces.head,
//      loc,
//      ReceiverOption.NONE)
//    walkNode(element)
//    receiver.endElement()
//  }
//
//  private def getUriForPrefix(prefix: String): String = {
//    if (prefix.==("xml")) {
//      NamespaceConstant.XML
//    }
//    val uri: String = namespaces.head.getURI(prefix)
//    if (uri == null) "" else uri
//  }
//
//  private def gatherNamespaces(element: Element): NamespaceDeltaMap = {
//    var result: NamespaceDeltaMap = NamespaceDeltaMap.emptyMap()
//    try {
//      var prefix: String = element.getPrefix
//      var uri: String = element.getNamespaceURI
//      if (prefix == null) {
//        prefix = ""
//      }
//      if (uri == null) {
//        uri = ""
//      }
//      result = result.put(prefix, uri)
//    } catch {
//      case _: Throwable =>
//    }
//    val atts = element.getAttributes
//    if (atts == null)
//      return result
//
//    val alen = atts.getLength
//    for (a1 <- 0 until alen) {
//      val att = atts.item(a1).asInstanceOf[Attr]
//      val attname = att.getName
//      val possibleNamespace = attname.startsWith("xmlns")
//      if (possibleNamespace && attname.length == 5) {
//        val uri = att.getValue
//        result = result.put("", uri)
//      } else if (possibleNamespace && attname.startsWith("xmlns:")) {
//        val prefix = attname.substring(6)
//        val uri = att.getValue
//        result = result.put(prefix, uri)
//      } else if (attname.indexOf(':') >= 0) {
//        try {
//          val prefix = att.getPrefix
//          val uri = att.getNamespaceURI
//          result = result.put(prefix, uri)
//        } catch {
//          case _: Throwable =>
//        }
//      }
//    }
//    result
//  }
//}
