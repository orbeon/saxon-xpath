//package org.orbeon.saxon.dom
//
//import org.orbeon.saxon.utils.Configuration
//import org.orbeon.saxon.event.Receiver
//import org.orbeon.saxon.event.Sender
//import org.orbeon.saxon.expr.JPConverter
//import org.orbeon.saxon.expr.PJConverter
//import org.orbeon.saxon.expr.XPathContext
//import org.orbeon.saxon.lib.ExternalObjectModel
//import org.orbeon.saxon.model.ItemType
//import org.orbeon.saxon.om.{Item, NodeInfo, Sequence}
//import org.orbeon.saxon.pattern.AnyNodeTest
//import org.orbeon.saxon.trans.XPathException
//import org.orbeon.saxon.value.SequenceExtent
//import org.w3c.dom.Node
//import org.w3c.dom.NodeList
//import javax.xml.transform.Result
//import javax.xml.transform.Source
//import javax.xml.transform.dom.DOMSource
//import javax.xml.xpath.XPathConstants
//
//object DOMEnvelope {
//
//  private var THE_INSTANCE: DOMEnvelope = new DOMEnvelope()
//
//  def getInstance: DOMEnvelope = THE_INSTANCE
//
//}
//
//class DOMEnvelope extends ExternalObjectModel {
//
//  def getDocumentClassName(): String = "org.w3c.dom.Document"
//
//  def getIdentifyingURI: String = XPathConstants.DOM_OBJECT_MODEL
//
//  def getPJConverter(targetClass: Class[_]): PJConverter =
//    if (classOf[NodeOverNodeInfo].isAssignableFrom(targetClass)) {
//      new PJConverter() {
//        def convert(value: Sequence,
//                    targetClass: Class[_],
//                    context: XPathContext): AnyRef =
//          DOMObjectModel.convertXPathValueToObject(value, targetClass)
//      }
//    } else if (classOf[NodeList].isAssignableFrom(targetClass)) {
//      new PJConverter() {
//        def convert(value: Sequence,
//                    targetClass: Class[_],
//                    context: XPathContext): AnyRef =
//          DOMObjectModel.convertXPathValueToObject(value, targetClass)
//      }
//    } else {
//      null
//    }
//
//  def getJPConverter(sourceClass: Class[_],
//                     config: Configuration): JPConverter =
//    if (classOf[NodeOverNodeInfo].isAssignableFrom(sourceClass)) {
//      new JPConverter() {
//        def convert(`object`: AnyRef, context: XPathContext): Sequence =
//          convertObjectToXPathValue(`object`)
//
//        def getItemType: ItemType = AnyNodeTest
//      }
//    } else {
//      null
//    }
//
//  def getNodeListCreator(node: AnyRef): PJConverter = null
//
//  def isRecognizedNode(`object`: AnyRef): Boolean =
//    `object`.isInstanceOf[NodeOverNodeInfo]
//
//  def isRecognizedNodeClass(nodeClass: Class[_]): Boolean =
//    classOf[NodeOverNodeInfo].isAssignableFrom(nodeClass)
//
//  def getDocumentBuilder(result: Result): Receiver = null
//
//  def sendSource(source: Source, receiver: Receiver): Boolean = {
//    if (source.isInstanceOf[DOMSource]) {
//      val startNode: Node = source.asInstanceOf[DOMSource].getNode
//      if (startNode.isInstanceOf[NodeOverNodeInfo]) {
//        val base: NodeInfo =
//          startNode.asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo
//        Sender.send(base, receiver, null)
//        return true
//      }
//    }
//    false
//  }
//
//  def unravel(source: Source, config: Configuration): NodeInfo = {
//    if (source.isInstanceOf[DOMSource]) {
//      val dsnode: Node = source.asInstanceOf[DOMSource].getNode
//      if (dsnode.isInstanceOf[NodeOverNodeInfo]) {
//        dsnode.asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo
//      }
//    }
//    null
//  }
//
//  private def convertObjectToXPathValue(`object`: AnyRef): Sequence =
//    if (`object`.isInstanceOf[NodeList]) {
//      val list: NodeList = `object`.asInstanceOf[NodeList]
//      val len = list.getLength
//      if (len == 0) {
//        return null
//      }
//      val nodes: Array[NodeInfo] = Array.ofDim[NodeInfo](len)
//      for (i <- 0 until len) {
//        if (list.item(i).isInstanceOf[NodeOverNodeInfo]) {
//          nodes(i) =
//            list.item(i).asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo
//        } else {
//          return null
//        }
//      }
//      new SequenceExtent(nodes.asInstanceOf[Array[Item]])
//    } else if (`object`.isInstanceOf[NodeOverNodeInfo]) {
//      `object`.asInstanceOf[NodeOverNodeInfo].getUnderlyingNodeInfo
//    } else {
//      null
//    }
//
//}
