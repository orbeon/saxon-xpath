package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class XHTMLPrefixRemover(next: Receiver) extends ProxyReceiver(next) {

  private def isSpecial(uri: String): Boolean =
    uri == NamespaceConstant.XHTML || uri == NamespaceConstant.SVG ||
      uri == NamespaceConstant.MATHML

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    var nameSpaceMap = namespaces
    var elemNodeName = elemName
    for (ns <- nameSpaceMap.asScala if isSpecial(ns.getURI)) {
      nameSpaceMap = nameSpaceMap.remove(ns.getPrefix)
    }
    if (isSpecial(elemNodeName.getURI)) {
      val uri: String = elemNodeName.getURI
      if (!elemNodeName.getPrefix.isEmpty) {
        elemNodeName = new FingerprintedQName("", uri, elemNodeName.getLocalPart)
      }
      nameSpaceMap = nameSpaceMap.put("", uri)
    }
    for (att <- attributes if isSpecial(att.getNodeName.getURI)) {
      nameSpaceMap = nameSpaceMap.put(att.getNodeName.getPrefix, att.getNodeName.getURI)
    }
    nextReceiver.startElement(elemNodeName,
      `type`,
      attributes,
      nameSpaceMap,
      location,
      properties)
  }

}
