package net.sf.saxon.serialize

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

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
