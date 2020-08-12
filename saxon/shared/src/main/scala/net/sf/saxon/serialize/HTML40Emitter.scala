package net.sf.saxon.serialize

import net.sf.saxon.lib.SaxonOutputKeys

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import javax.xml.transform.OutputKeys

import HTMLEmitter._



object HTML40Emitter {

  setEmptyTag("area")

  setEmptyTag("base")

  setEmptyTag("basefont")

  setEmptyTag("br")

  setEmptyTag("col")

  setEmptyTag("embed")

  setEmptyTag("frame")

  setEmptyTag("hr")

  setEmptyTag("img")

  setEmptyTag("input")

  setEmptyTag("isindex")

  setEmptyTag("link")

  setEmptyTag("meta")

  setEmptyTag("param")

}

class HTML40Emitter extends HTMLEmitter {

   override def isHTMLElement(name: NodeName): Boolean =
    name.getURI.==("")

   override def openDocument(): Unit = {
    var versionProperty: String =
      outputProperties.getProperty(SaxonOutputKeys.HTML_VERSION)
    if (versionProperty == null) {
      versionProperty = outputProperties.getProperty(OutputKeys.VERSION)
    }
    if (versionProperty != null) {
      if (versionProperty.==("4.0") || versionProperty.==("4.01")) {
        version = 4
      } else {
        val err: XPathException = new XPathException(
          "Unsupported HTML version: " + versionProperty)
        err.setErrorCode("SESU0013")
        throw err
      }
    }
    super.openDocument()
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (!started) {
      openDocument()
      var systemId: String =
        outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM)
      var publicId: String =
        outputProperties.getProperty(OutputKeys.DOCTYPE_PUBLIC)
      if ("" == systemId) {
        systemId = null
      }
      if ("" == publicId) {
        publicId = null
      }
      if (systemId != null || publicId != null) {
        writeDocType(null, "html", systemId, publicId)
      }
      started = true
    }
    super.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

   override def rejectControlCharacters(): Boolean = true

}
