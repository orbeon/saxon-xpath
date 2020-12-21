package org.orbeon.saxon.serialize

import javax.xml.transform.OutputKeys
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.{AttributeMap, NamespaceMap, NodeName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.HTMLEmitter._

object HTML50Emitter {
  setEmptyTag("area")
  setEmptyTag("base")
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
  setEmptyTag("keygen")
  setEmptyTag("link")
  setEmptyTag("meta")
  setEmptyTag("param")
  setEmptyTag("source")
  setEmptyTag("track")
  setEmptyTag("wbr")
}

class HTML50Emitter extends HTMLEmitter {

  version = 5

  def isHTMLElement(name: NodeName): Boolean = {
    val uri = name.getURI
    uri.==("") || uri == NamespaceConstant.XHTML
  }

  override  def openDocument(): Unit = {
    version = 5
    super.openDocument()
  }

  override  def writeDocType(name: NodeName,
                                      displayName: String,
                                      systemId: String,
                                      publicId: String): Unit =
    if (systemId == null && publicId == null)
      writer.write("<!DOCTYPE HTML>")
    else
      super.writeDocType(name, displayName, systemId, publicId)

  override  def writeDocTypeWithNullSystemId(): Boolean = true

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (!started) {
      openDocument()
      var systemId = outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM)
      var publicId = outputProperties.getProperty(OutputKeys.DOCTYPE_PUBLIC)
      if ("" == systemId)
        systemId = null
      if ("" == publicId)
        publicId = null
      writeDocType(null, "html", systemId, publicId)
      started = true
    }
    super.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  def rejectControlCharacters: Boolean = false
}
