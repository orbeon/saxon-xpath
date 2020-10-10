package org.orbeon.saxon.serialize

import java.util.HashSet

import org.orbeon.saxon.event.{Receiver, ReceiverOption}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.{AttributeInfo, AttributeMap, NamespaceMap, NodeName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.XHTMLURIEscaper._
import org.orbeon.saxon.serialize.codenorm.Normalizer

object XHTMLURIEscaper {

  private val urlTable: HashSet[String] = new HashSet[String](70)
  private val attTable: HashSet[String] = new HashSet[String](20)

  private def setUrlAttribute(element: String, attribute: String): Unit = {
    attTable.add(attribute)
    urlTable.add(element + "+" + attribute)
  }

  setUrlAttribute("form", "action")
  setUrlAttribute("object", "archive")
  setUrlAttribute("body", "background")
  setUrlAttribute("q", "cite")
  setUrlAttribute("blockquote", "cite")
  setUrlAttribute("del", "cite")
  setUrlAttribute("ins", "cite")
  setUrlAttribute("object", "classid")
  setUrlAttribute("object", "codebase")
  setUrlAttribute("applet", "codebase")
  setUrlAttribute("object", "data")
  setUrlAttribute("button", "datasrc")
  setUrlAttribute("div", "datasrc")
  setUrlAttribute("input", "datasrc")
  setUrlAttribute("object", "datasrc")
  setUrlAttribute("select", "datasrc")
  setUrlAttribute("span", "datasrc")
  setUrlAttribute("table", "datasrc")
  setUrlAttribute("textarea", "datasrc")
  setUrlAttribute("script", "for")
  setUrlAttribute("a", "href")
  setUrlAttribute("a", "name")
  setUrlAttribute("area", "href")
  setUrlAttribute("link", "href")
  setUrlAttribute("base", "href")
  setUrlAttribute("img", "longdesc")
  setUrlAttribute("frame", "longdesc")
  setUrlAttribute("iframe", "longdesc")
  setUrlAttribute("head", "profile")
  setUrlAttribute("script", "src")
  setUrlAttribute("input", "src")
  setUrlAttribute("frame", "src")
  setUrlAttribute("iframe", "src")
  setUrlAttribute("img", "src")
  setUrlAttribute("img", "usemap")
  setUrlAttribute("input", "usemap")
  setUrlAttribute("object", "usemap")

  private def isURLAttribute(elcode: NodeName, atcode: NodeName): Boolean = {
    if (!elcode.hasURI(NamespaceConstant.XHTML)) {
      return false
    }
    if (!atcode.hasURI("")) {
      return false
    }
    val attName: String = atcode.getLocalPart
    attTable.contains(attName) &&
      urlTable.contains(elcode.getLocalPart + "+" + attName)
  }

  private def isAllAscii(value: CharSequence): Boolean =
    (0 until value.length).forall(value.charAt(_) <= 127)

}

class XHTMLURIEscaper(next: Receiver) extends HTMLURIEscaper(next) {

   override def startElement(nameCode: NodeName,
                             `type`: SchemaType,
                             attributes: AttributeMap,
                             namespaces: NamespaceMap,
                             location: Location,
                             properties: Int): Unit = {
    currentElement = nameCode
    var atts2: AttributeMap = attributes
    if (escapeURIAttributes) {
      atts2 = attributes.apply(
        (att) =>
          if (!ReceiverOption.contains(att.getProperties,
            ReceiverOption.DISABLE_ESCAPING)) {
            val attName: NodeName = att.getNodeName
            if (isUrlAttribute(nameCode, attName)) {
              val value: String = att.getValue
              val normalized: CharSequence =
                (if (isAllAscii(value)) value
                else
                  Normalizer
                    .make(Normalizer.C, getConfiguration)
                    .normalize(value))
              new AttributeInfo(
                attName,
                att.getType,
                HTMLURIEscaper.escapeURL(normalized, normalize = true, getConfiguration).toString,
                att.getLocation,
                att.getProperties | ReceiverOption.DISABLE_CHARACTER_MAPS)
            } else {
              att
            }
          } else {
            att
          })
    }
    nextReceiver.startElement(nameCode,
      `type`,
      atts2,
      namespaces,
      location,
      properties)
  }
}
