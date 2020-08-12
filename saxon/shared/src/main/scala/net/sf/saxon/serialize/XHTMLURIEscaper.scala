package net.sf.saxon.serialize

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeInfo

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.serialize.codenorm.Normalizer

import java.util.HashSet

import XHTMLURIEscaper._

object XHTMLURIEscaper {

  private var urlTable: HashSet[String] = new HashSet[String](70)

  private var attTable: HashSet[String] = new HashSet[String](20)

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
      false
    }
    if (!atcode.hasURI("")) {
      false
    }
    val attName: String = atcode.getLocalPart
    attTable.contains(attName) &&
      urlTable.contains(elcode.getLocalPart + "+" + attName)
  }

  private def isAllAscii(value: CharSequence): Boolean =
    (0 until value.length)
      .find(value.charAt(_) > 127)
      .map(_ => false)
      .getOrElse(true)

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
            var attName: NodeName = att.getNodeName
            if (isUrlAttribute(nameCode, attName)) {
              var value: String = att.getValue
              var normalized: CharSequence =
                (if (isAllAscii(value)) value
                else
                  Normalizer
                    .make(Normalizer.C, getConfiguration)
                    .normalize(value))
              new AttributeInfo(
                attName,
                att.getType,
                HTMLURIEscaper.escapeURL(normalized, true, getConfiguration).toString,
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
