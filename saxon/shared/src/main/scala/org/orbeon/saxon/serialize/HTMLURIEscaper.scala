////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.serialize.charcode.UTF8CharacterSet

import org.orbeon.saxon.serialize.codenorm.Normalizer

import org.orbeon.saxon.trans.UncheckedXPathException

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import HTMLURIEscaper._


object HTMLURIEscaper {

  private var urlAttributes: HTMLTagHashSet = new HTMLTagHashSet(47)

  private var urlCombinations: HTMLTagHashSet = new HTMLTagHashSet(101)

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

  // see second note in section B.2.1 of HTML 4 specification
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

  private def setUrlAttribute(element: String, attribute: String): Unit = {
    urlAttributes.add(attribute)
    urlCombinations.add(element + '+' + attribute)
  }

  /*@NotNull*/

  def escapeURL(url: CharSequence,
                normalize: Boolean,
                config: Configuration): CharSequence = {
    // optimize for the common case where the string is all ASCII characters
    var i: Int = url.length - 1
    while (i >= 0) {
      val ch: Char = url.charAt(i)
      if (ch < 32 || ch > 126) {
        if (normalize) {
          val normalized: CharSequence =
            Normalizer.make(Normalizer.C, config).normalize(url)
          return reallyEscapeURL(normalized)
        } else {
          return reallyEscapeURL(url)
        }
      }
      i -= 1
    }
    url
  }

  private def reallyEscapeURL(url: CharSequence): CharSequence = {
    val sb = new FastStringBuffer(url.length + 20)
    val hex: String = "0123456789ABCDEF"
    val array: Array[Byte] = Array.ofDim[Byte](4)
    for (i <- 0 until url.length) {
      val ch: Char = url.charAt(i)
      if (ch < 32 || ch > 126) {
        val used: Int = UTF8CharacterSet.getUTF8Encoding(
          ch,
          (if (i + 1 < url.length) url.charAt(i + 1) else ' '),
          array)
        for (b <- 0 until used) {
          //int v = (array[b]>=0 ? array[b] : 256 + array[b]);
          val v: Int = array(b).toInt & 0xff
          sb.cat('%')
          sb.cat(hex.charAt(v / 16))
          sb.cat(hex.charAt(v % 16))
        }
      } else {
        sb.cat(ch)
      }
    }
    sb
  }

}

class HTMLURIEscaper(nextReceiver: Receiver) extends ProxyReceiver(nextReceiver) {

  def isUrlAttribute(element: NodeName, attribute: NodeName): Boolean = {
    if (pool == null) {
      pool = getNamePool
    }
    val attributeName: String = attribute.getDisplayName
    if (!urlAttributes.contains(attributeName)) {
      return false
    }
    val elementName: String = element.getDisplayName
    urlCombinations.contains(elementName + '+' + attributeName)
  }

  var currentElement: NodeName = _

  var escapeURIAttributes: Boolean = true

  var pool: NamePool = _

  override def startDocument(properties: Int): Unit = {
    nextReceiver.startDocument(properties)
    pool = getPipelineConfiguration.getConfiguration.getNamePool
  }

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
              return new AttributeInfo(
                att.getNodeName,
                att.getType,
                escapeURL(value, normalize = true, getConfiguration).toString,
                att.getLocation,
                att.getProperties | ReceiverOption.DISABLE_CHARACTER_MAPS)
            } else {
             return  att
            }
          } else {
            return att
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
