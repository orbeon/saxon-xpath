////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.serialize.charcode.UTF8CharacterSet

import net.sf.saxon.serialize.codenorm.Normalizer

import net.sf.saxon.trans.UncheckedXPathException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

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
          reallyEscapeURL(normalized)
        } else {
          reallyEscapeURL(url)
        }
      }
      {
        i -= 1; i + 1
      }
    }
    url
  }

  private def reallyEscapeURL(url: CharSequence): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(url.length + 20)
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
              new AttributeInfo(
                att.getNodeName,
                att.getType,
                escapeURL(value, normalize = true, getConfiguration).toString,
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
