package net.sf.saxon.serialize

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import java.io.IOException

import java.util.Collections

import java.util.HashSet

import java.util.Set

import XHTML5Emitter._

object XHTML5Emitter {

  private var html5ElementNames: Array[String] = Array(
    "a",
    "abbr",
    "address",
    "area",
    "article",
    "aside",
    "audio",
    "b",
    "base",
    "bdi",
    "bdo",
    "blockquote",
    "body",
    "br",
    "button",
    "canvas",
    "caption",
    "cite",
    "code",
    "col",
    "colgroup",
    "datalist",
    "dd",
    "del",
    "details",
    "dfn",
    "dialog",
    "div",
    "dl",
    "dt",
    "em",
    "embed",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "head",
    "header",
    "hgroup",
    "hr",
    "html",
    "i",
    "iframe",
    "img",
    "input",
    "ins",
    "kbd",
    "keygen",
    "label",
    "legend",
    "li",
    "link",
    "map",
    "mark",
    "menu",
    "meta",
    "meter",
    "nav",
    "noscript",
    "object",
    "ol",
    "optgroup",
    "option",
    "output",
    "p",
    "param",
    "pre",
    "progress",
    "q",
    "rp",
    "rt",
    "ruby",
    "s",
    "samp",
    "script",
    "section",
    "select",
    "small",
    "source",
    "span",
    "strong",
    "style",
    "sub",
    "summary",
    "sup",
    "table",
    "tbody",
    "td",
    "textarea",
    "tfoot",
    "th",
    "thead",
    "time",
    "title",
    "tr",
    "track",
    "u",
    "ul",
    "var",
    "video",
    "wbr"
  )

  var html5Elements: Set[String] = new HashSet[String](128)

  var emptyTags5: Set[String] = new HashSet[String](31)

  private var emptyTagNames5: Array[String] = Array("area",
    "base",
    "br",
    "col",
    "embed",
    "hr",
    "img",
    "input",
    "keygen",
    "link",
    "meta",
    "param",
    "source",
    "track",
    "wbr")

  Collections.addAll(emptyTags5, emptyTagNames5:_*)

  Collections.addAll(html5Elements, html5ElementNames:_*)

}

class XHTML5Emitter extends XMLEmitter {

  private def isRecognizedHtmlElement(name: NodeName): Boolean =
    name.hasURI(NamespaceConstant.XHTML) ||
      name.hasURI("") &&
        html5Elements.contains(name.getLocalPart.toLowerCase())

   override def writeDocType(name: NodeName,
                                      displayName: String,
                                      systemId: String,
                                      publicId: String): Unit = {
    if (systemId == null && isRecognizedHtmlElement(name) && name.getLocalPart
      .toLowerCase()
      .==("html")) {
      writer.write("<!DOCTYPE " + displayName + ">")
    } else if (systemId != null) {
      super.writeDocType(name, displayName, systemId, publicId)
    }
  }

  override  def writeDocTypeWithNullSystemId(): Boolean = true

  override  def emptyElementTagCloser(displayName: String,
                                               name: NodeName): String =
    if (isRecognizedHtmlElement(name) && emptyTags5.contains(
      name.getLocalPart)) {
      "/>"
    } else {
      "></" + displayName + '>'
    }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (!started && Whitespace.isWhite(chars)) {} else {
      super.characters(chars, locationId, properties)
    }
  }

}
