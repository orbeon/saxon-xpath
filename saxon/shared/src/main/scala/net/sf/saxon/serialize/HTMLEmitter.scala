////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize

import java.io.IOException

import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.lib.SaxonOutputKeys
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.AttributeMap
import net.sf.saxon.om.NamespaceMap
import net.sf.saxon.om.NodeName
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.tiny.CompressedWhitespace
import javax.xml.transform.OutputKeys
import HTMLEmitter._
import XMLEmitter._
import scala.collection.mutable

/**
 * This class generates HTML output
 */
object HTMLEmitter {
  /**
   * Preferred character representations
   */
  private val REP_NATIVE = 0
  private val REP_ENTITY = 1
  private val REP_DECIMAL = 2
  private val REP_HEX = 3

  /**
   * Decode preferred representation
   *
   * @param rep string containing preferred representation (native, entity, decimal, or hex)
   * @return integer code for the preferred representation
   */
  private def representationCode(rep: String): Int = {
    rep.toLowerCase match {
      case "native" =>
        REP_NATIVE
      case "entity" =>
        REP_ENTITY
      case "decimal" =>
        REP_DECIMAL
      case "hex" =>
        REP_HEX
      case _ =>
        REP_ENTITY
    }
  }

  /**
   * Table of HTML tags that have no closing tag
   */
  private[serialize] val emptyTags = new HTMLTagHashSet(31)

  def setEmptyTag(tag: String): Unit = emptyTags.add(tag)
  def isEmptyTag(tag: String): Boolean = emptyTags.contains(tag)

  /**
   * Table of boolean attributes
   */
  // we use two HashMaps to avoid unnecessary string concatenations
  // Sizes must be large enough: this hash set cannot grow beyond the initial size
  private val booleanAttributes = new HTMLTagHashSet(43)
  private val booleanCombinations = new HTMLTagHashSet(57)

  private def setBooleanAttribute(element: String, attribute: String): Unit = {
    booleanAttributes.add(attribute)
    booleanCombinations.add(element + '+' + attribute)
  }

  private def isBooleanAttribute(element: String, attribute: String, value: String) =
    attribute.equalsIgnoreCase(value)       &&
      booleanAttributes.contains(attribute) &&
      (booleanCombinations.contains(element + '+' + attribute) || booleanCombinations.contains("*+" + attribute))

  setBooleanAttribute("*", "hidden") // HTML5
  setBooleanAttribute("area", "nohref")
  setBooleanAttribute("audio", "autoplay")
  setBooleanAttribute("audio", "controls")
  setBooleanAttribute("audio", "loop")
  setBooleanAttribute("audio", "muted")
  setBooleanAttribute("button", "disabled")
  setBooleanAttribute("button", "autofocus")
  setBooleanAttribute("button", "formnovalidate") //HTML5
  setBooleanAttribute("details", "open")
  setBooleanAttribute("dialog", "open")
  setBooleanAttribute("dir", "compact")
  setBooleanAttribute("dl", "compact")
  setBooleanAttribute("fieldset", "disabled")
  setBooleanAttribute("form", "novalidate")
  setBooleanAttribute("frame", "noresize")
  setBooleanAttribute("hr", "noshade")
  setBooleanAttribute("img", "ismap")
  setBooleanAttribute("input", "checked")
  setBooleanAttribute("input", "disabled")
  setBooleanAttribute("input", "multiple")
  setBooleanAttribute("input", "readonly")
  setBooleanAttribute("input", "required")
  setBooleanAttribute("input", "autofocus")
  setBooleanAttribute("input", "formnovalidate")
  setBooleanAttribute("iframe", "seamless")
  setBooleanAttribute("keygen", "autofocus")
  setBooleanAttribute("keygen", "disabled")
  setBooleanAttribute("menu", "compact")
  setBooleanAttribute("object", "declare")
  setBooleanAttribute("object", "typemustmatch")
  setBooleanAttribute("ol", "compact")
  setBooleanAttribute("ol", "reversed")
  setBooleanAttribute("optgroup", "disabled")
  setBooleanAttribute("option", "selected")
  setBooleanAttribute("option", "disabled")
  setBooleanAttribute("script", "defer")
  setBooleanAttribute("script", "async")
  setBooleanAttribute("select", "multiple")
  setBooleanAttribute("select", "disabled")
  setBooleanAttribute("select", "autofocus")
  setBooleanAttribute("select", "required")
  setBooleanAttribute("style", "scoped")
  setBooleanAttribute("td", "nowrap")
  setBooleanAttribute("textarea", "disabled")
  setBooleanAttribute("textarea", "readonly")
  setBooleanAttribute("textarea", "autofocus")
  setBooleanAttribute("textarea", "required")
  setBooleanAttribute("th", "nowrap")
  setBooleanAttribute("track", "default")
  setBooleanAttribute("ul", "compact")
  setBooleanAttribute("video", "autoplay")
  setBooleanAttribute("video", "controls")
  setBooleanAttribute("video", "loop")
  setBooleanAttribute("video", "muted")
}

abstract class HTMLEmitter extends XMLEmitter {

  private var nonASCIIRepresentation: Int = REP_NATIVE
  private var excludedRepresentation: Int = REP_ENTITY

  private var inScript: Int = _
  var version: Int = 5
  private var parentElement: String = _
  private var uri: String = _
  private var escapeNonAscii: Boolean = false
  private val nodeNameStack: mutable.Stack[NodeName] = new mutable.Stack[NodeName]()

  /**
   * Say that all non-ASCII characters should be escaped, regardless of the character encoding
   *
   * @param escape true if all non ASCII characters should be escaped
   */
  def setEscapeNonAscii(escape: Boolean): Unit = escapeNonAscii = escape

  /**
   * Decide whether an element is "serialized as an HTML element" in the language of the 3.0 specification
   *
   * @return true if the element is to be serialized as an HTML element
   */
   def isHTMLElement(name: NodeName): Boolean

  /**
   * Output start of document
   */
  @throws[XPathException]
  override def open(): Unit = {
  }

  @throws[XPathException]
  override  def openDocument(): Unit = {
    if (writer == null) makeWriter()
    if (started) return
    val byteOrderMark = outputProperties.getProperty(SaxonOutputKeys.BYTE_ORDER_MARK)
    if ("yes" == byteOrderMark && "UTF-8".equalsIgnoreCase(outputProperties.getProperty(OutputKeys.ENCODING))) try writer.write('\uFEFF')
    catch {
      case _: IOException =>
      // Might be an encoding exception; just ignore it
    }
    if ("yes" == outputProperties.getProperty(SaxonOutputKeys.SINGLE_QUOTES)) {
      delimiter = '\''
      attSpecials = XMLEmitter.specialInAttSingle
    }
    inScript = -1000000
  }

  /**
   * Output the document type declaration
   *
   * @param displayName The element name
   * @param systemId    The DOCTYPE system identifier
   * @param publicId    The DOCTYPE public identifier
   */
  @throws[XPathException]
  override  def writeDocType(name: NodeName, displayName: String, systemId: String, publicId: String): Unit =
    super.writeDocType(name, displayName, systemId, publicId)

  /**
   * Output element start tag
   */
  @throws[XPathException]
  override def startElement(elemName: NodeName, `type`: SchemaType, attributes: AttributeMap, namespaces: NamespaceMap, location: Location, properties: Int): Unit = {
    uri = elemName.getURI
    super.startElement(elemName, `type`, attributes, namespaces, location, properties)
    parentElement = elementStack.peek
    if (elemName.hasURI("") && (parentElement.equalsIgnoreCase("script") || parentElement.equalsIgnoreCase("style"))) inScript = 0
    inScript += 1
    nodeNameStack.push(elemName)
  }

  @throws[XPathException]
  def startContentOLD(): Unit = closeStartTag() // prevent <xxx/> syntax
  /**
   * Write attribute name=value pair. Overrides the XML behaviour if the name and value
   * are the same (we assume this is a boolean attribute to be minimised), or if the value is
   * a URL.
   */
  @throws[XPathException]
  override  def writeAttribute(elCode: NodeName, attname: String, value: CharSequence, properties: Int): Unit = try {
    if (uri.isEmpty) if (HTMLEmitter.isBooleanAttribute(elCode.getLocalPart, attname, value.toString)) {
      writer.write(attname)
      return
    }
    super.writeAttribute(elCode, attname, value, properties)
  } catch {
    case err: IOException =>
      throw new XPathException(err)
  }

  /**
   * Escape characters. Overrides the XML behaviour
   */
  @throws[java.io.IOException]
  @throws[XPathException]
  override  def writeEscape(chars: CharSequence, inAttribute: Boolean): Unit = {
    var segstart = 0
    val specialChars = if (inAttribute) attSpecials
    else specialInText
    chars match {
      case whitespace: CompressedWhitespace =>
        whitespace.writeEscape(specialChars, writer)
        return
      case _ =>
    }
    var disabled = false
    while ( {
      segstart < chars.length
    }) {
      var i = segstart
      // find a maximal sequence of "ordinary" characters
      if (escapeNonAscii) {
        var c = 0
        while ( {
          i < chars.length && {
            c = chars.charAt(i)
            c
          } < 127 && !specialChars(c)
        }) i += 1
      }
      else {
        var c = 0
        while ( {
          i < chars.length && {
            c = chars.charAt(i)
            if (c < 127) !specialChars(c)
            else characterSet.inCharset(c)
          } && c > 160

        }) i += 1
      }
      // if this was the whole string, output the string and quit
      if (i == chars.length) {
        if (segstart == 0) writeCharSequence(chars)
        else writeCharSequence(chars.subSequence(segstart, i))
        return
      }
      // otherwise, output this sequence and continue
      if (i > segstart) writeCharSequence(chars.subSequence(segstart, i))
      val c = chars.charAt(i)
      if (c == 0) { // used to switch escaping on and off
        disabled = !disabled
      }
      else if (disabled) writer.write(c)
      else if (c <= 127) { // handle a special ASCII character
        if (inAttribute) if (c == '<') writer.write('<') // not escaped
        else if (c == '>') writer.write("&gt;") // recommended for older browsers
        else if (c == '&') if (i + 1 < chars.length && chars.charAt(i + 1) == '{') writer.write('&') // not escaped if followed by '{'
        else writer.write("&amp;")
        else if (c == '\"') writer.write("&#34;")
        else if (c == '\'') writer.write("&#39;")
        else if (c == '\n') writer.write("&#xA;")
        else if (c == '\t') writer.write("&#x9;")
        else if (c == '\r') writer.write("&#xD;")
        else if (c == '<') writer.write("&lt;")
        else if (c == '>') writer.write("&gt;") // changed to allow for "]]>"
        else if (c == '&') writer.write("&amp;")
        else if (c == '\r') writer.write("&#xD;")
      }
      else if (c < 160) if (rejectControlCharacters) { // these control characters are illegal in HTML
        val err = new XPathException("Illegal HTML character: decimal " + c.toInt)
        err.setErrorCode("SERE0014")
        throw err
      }
      else characterReferenceGenerator.outputCharacterReference(c, writer)
      else if (c == 160) { // always output NBSP as an entity reference
        writer.write("&nbsp;")
      }
      else if (c >= 55296 && c <= 56319) { //handle surrogate pair
        //A surrogate pair is two consecutive Unicode characters.  The first
        //is in the range D800 to DBFF, the second is in the range DC00 to DFFF.
        //To compute the numeric value of the character corresponding to a surrogate
        //pair, use this formula (all numbers are hex):
        //(FirstChar - D800) * 400 + (SecondChar - DC00) + 10000
        // we'll trust the data to be sound
        val charval = ((c.toInt - 55296) * 1024) + (chars.charAt(i + 1).toInt - 56320) + 65536
        characterReferenceGenerator.outputCharacterReference(charval, writer)
        i += 1
      }
      else if (escapeNonAscii || !characterSet.inCharset(c)) characterReferenceGenerator.outputCharacterReference(c, writer)
      else writer.write(c)
      segstart = {
        i += 1
        i
      }
    }
  }

  /**
   * Ask whether control characters should be rejected: true for HTML4, false for HTML5
   *
   * @return true if control characters should be rejected
   */
   def rejectControlCharacters: Boolean

  /**
   * Close an empty element tag. (This is overridden in XHTMLEmitter).
   *
   * @param displayName the name of the empty element
   * @param nameCode    the fingerprint of the name of the empty element
   * @return the string used to close an empty element tag.
   */
  override  def emptyElementTagCloser(displayName: String, nameCode: NodeName): String = "></" + displayName + ">"

  /**
   * Output an element end tag.
   */
  @throws[XPathException]
  override def endElement(): Unit = {
    val nodeName = nodeNameStack.pop
    val name = elementStack.peek
    inScript -= 1
    if (inScript == 0) inScript = -1000000
    if (HTMLEmitter.isEmptyTag(name) && isHTMLElement(nodeName)) { // no end tag required
      elementStack.pop
    }
    else super.endElement()
  }

  /**
   * Character data.
   */
  @throws[XPathException]
  override def characters(chars: CharSequence, locationId: Location, properties: Int): Unit = {
    var props = properties
    if (inScript > 0)
      props = props | ReceiverOption.DISABLE_ESCAPING
    super.characters(chars, locationId, props)
  }

  /**
   * Handle a processing instruction.
   */
  @throws[XPathException]
  override def processingInstruction(target: String, data: CharSequence, locationId: Location, properties: Int): Unit = {
    if (!started) openDocument()
    for (i <- 0 until data.length) {
      if (data.charAt(i) == '>') {
        val err = new XPathException("A processing instruction in HTML must not contain a > character")
        err.setErrorCode("SERE0015")
        throw err
      }
    }
    try {
      if (openStartTag) closeStartTag()
      writer.write("<?")
      writer.write(target)
      writer.write(' ')
      writeCharSequence(data)
      writer.write('>')
    } catch {
      case err: IOException =>
        throw new XPathException(err)
    }
  }
}