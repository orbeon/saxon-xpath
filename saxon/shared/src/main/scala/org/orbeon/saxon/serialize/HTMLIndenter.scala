package org.orbeon.saxon.serialize

import java.util

import org.orbeon.saxon.event.ProxyReceiver
import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.SaxonOutputKeys
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.AttributeMap
import org.orbeon.saxon.om.FingerprintedQName
import org.orbeon.saxon.om.NamespaceMap
import org.orbeon.saxon.om.NodeName
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.tiny.CharSlice
import java.util._

import HTMLIndenter._

object HTMLIndenter {

  private val formattedTags: Array[String] =
    Array("pre", "script", "style", "textarea", "title", "xmp")

  private val inlineTags: Array[String] = Array(
    "a",
    "abbr",
    "acronym",
    "applet",
    "area",
    "audio",
    "b",
    "basefont",
    "bdi",
    "bdo",
    "big",
    "br",
    "button",
    "canvas",
    "cite",
    "code",
    "data",
    "datalist",
    "del",
    "dfn",
    "em",
    "embed",
    "font",
    "i",
    "iframe",
    "img",
    "input",
    "ins",
    "kbd",
    "label",
    "map",
    "mark",
    "math",
    "meter",
    "noscript",
    "object",
    "output",
    "picture",
    "progress",
    "q",
    "ruby",
    "s",
    "samp",
    "script",
    "select",
    "small",
    "span",
    "strike",
    "strong",
    "sub",
    "sup",
    "svg",
    "template",
    "textarea",
    "time",
    "tt",
    "u",
    "var",
    "video",
    "wbr"
  )

  private val inlineTable: Set[String] = new HashSet(70)

  private val formattedTable: Set[String] = new HashSet(10)

  Collections.addAll(inlineTable, inlineTags: _*)

  Collections.addAll(formattedTable, formattedTags: _*)

  private val IS_INLINE: Int = 1

  private val IS_FORMATTED: Int = 2

  private val IS_SUPPRESSED: Int = 4

}

class HTMLIndenter(next: Receiver, method: String) extends ProxyReceiver(next) {

   var indentChars: Array[Char] = Array('\n', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

  private var level: Int = 0

  private var sameLine: Boolean = false

  private var inFormattedTag: Boolean = false

  private var afterInline: Boolean = false

  private var afterEndElement: Boolean = false

  private var propertyStack: Array[Int] = new Array[Int](20)

  private var suppressed: Set[String] = null

  def setOutputProperties(props: Properties): Unit = {
    val s: String = props.getProperty(SaxonOutputKeys.SUPPRESS_INDENTATION)
    if (s != null) {
      suppressed = new HashSet(8)
      val st: StringTokenizer = new StringTokenizer(s, " \t\r\n")
      while (st.hasMoreTokens()) {
        val eqName: String = st.nextToken()
        suppressed.add(
          FingerprintedQName.fromEQName(eqName).getLocalPart.toLowerCase())
      }
    }
  }

  def classifyTag(name: NodeName): Int = {
    var r: Int = 0
    if (inlineTable.contains(name.getLocalPart.toLowerCase())) {
      r |= IS_INLINE
    }
    if (formattedTable.contains(name.getLocalPart.toLowerCase())) {
      r |= IS_FORMATTED
    }
    if (suppressed != null &&
      suppressed.contains(name.getLocalPart.toLowerCase())) {
      r |= IS_SUPPRESSED
    }
    r
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    val withinSuppressed: Int =
      if (level == 0) 0 else (propertyStack(level - 1) & IS_SUPPRESSED)
    val tagProps: Int = classifyTag(elemName) | withinSuppressed
    if (level >= propertyStack.length) {
      propertyStack = Arrays.copyOf(propertyStack, level * 2)
    }
    propertyStack(level) = tagProps
    val inlineTag: Boolean = (tagProps & IS_INLINE) != 0
    if (!inlineTag && !inFormattedTag && !afterInline && withinSuppressed == 0 &&
      level != 0) {
      indent()
    }
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    inFormattedTag = inFormattedTag || ((tagProps & IS_FORMATTED) != 0)
    level += 1
    sameLine = true
    afterInline = false
    afterEndElement = false
  }

  override def endElement(): Unit = {
    level -= 1
    val thisInline: Boolean = (propertyStack(level) & IS_INLINE) != 0
    val thisFormatted: Boolean = (propertyStack(level) & IS_FORMATTED) != 0
    val thisSuppressed: Boolean = (propertyStack(level) & IS_SUPPRESSED) != 0
    if (afterEndElement && !thisInline && !thisSuppressed && !afterInline &&
      !sameLine &&
      !inFormattedTag) {
      indent()
      afterInline = false
    } else {
      afterInline = thisInline
    }
    nextReceiver.endElement()
    inFormattedTag = inFormattedTag && !thisFormatted
    sameLine = false
    afterEndElement = true
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (inFormattedTag ||
      ReceiverOption.contains(properties, ReceiverOption.USE_NULL_MARKERS) ||
      ReceiverOption.contains(properties, ReceiverOption.DISABLE_ESCAPING)) {
      nextReceiver.characters(chars, locationId, properties)
    } else {
      var lastNL: Int = 0
      for (i <- 0 until chars.length if chars.charAt(i) == '\n' ||
        (i - lastNL > getLineLength && chars.charAt(i) == ' ')) {
        sameLine = false
        nextReceiver.characters(chars.subSequence(lastNL, i),
          locationId,
          properties)
        indent()
        lastNL = i + 1
        while (lastNL < chars.length && chars.charAt(lastNL) == ' ') {
          lastNL += 1;
          lastNL - 1
        }
      }
      if (lastNL < chars.length) {
        nextReceiver.characters(chars.subSequence(lastNL, chars.length),
          locationId,
          properties)
      }
    }
    afterInline = false
    afterEndElement = false
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (afterEndElement && level != 0 && (propertyStack(level - 1) & IS_INLINE) == 0) {
      indent()
    }
    nextReceiver.processingInstruction(target, data, locationId, properties)
    afterEndElement = false
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (afterEndElement && level != 0 && (propertyStack(level - 1) & IS_INLINE) == 0) {
      indent()
    }
    nextReceiver.comment(chars, locationId, properties)
    afterEndElement = false
  }

   def getLineLength: Int = 80

  private def indent(): Unit = {
    val spaces: Int = level * getIndentation
    if (spaces + 1 >= indentChars.length) {
      var increment: Int = 5 * getIndentation
      if (spaces + 1 > indentChars.length + increment) {
        increment += spaces + 1
      }
      val c2: Array[Char] = Array.ofDim[Char](indentChars.length + increment)
      System.arraycopy(indentChars, 0, c2, 0, indentChars.length)
      Arrays.fill(c2, indentChars.length, c2.length, ' ')
      indentChars = c2
    }
    nextReceiver.characters(new CharSlice(indentChars, 0, spaces + 1),
      Loc.NONE,
      ReceiverOption.NONE)
    sameLine = false
  }

   def getIndentation: Int = 3

}
