package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.Event

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.lib.SaxonOutputKeys

import org.orbeon.saxon.model.AnyType

import org.orbeon.saxon.model.ComplexType

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Untyped

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.tiny.CharSlice

import org.orbeon.saxon.value.Whitespace

import javax.xml.transform.OutputKeys

import java.util._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class XMLIndenter(var emitter: XMLEmitter) extends ProxyReceiver(emitter) {

  private var level: Int = 0

   var indentChars: Array[Char] = Array('\n', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

  private var sameline: Boolean = false

  private var afterStartTag: Boolean = false

  private var afterEndTag: Boolean = true

  private var pendingWhitespace: Event.Text = null

  private var line: Int = 0

  private var column: Int = 0

  private var suppressedAtLevel: Int = -1

  private var suppressedElements: Set[NodeName] = null

  def setOutputProperties(props: Properties): Unit = {
    val omit: String = props.getProperty(OutputKeys.OMIT_XML_DECLARATION)
    afterEndTag = omit == null || "yes" != Whitespace.trim(omit) ||
      props.getProperty(OutputKeys.DOCTYPE_SYSTEM) != null
    var s: String = props.getProperty(SaxonOutputKeys.SUPPRESS_INDENTATION)
    if (s == null) {
      s = props.getProperty("{http://saxon.sf.net/}suppress-indentation")
    }
    if (s != null) {
      suppressedElements = new HashSet(8)
      val st: StringTokenizer = new StringTokenizer(s, " \t\r\n")
      while (st.hasMoreTokens()) {
        val eqName: String = st.nextToken()
        suppressedElements.add(FingerprintedQName.fromEQName(eqName))
      }
    }
  }

  override def open(): Unit = {
    emitter.open()
  }

  override def startElement(nameCode: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (afterStartTag || afterEndTag) {
      if (isDoubleSpaced(nameCode)) {
        nextReceiver.characters("\n", location, ReceiverOption.NONE)
        line = 0
        column = 0
      }
      indent()
    } else {
      flushPendingWhitespace()
    }
    {
      level += 1;
    }
    if (suppressedAtLevel < 0) {
      val xmlSpace: String =
        attributes.getValue(NamespaceConstant.XML, "space")
      if (xmlSpace != null && xmlSpace.trim().==("preserve")) {
        suppressedAtLevel = level
      }
    }
    sameline = true
    afterStartTag = true
    afterEndTag = false
    line = 0
    if (suppressedElements != null && suppressedAtLevel == -1 &&
      suppressedElements.contains(nameCode)) {
      suppressedAtLevel = level
    }
    if (`type` != AnyType.getInstance && `type` != Untyped.getInstance &&
      suppressedAtLevel < 0 &&
      `type`.isComplexType &&
      `type`.asInstanceOf[ComplexType].isMixedContent) {
      suppressedAtLevel = level
    }
    if (suppressedAtLevel < 0) {
      var len: Int = 0
      for (nbs <- namespaces.asScala; binding <- nbs.asScala) {
        val prefix: String = binding.getPrefix
        if (prefix.isEmpty) {
          len += 9 + binding.getURI.length
        } else {
          len += prefix.length + 10 + binding.getURI.length
        }
      }
      for (att <- attributes) {
        val name: NodeName = att.getNodeName
        val prefix: String = name.getPrefix
        len += name.getLocalPart.length + att.getValue.length + 4 + (if (prefix.isEmpty)
          4
        else
          prefix.length + 5)
      }
      if (len > getLineLength) {
        val indent: Int = (level - 1) * getIndentation + 3
        emitter.setIndentForNextAttribute(indent)
      }
    }
    nextReceiver.startElement(nameCode,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def endElement(): Unit = {
    {
      level -= 1;
    }
    if (afterEndTag && !sameline) {
      indent()
    } else {
      flushPendingWhitespace()
    }
    emitter.endElement()
    sameline = false
    afterEndTag = true
    afterStartTag = false
    line = 0
    if (level == (suppressedAtLevel - 1)) {
      suppressedAtLevel = -1
    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (afterEndTag) {
      indent()
    } else {
      flushPendingWhitespace()
    }
    emitter.processingInstruction(target, data, locationId, properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (suppressedAtLevel < 0 && Whitespace.isWhite(chars)) {
      pendingWhitespace = new Event.Text(chars, locationId, properties)
    } else {
      flushPendingWhitespace()
      for (i <- 0 until chars.length) {
        val c: Char = chars.charAt(i)
        if (c == '\n') {
          sameline = false
          line += 1
          column = 0
        }
        column += 1
      }
      emitter.characters(chars, locationId, properties)
      afterStartTag = false
      afterEndTag = false
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (afterEndTag) {
      indent()
    } else {
      flushPendingWhitespace()
    }
    emitter.comment(chars, locationId, properties)
  }

  override def usesTypeAnnotations: Boolean = true

  private def indent(): Unit = {
    if (suppressedAtLevel >= 0) {
      flushPendingWhitespace()
      return
    }
    pendingWhitespace = null
    var spaces: Int = level * getIndentation
    if (line > 0) {
      spaces -= column
      if (spaces <= 0) {
        return
      }
    }
    if (spaces + 2 >= indentChars.length) {
      var increment: Int = 5 * getIndentation
      if (spaces + 2 > indentChars.length + increment) {
        increment += spaces + 2
      }
      val c2: Array[Char] = Array.ofDim[Char](indentChars.length + increment)
      System.arraycopy(indentChars, 0, c2, 0, indentChars.length)
      Arrays.fill(c2, indentChars.length, c2.length, ' ')
      indentChars = c2
    }
    val start: Int = if (line == 0) 0 else 1
    emitter.characters(new CharSlice(indentChars, start, spaces + 1),
      Loc.NONE,
      ReceiverOption.NO_SPECIAL_CHARS)
    sameline = false
  }

  private def flushPendingWhitespace(): Unit = {
    if (pendingWhitespace != null) {
      pendingWhitespace.replay(nextReceiver)
      pendingWhitespace = null
    }
  }

  override def endDocument(): Unit = {
    if (afterEndTag) {
      emitter.characters("\n", Loc.NONE, ReceiverOption.NONE)
    }
    super.endDocument()
  }

   def getIndentation: Int = 3

   def isDoubleSpaced(name: NodeName): Boolean = false

   def getLineLength: Int = 80

}
