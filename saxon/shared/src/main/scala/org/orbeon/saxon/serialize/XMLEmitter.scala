package org.orbeon.saxon.serialize

import java.{lang => jl, util => ju}

import javax.xml.transform.OutputKeys
import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.lib.SaxonOutputKeys
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.XMLEmitter._
import org.orbeon.saxon.serialize.charcode.{UTF16CharacterSet, UTF8CharacterSet}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.tiny.{CharSlice, CompressedWhitespace}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Whitespace

import scala.util.control.Breaks._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object XMLEmitter {

  val specialInText     : Array[Boolean] = new Array[Boolean](128)
  val specialInAtt      : Array[Boolean] = new Array[Boolean](128)

  locally {
    var i = 0
    while (i <= 31) {
      specialInText(i) = true
      i += 1
    }
  }

  locally {
    var i = 32
    while (i <= 127) {
      specialInText(i) = false
      i += 1
    }
  }

  specialInText('\n') = false
  specialInText('\t') = false
  specialInText('\r') = true
  specialInText('<')  = true
  specialInText('>')  = true
  specialInText('&')  = true

  locally {
    var i = 0
    while (i <= 31) {
      specialInAtt(i) = true
      i += 1
    }
  }

  locally {
    var i = 32
    while (i <= 127) {
      specialInAtt(i) = false
      i += 1
    }
  }
  specialInAtt(0.toChar)   = true

  specialInAtt('\r')       = true
  specialInAtt('\n')       = true
  specialInAtt('\t')       = true
  specialInAtt('<')        = true
  specialInAtt('>')        = true
  specialInAtt('&')        = true
  specialInAtt('\"')       = true

  val specialInAttSingle: Array[Boolean] = ju.Arrays.copyOf(specialInAtt, 128)
  specialInAttSingle('\"') = false
  specialInAttSingle('\'') = true
}

class XMLEmitter extends Emitter {

  var canonical: Boolean = false
  var started: Boolean = false
  var startedElement: Boolean = false
  var openStartTag: Boolean = false
  var declarationIsWritten: Boolean = false
  var elementCode: NodeName = _
  var indentForNextAttribute: Int = -1
  var undeclareNamespaces: Boolean = false
  var unfailing: Boolean = false
  var delimiter: Char = '"'
  var attSpecials: Array[Boolean] = specialInAtt
  var elementStack: List[String] = Nil

  private var indenting: Boolean = false

  private var indentChars: String =
    "\n                                                          "

  private var requireWellFormed: Boolean = false

  var characterReferenceGenerator: CharacterReferenceGenerator =
    HexCharacterReferenceGenerator.THE_INSTANCE

  def setCharacterReferenceGenerator(generator: CharacterReferenceGenerator): Unit =
    this.characterReferenceGenerator = generator

  def setEscapeNonAscii(escape: jl.Boolean): Unit = ()

  override def open(): Unit = ()

  def startDocument(properties: Int): Unit = ()

  def endDocument(): Unit = ()

  def openDocument(): Unit = {

    if (writer == null)
      makeWriter()

    if (characterSet == null)
      characterSet = UTF8CharacterSet.getInstance

    if (outputProperties == null)
      outputProperties = new ju.Properties

    undeclareNamespaces = "yes" == outputProperties.getProperty(SaxonOutputKeys.UNDECLARE_PREFIXES)
    canonical = "yes" == outputProperties.getProperty(SaxonOutputKeys.CANONICAL)
    unfailing = "yes" == outputProperties.getProperty(SaxonOutputKeys.UNFAILING)
    if ("yes" == outputProperties.getProperty(SaxonOutputKeys.SINGLE_QUOTES)) {
      delimiter = '\''
      attSpecials = specialInAttSingle
    }
    writeDeclaration()
  }

  def writeDeclaration(): Unit = {

    if (declarationIsWritten)
      return

    declarationIsWritten = true
    indenting = "yes" == outputProperties.getProperty(OutputKeys.INDENT)

    val byteOrderMark = outputProperties.getProperty(SaxonOutputKeys.BYTE_ORDER_MARK)

    var encoding = outputProperties.getProperty(OutputKeys.ENCODING)
    if (encoding == null || encoding.equalsIgnoreCase("utf8") || canonical)
      encoding = "UTF-8"

    if ("yes" == byteOrderMark && !canonical &&
      ("UTF-8".equalsIgnoreCase(encoding) || "UTF-16LE".equalsIgnoreCase(encoding) ||
        "UTF-16BE".equalsIgnoreCase(encoding))) {
      writer.write('﻿')
    }

    var omitXMLDeclaration = outputProperties.getProperty(OutputKeys.OMIT_XML_DECLARATION)
    if (omitXMLDeclaration == null)
      omitXMLDeclaration = "no"

    if (canonical)
      omitXMLDeclaration = "yes"

    var version = outputProperties.getProperty(OutputKeys.VERSION)
    if (version == null) {
      version =
        if (getConfiguration.getXMLVersion == Configuration.XML10)
          "1.0"
        else
          "1.1"
    } else {
      if (version != "1.0" && version != "1.1") {
        if (unfailing) {
          version = "1.0"
        } else {
          val err = new XPathException("XML version must be 1.0 or 1.1")
          err.setErrorCode("SESU0013")
          throw err
        }
      }
      if (version != "1.0" && omitXMLDeclaration == "yes" &&
        outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM) != null) {
        if (! unfailing) {
          val err = new XPathException("Values of 'version', 'omit-xml-declaration', and 'doctype-system' conflict")
          err.setErrorCode("SEPM0009")
          throw err
        }
      }
    }
    val undeclare = outputProperties.getProperty(SaxonOutputKeys.UNDECLARE_PREFIXES)
    if ("yes" == undeclare)
      undeclareNamespaces = true

    if (version == "1.0" && undeclareNamespaces) {
      if (unfailing) {
        undeclareNamespaces = false
      } else {
        val err = new XPathException("Cannot undeclare namespaces with XML version 1.0")
        err.setErrorCode("SEPM0010")
        throw err
      }
    }
    var standalone = outputProperties.getProperty(OutputKeys.STANDALONE)
    if ("omit" == standalone)
      standalone = null

    if (standalone != null) {
      requireWellFormed = true
      if (omitXMLDeclaration.==("yes") && !unfailing) {
        val err = new XPathException("Values of 'standalone' and 'omit-xml-declaration' conflict")
        err.setErrorCode("SEPM0009")
        throw err
      }
    }
    val systemId = outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM)
    if (systemId != null && "" != systemId)
      requireWellFormed = true

    if (omitXMLDeclaration =="no") {
      writer.write(
        "<?xml version=\"" + version + "\" " + "encoding=\"" +
          encoding +
          '\"' +
          (if (standalone != null) " standalone=\"" + standalone + '\"'
          else "") +
          "?>")
    }
  }

  def writeDocType(name: NodeName,
                   displayName: String,
                   systemId: String,
                   publicId: String): Unit = {
    if (! canonical) {
      if (declarationIsWritten && !indenting)
        writer.write("\n")
      writer.write("<!DOCTYPE " + displayName + '\n')
      var quotedSystemId: String = null
      if (systemId != null)
        quotedSystemId =
          if (systemId.contains("\""))
            "'" + systemId + "'"
          else
            "\"" + systemId + "\""
      if (systemId != null && publicId == null)
        writer.write("  SYSTEM " + quotedSystemId + ">\n")
      else if (systemId == null && publicId != null)
        writer.write("  PUBLIC \"" + publicId + "\">\n")
      else
        writer.write("  PUBLIC \"" + publicId + "\" " + quotedSystemId + ">\n")
    }
  }

  override def close(): Unit = {
    if (! started)
      openDocument()
    if (writer != null)
      writer.flush()
    super.close()
  }

  def startElement(
    elemName   : NodeName,
    `type`     : SchemaType,
    attributes : AttributeMap,
    namespaces : NamespaceMap,
    location   : Location,
    properties : Int
  ): Unit = {

    previousAtomic = false
    if (! started) {
      openDocument()
    } else if (requireWellFormed && elementStack.isEmpty && startedElement && ! unfailing) {
      val err = new XPathException(
        "When 'standalone' or 'doctype-system' is specified, " +
          "the document must be well-formed; but this document contains more than one top-level element")
      err.setErrorCode("SEPM0004")
      throw err
    }
    startedElement = true
    val displayName = elemName.getDisplayName
    if (! allCharactersEncodable) {
      val badchar = testCharacters(displayName)
      if (badchar != 0) {
        val err = new XPathException(
          "Element name contains a character (decimal + " + badchar +
            ") not available in the selected encoding")
        err.setErrorCode("SERE0008")
        throw err
      }
    }
    elementStack ::= displayName
    elementCode = elemName
    if (! started) {
      var systemId = outputProperties.getProperty(OutputKeys.DOCTYPE_SYSTEM)
      var publicId = outputProperties.getProperty(OutputKeys.DOCTYPE_PUBLIC)
      if ("" == systemId)
        systemId = null
      if ("" == publicId)
        publicId = null
      if (systemId != null) {
        requireWellFormed = true
        writeDocType(elemName, displayName, systemId, publicId)
      } else if (writeDocTypeWithNullSystemId()) {
        writeDocType(elemName, displayName, null, publicId)
      }
      started = true
    }
    if (openStartTag) {
      closeStartTag()
    }
    writer.write('<')
    writer.write(displayName)
    if (indentForNextAttribute >= 0)
      indentForNextAttribute += displayName.length
    var isFirst = true
    for (ns <- namespaces.asScala) {
      namespace(ns.getPrefix, ns.getURI, isFirst)
      isFirst = false
    }
    for (att <- attributes.iterator.asScala) {
      attribute(att.getNodeName, att.getValue, att.getProperties, isFirst)
      isFirst = false
    }
    openStartTag = true
    indentForNextAttribute = -1
  }

  def writeDocTypeWithNullSystemId(): Boolean = false

  def namespace(nsprefix: String, nsuri: String, isFirst: Boolean): Unit = {
    val sep = if (isFirst) " " else getAttributeIndentString
    if (nsprefix.isEmpty) {
      writer.write(sep)
      writeAttribute(elementCode, "xmlns", nsuri, ReceiverOption.NONE)
    } else if (nsprefix == "xml") {
      //
    } else {
      val badchar = testCharacters(nsprefix)
      if (badchar != 0) {
        val err = new XPathException(
          "Namespace prefix contains a character (decimal + " +
            badchar +
            ") not available in the selected encoding")
        err.setErrorCode("SERE0008")
        throw err
      }
      if (undeclareNamespaces || nsuri.nonEmpty) {
        writer.write(sep)
        writeAttribute(elementCode,
          "xmlns:" + nsprefix,
          nsuri,
          ReceiverOption.NONE)
      }
    }
  }

  def setIndentForNextAttribute(indent: Int): Unit =
    indentForNextAttribute = indent

  private def attribute(
    nameCode   : NodeName,
    value      : CharSequence,
    properties : Int,
    isFirst    : Boolean
  ): Unit = {
    var displayName = nameCode.getDisplayName
    if (! allCharactersEncodable) {
      val badchar = testCharacters(displayName)
      if (badchar != 0) {
        if (unfailing) {
          displayName = convertToAscii(displayName)
        } else {
          val err = new XPathException(
            "Attribute name contains a character (decimal + " + badchar +
              ") not available in the selected encoding")
          err.setErrorCode("SERE0008")
          throw err
        }
      }
    }
    writer.write(if (isFirst) " " else getAttributeIndentString)
    writeAttribute(elementCode, displayName, value, properties)
  }

  def getAttributeIndentString: String =
    if (indentForNextAttribute < 0) {
      " "
    } else {
      val indent = indentForNextAttribute
      while (indent >= indentChars.length)
        indentChars += "                     "
      indentChars.substring(0, indent)
    }

  def closeStartTag(): Unit =
    if (openStartTag) {
      writer.write('>')
      openStartTag = false
    }

  def emptyElementTagCloser(displayName: String, nameCode: NodeName): String =
    if (canonical) "></" + displayName + ">" else "/>"

  def writeAttribute(
    elCode     : NodeName,
    attname    : String,
    value      : CharSequence,
    properties : Int
  ): Unit = {

    val `val` = value.toString
    writer.write(attname)
    if (ReceiverOption.contains(properties, ReceiverOption.NO_SPECIAL_CHARS)) {
      writer.write('=')
      writer.write(delimiter)
      writer.write(`val`)
      writer.write(delimiter)
    } else if (ReceiverOption.contains(properties, ReceiverOption.USE_NULL_MARKERS)) {
      writer.write('=')
      val delim =
        if (`val`.indexOf('"') >= 0 && `val`.indexOf('\'') < 0)
          '\''
        else
          delimiter
      writer.write(delim)
      writeEscape(value, inAttribute = true)
      writer.write(delim)
    } else {
      writer.write("=")
      writer.write(delimiter)
      writeEscape(value, inAttribute = true)
      writer.write(delimiter)
    }
  }

  def testCharacters(chars: CharSequence): Int = {
    var i = 0
    while (i < chars.length) {
      val c = chars.charAt(i)
      if (c > 127) {
        if (UTF16CharacterSet.isHighSurrogate(c)) {
          i += 1
          val cc = UTF16CharacterSet.combinePair(c, chars.charAt(i))
          if (! characterSet.inCharset(cc)) {
            return cc
          }
        } else if (!characterSet.inCharset(c)) {
          return c
        }
      }
      i += 1
    }
    0
  }

  def convertToAscii(chars: CharSequence): String = {
    val buff = new FastStringBuffer(chars.length)
    for (i <- 0 until chars.length) {
      val c = chars.charAt(i)
      if (c >= 20 && c < 127)
        buff.cat(c)
      else
        buff.append("_" + c.toInt + "_")
    }
    buff.toString
  }

  def endElement(): Unit = {
    val displayName = elementStack.head
    elementStack = elementStack.tail
    if (openStartTag) {
      writer.write(emptyElementTagCloser(displayName, elementCode))
      openStartTag = false
    } else {
      writer.write("</")
      writer.write(displayName)
      writer.write('>')
    }
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    if (! started)
      openDocument()
    if (requireWellFormed && elementStack.isEmpty && !Whitespace.isWhite(chars) && !unfailing) {
      val err = new XPathException(
        "When 'standalone' or 'doctype-system' is specified, " +
          "the document must be well-formed; but this document contains a top-level text node")
      err.setErrorCode("SEPM0004")
      throw err
    }
    if (openStartTag)
      closeStartTag()
    if (ReceiverOption.contains(properties, ReceiverOption.NO_SPECIAL_CHARS)) {
      writeCharSequence(chars)
    } else if (!ReceiverOption.contains(properties,
      ReceiverOption.DISABLE_ESCAPING)) {
      writeEscape(chars, inAttribute = false)
    } else {
      if (testCharacters(chars) == 0) {
        if (!ReceiverOption.contains(properties,
          ReceiverOption.USE_NULL_MARKERS)) {
          writeCharSequence(chars)
        } else {
          val len = chars.length
          for (i <- 0 until len) {
            val c: Char = chars.charAt(i)
            if (c != 0) {
              writer.write(c)
            }
          }
        }
      } else {
        val len = chars.length
        var i = 0
        while (i < len) {
          val c = chars.charAt(i)
          if (c != 0) {
            if (c > 127 && UTF16CharacterSet.isHighSurrogate(c)) {
              val pair = Array.ofDim[Char](2)
              pair(0) = c
              i += 1
              pair(1) = chars.charAt(i)
              val cc = UTF16CharacterSet.combinePair(c, pair(1))
              if (!characterSet.inCharset(cc))
                writeEscape(new CharSlice(pair), inAttribute = false)
              else
                writeCharSequence(new CharSlice(pair))
            } else {
              val ca = Array(c)
              if (! characterSet.inCharset(c))
                writeEscape(new CharSlice(ca), inAttribute = false)
              else
                writeCharSequence(new CharSlice(ca))
            }
          }
          i += 1
        }
      }
    }
  }

  def writeCharSequence(s: CharSequence): Unit =
    s match {
      case str: String =>
        writer.write(str)
      case slice: CharSlice =>
        slice.write(writer)
      case buffer: FastStringBuffer =>
        buffer.write(writer)
      case whitespace: CompressedWhitespace =>
        whitespace.write(writer)
      case _ =>
        writer.write(s.toString)
    }

  /**
   * Handle a processing instruction.
   */
  @throws[XPathException]
  def processingInstruction(
    target     : String,
    data       : CharSequence,
    locationId : Location,
    properties : Int
  ): Unit = {

    if (!started)
      openDocument()

    var targetVar: String = target
    var dataVar: CharSequence = data
    var x: Int = testCharacters(targetVar)
    if (x != 0) {
      if (unfailing) {
        targetVar = convertToAscii(targetVar)
      } else {
        val err = new XPathException(
          "Character in processing instruction name cannot be represented " +
            "in the selected encoding (code " +
            x +
            ')')
        err.setErrorCode("SERE0008")
        throw err
      }
    }
    x = testCharacters(dataVar)
    if (x != 0) {
      if (unfailing) {
        dataVar = convertToAscii(dataVar)
      } else {
        val err = new XPathException(
          "Character in processing instruction data cannot be represented " +
            "in the selected encoding (code " +
            x +
            ')')
        err.setErrorCode("SERE0008")
        throw err
      }
    }

    if (openStartTag)
      closeStartTag()

    writer.write("<?" + targetVar + (if (dataVar.length > 0) " " + dataVar.toString else "") + "?>")
  }

  /**
   * Write contents of array to current writer, after escaping special characters.
   * This method converts the XML special characters (such as &lt; and &amp;) into their
   * predefined entities.
   *
   * @param chars       The character sequence containing the string
   * @param inAttribute Set to true if the text is in an attribute value
   */
  @throws[java.io.IOException]
  @throws[XPathException]
  def writeEscape(chars: CharSequence, inAttribute: Boolean): Unit = {

    var segstart = 0
    var disabled = false
    val specialChars =
      if (inAttribute)
        attSpecials
      else
        XMLEmitter.specialInText

    chars match {
      case whitespace: CompressedWhitespace =>
        whitespace.writeEscape(specialChars, writer)
        return
      case _ =>
    }

    val clength = chars.length
    while (segstart < clength) {
      var i = segstart
      // find a maximal sequence of "ordinary" characters
      breakable {
        while (i < clength) {
          val c = chars.charAt(i)
          if (c < 127)
            if (specialChars(c))
              break()
            else
              i += 1
          else if (c < 160)
            break()
          else if (c == 0x2028)
            break()
          else if (UTF16CharacterSet.isHighSurrogate(c))
            break()
          else if (! characterSet.inCharset(c))
            break()
          else i += 1
        }
      }

      // if this was the whole string write it out and exit
      if (i >= clength) {
        if (segstart == 0)
          writeCharSequence(chars)
        else
          writeCharSequence(chars.subSequence(segstart, i))
        return
      }

      // otherwise write out this sequence
      if (i > segstart)
        writeCharSequence(chars.subSequence(segstart, i))

      // examine the special character that interrupted the scan
      val c = chars.charAt(i)
      if (c == 0) {
        // used to switch escaping on and off
        disabled = ! disabled
      } else if (disabled) {
        if (c > 127)
          if (UTF16CharacterSet.isHighSurrogate(c)) {
            val cc = UTF16CharacterSet.combinePair(c, chars.charAt(i + 1))
            if (! characterSet.inCharset(cc)) {
              val de = new XPathException("Character x" + Integer.toHexString(cc) + " is not available in the chosen encoding")
              de.setErrorCode("SERE0008")
              throw de
            }
          } else if (! characterSet.inCharset(c)) {
            val de = new XPathException("Character " + c + " (x" + Integer.toHexString(c.toInt) + ") is not available in the chosen encoding")
            de.setErrorCode("SERE0008")
            throw de
          }
        writer.write(c)
      } else if (c < 127) {
        // process special ASCII characters
        c match {
          case '<' =>
            writer.write("&lt;")
          case '>' =>
            writer.write("&gt;")
          case '&' =>
            writer.write("&amp;")
          case '\"' =>
            writer.write("&#34;")
          case '\'' =>
            writer.write("&#39;")
          case '\n' =>
            writer.write("&#xA;")
          case '\r' =>
            writer.write("&#xD;")
          case '\t' =>
            writer.write("&#x9;")
          case _ =>
            // C0 control characters
            characterReferenceGenerator.outputCharacterReference(c, writer)
        }
      } else if (c < 160 || c == 0x2028) {
        // XML 1.1 requires these characters to be written as character references
        characterReferenceGenerator.outputCharacterReference(c, writer)
      } else if (UTF16CharacterSet.isHighSurrogate(c)) {
        i += 1
        val d = chars.charAt(i)
        val charval = UTF16CharacterSet.combinePair(c, d)
        if (characterSet.inCharset(charval)) {
          writer.write(c)
          writer.write(d)
        }
        else characterReferenceGenerator.outputCharacterReference(charval, writer)
      } else {
        // process characters not available in the current encoding
        characterReferenceGenerator.outputCharacterReference(c, writer)
      }
      i += 1
      segstart = i
    }
  }

  /**
   * Handle a comment.
   */
  @throws[XPathException]
  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {

    if (! started)
      openDocument()

    var charsVar = chars
    val x = testCharacters(charsVar)
    if (x != 0) {
      if (unfailing) {
        charsVar = convertToAscii(charsVar)
      } else {
        val err = new XPathException(
          "Character in comment cannot be represented " + "in the selected encoding (code " +
            x +
            ')')
        err.setErrorCode("SERE0008")
        throw err
      }
    }
    if (openStartTag) {
      closeStartTag()
    }
    writer.write("<!--")
    writer.write(charsVar.toString)
    writer.write("-->")
  }

  /**
   * Ask whether this Receiver (or the downstream pipeline) makes any use of the type annotations
   * supplied on element and attribute events
   *
   * @return true if the Receiver makes any use of this information. If false, the caller
   *         may supply untyped nodes instead of supplying the type annotation
   */
  override def usesTypeAnnotations = false

  /**
   * Ask whether anything has yet been written
   *
   * @return true if content has been output
   */
  def isStarted: Boolean = started
}