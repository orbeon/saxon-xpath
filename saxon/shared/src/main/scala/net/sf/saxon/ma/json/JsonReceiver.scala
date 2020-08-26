package net.sf.saxon.ma.json

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.StringConverter

import net.sf.saxon.om.AttributeInfo

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.CharSequenceConsumer

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.DoubleValue

import net.sf.saxon.value.StringToDouble11

import net.sf.saxon.value.Whitespace

import java.util.HashSet

import java.util.Objects

import java.util.Set

import java.util.Stack

import java.util.function.IntPredicate

import JsonReceiver._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object JsonReceiver {

  private val ERR_INPUT: String = "FOJS0006"

  private def handleEscapedString(str: String): CharSequence = {
    unescape(str)
    val out: FastStringBuffer = new FastStringBuffer(str.length * 2)
    var afterEscapeChar: Boolean = false
    for (i <- 0 until str.length) {
      val c: Char = str.charAt(i)
      if (c == '"' && !afterEscapeChar) {
        out.append("\\\"")
      } else if (c < 32 || (c >= 127 && c < 160)) {
        if (c == '\b') {
          out.append("\\b")
        } else if (c == '\f') {
          out.append("\\f")
        } else if (c == '\n') {
          out.append("\\n")
        } else if (c == '\r') {
          out.append("\\r")
        } else if (c == '\t') {
          out.append("\\t")
        } else {
          out.append("\\u")
          var hex: String = java.lang.Integer.toHexString(c).toUpperCase()
          while (hex.length < 4) hex = "0" + hex
          out.append(hex)
        }
      } else if (c == '/' && !afterEscapeChar) {
        out.append("\\/")
      } else {
        out.cat(c)
      }
      afterEscapeChar = c == '\\' && !afterEscapeChar
    }
    out
  }

  def escape(in: CharSequence,
             forXml: Boolean,
             hexEscapes: IntPredicate): CharSequence = {
    val out: FastStringBuffer = new FastStringBuffer(in.length)
    for (i <- 0 until in.length) {
      val c: Char = in.charAt(i)
      c match {
        case '"' => out.append(if (forXml) "\"" else "\\\"")
        case '\b' => out.append("\\b")
        case '\f' => out.append("\\f")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case '/' => out.append(if (forXml) "/" else "\\/")
        case '\\' => out.append("\\\\")
        case _ =>
          if (hexEscapes.test(c)) {
            out.append("\\u")
            var hex: String = java.lang.Integer.toHexString(c).toUpperCase()
            while (hex.length < 4) hex = "0" + hex
            out.append(hex)
          } else {
            out.cat(c)
          }

      }
    }
    out
  }

  private class ControlChar extends IntPredicate {

    def test(c: Int): Boolean = c < 31 || (c >= 127 && c <= 159)

  }

  private def unescape(literal: String): String = {
    if (literal.indexOf('\\') < 0) {
      return literal
    }
    val buffer: FastStringBuffer = new FastStringBuffer(literal.length)
    for (k <- 0 until literal.length) {
      var i = k
      val c: Char = literal.charAt(i)
      if (c == '\\') {
        if ( {
          i += 1
          i
        } == literal.length - 1) {
          throw new XPathException(
            "String '" + Err.wrap(literal) + "' ends in backslash ",
            "FOJS0007")
        }
        literal.charAt(i) match {
          case '"' => buffer.cat('"')
          case '\\' => buffer.cat('\\')
          case '/' => buffer.cat('/')
          case 'b' => buffer.cat('\b')
          case 'f' => buffer.cat('\f')
          case 'n' => buffer.cat('\n')
          case 'r' => buffer.cat('\r')
          case 't' => buffer.cat('\t')
          case 'u' => {
            val hex: String = literal.substring(i + 1, i + 5)
            val code: Int = java.lang.Integer.parseInt(hex, 16)
            buffer.cat(code.toChar)
            i += 4
          }
          case _ =>
            var next: Char = literal.charAt(i)
            var xx: String =
              if (next < 256) next.toString + ""
              else "x" + java.lang.Integer.toHexString(next)
            throw new XPathException("Unknown escape sequence \\" + xx,
              "FOJS0007")

        }
      } else {
        buffer.cat(c)
      }
    }
    buffer.toString
  }

}

class JsonReceiver(var pipeLine: PipelineConfiguration,
                   private var output: CharSequenceConsumer)
  extends Receiver {

  private var pipe: PipelineConfiguration = pipeLine

  private var textBuffer: FastStringBuffer = new FastStringBuffer(128)

  private var stack: Stack[NodeName] = new Stack()

  private var atStart: Boolean = true

  @BooleanBeanProperty
  var indenting: Boolean = false

  private var escaped: Boolean = false

  private var keyChecker: Stack[Set[String]] = new Stack()

  Objects.requireNonNull(pipe)

  Objects.requireNonNull(output)

  this.setPipelineConfiguration(pipe)

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
  }

  def getPipelineConfiguration(): PipelineConfiguration = pipe

  def setSystemId(systemId: String): Unit = ()

  def open(): Unit = {
    output.open()
  }

  def startDocument(properties: Int): Unit = ()

  def endDocument(): Unit = ()

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit = ()

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    val parent: String = if (stack.empty()) null else stack.peek().getLocalPart
    val inMap: Boolean = "map" == parent || stack.isEmpty
    stack.push(elemName)
    if (!elemName.hasURI(NamespaceConstant.FN)) {
      throw new XPathException(
        "xml-to-json: element found in wrong namespace: " + elemName.getStructuredQName.getEQName,
        ERR_INPUT)
    }
    var key: String = null
    var escapedAtt: String = null
    var escapedKey: String = null
    for (att <- attributes) {
      val attName: NodeName = att.getNodeName
      if (attName.hasURI("")) {
        if (attName.getLocalPart.==("key")) {
          if (!inMap) {
            throw new XPathException(
              "xml-to-json: The key attribute is allowed only on elements within a map",
              ERR_INPUT)
          }
          key = att.getValue
        } else if (attName.getLocalPart.==("escaped-key")) {
          if (!inMap) {
            throw new XPathException(
              "xml-to-json: The escaped-key attribute is allowed only on elements within a map",
              ERR_INPUT)
          }
          escapedKey = att.getValue
        } else if (attName.getLocalPart.==("escaped")) {
          val allowed: Boolean = stack.size == 1 || elemName.getLocalPart.==(
            "string")
          if (!allowed) {
            throw new XPathException(
              "xml-to-json: The escaped attribute is allowed only on the <string> element",
              ERR_INPUT)
          }
          escapedAtt = att.getValue
        } else {
          throw new XPathException(
            "xml-to-json: Disallowed attribute in input: " + attName.getDisplayName,
            ERR_INPUT)
        }
      } else if (attName.hasURI(NamespaceConstant.FN)) {
        throw new XPathException(
          "xml-to-json: Disallowed attribute in input: " + attName.getDisplayName,
          ERR_INPUT)
      }
    }
    if (!atStart) {
      output.cat(",")
      if (indenting) {
        indent(stack.size)
      }
    }
    if (inMap && !keyChecker.isEmpty) {
      if (key == null) {
        throw new XPathException(
          "xml-to-json: Child elements of <map> must have a key attribute",
          ERR_INPUT)
      }
      var alreadyEscaped: Boolean = false
      if (escapedKey != null) {
        alreadyEscaped = StringConverter.StringToBoolean.INSTANCE
          .convertString(escapedKey)
          .asAtomic()
          .effectiveBooleanValue()
      }
      key = (if (alreadyEscaped) handleEscapedString(key)
      else escape(key, forXml = false, new ControlChar())).toString
      val normalizedKey: String = if (alreadyEscaped) unescape(key) else key
      val added: Boolean = keyChecker.peek().add(normalizedKey)
      if (!added) {
        throw new XPathException(
          "xml-to-json: duplicate key value " + Err.wrap(key),
          ERR_INPUT)
      }
      output.cat("\"").cat(key).cat("\"").cat(if (indenting) " : " else ":")
    }
    val local: String = elemName.getLocalPart
    local match {
      case "array" =>
        if (indenting) {
          indent(stack.size)
          output.cat("[ ")
        } else {
          output.cat("[")
        }
        atStart = true
      case "map" =>
        if (indenting) {
          indent(stack.size)
          output.cat("{ ")
        } else {
          output.cat("{")
        }
        atStart = true
        keyChecker.push(new HashSet[String]())
      case "null" =>
        checkParent(local, parent)
        output.cat("null")
        atStart = false
      case "string" =>
        if (escapedAtt != null) {
          escaped = StringConverter.StringToBoolean.INSTANCE
            .convertString(escapedAtt)
            .asAtomic()
            .effectiveBooleanValue()
        }
        checkParent(local, parent)
        atStart = false
      case "boolean" | "number" =>
        checkParent(local, parent)
        atStart = false
      case _ =>
        throw new XPathException(
          "xml-to-json: unknown element <" + local + ">",
          ERR_INPUT)

    }
    textBuffer.setLength(0)
  }

  private def checkParent(child: String, parent: String): Unit = {
    if ("null" == parent || "string" == parent || "number" == parent ||
      "boolean" == parent) {
      throw new XPathException(
        "xml-to-json: A " + Err
          .wrap(child, Err.ELEMENT) + " element cannot appear as a child of " +
          Err.wrap(parent, Err.ELEMENT),
        ERR_INPUT)
    }
  }

  def endElement(): Unit = {
    val name: NodeName = stack.pop()
    val local: String = name.getLocalPart
    if (local.==("boolean")) {
      val b: Boolean = StringConverter.StringToBoolean.INSTANCE
        .convertString(textBuffer)
        .asAtomic()
        .effectiveBooleanValue()
      output.cat(if (b) "true" else "false")
    } else if (local.==("number")) {
      val d: Double = StringToDouble11.getInstance.stringToNumber(textBuffer)
      if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) {
        throw new XPathException(
          "xml-to-json: Infinity and NaN are not allowed",
          ERR_INPUT)
      }
      output.cat(new DoubleValue(d).getStringValueCS)
    } else if (local.==("string")) {
      output.cat("\"")
      val str: String = textBuffer.toString
      if (escaped) {
        output.cat(handleEscapedString(str))
      } else {
        output.cat(escape(str, forXml = false, new ControlChar()))
      }
      output.cat("\"")
    } else if (!Whitespace.isWhite(textBuffer)) {
      throw new XPathException(
        "xml-to-json: Element " + name.getDisplayName + " must have no text content",
        ERR_INPUT)
    }
    textBuffer.setLength(0)
    escaped = false
    if (local.==("array")) {
      output.cat(if (indenting) " ]" else "]")
    } else if (local.==("map")) {
      keyChecker.pop()
      output.cat(if (indenting) " }" else "}")
    }
    atStart = false
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    textBuffer.cat(chars)
  }

  def processingInstruction(name: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = ()

  def comment(content: CharSequence,
              locationId: Location,
              properties: Int): Unit = ()

  def close(): Unit = {
    if (output != null) {
      output.close()
      output = null
    }
  }

  override def usesTypeAnnotations(): Boolean = false

  def getSystemId(): String = null

  private def indent(depth: Int): Unit = {
    output.cat("\n")
    for (i <- 0 until depth) {
      output.cat("  ")
    }
  }

}
