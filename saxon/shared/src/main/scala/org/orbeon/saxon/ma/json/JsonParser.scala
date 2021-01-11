////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.ma.json.JsonParser.JsonToken.JsonToken
import org.orbeon.saxon.model.StringToDouble
import org.orbeon.saxon.om.{NameChecker, Sequence}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{BooleanValue, StringValue}

import java.util
import scala.util.control.Breaks._

/**
 * Parser for JSON, which notifies parsing events to a JsonHandler
 */
object JsonParser {

  val ESCAPE              : Int = 1
  val ALLOW_ANY_TOP_LEVEL : Int = 2
  val LIBERAL             : Int = 4
  val VALIDATE            : Int = 8
  val DEBUG               : Int = 16
  val DUPLICATES_RETAINED : Int = 32
  val DUPLICATES_LAST     : Int = 64
  val DUPLICATES_FIRST    : Int = 128
  val DUPLICATES_REJECTED : Int = 256

  val DUPLICATES_SPECIFIED: Int = DUPLICATES_FIRST | DUPLICATES_LAST | DUPLICATES_RETAINED | DUPLICATES_REJECTED

  private val ERR_GRAMMAR   : String = "FOJS0001"
  private val ERR_DUPLICATE : String = "FOJS0003"
  private val ERR_SCHEMA    : String = "FOJS0004"
  private val ERR_OPTIONS   : String = "FOJS0005"
  private val ERR_LIMITS    : String = "FOJS0001"

  @throws[XPathException]
  def getFlags(options: util.Map[String, Sequence], context: XPathContext, allowValidate: Boolean): Int = {
    var flags = 0
    val debug = options.get("debug").asInstanceOf[BooleanValue]
    if (debug != null && debug.getBooleanValue)
      flags |= DEBUG
    val escape = options.get("escape").asInstanceOf[BooleanValue].getBooleanValue
    if (escape) {
      flags |= ESCAPE
      if (options.get("fallback") != null)
        throw new XPathException("Cannot specify a fallback function when escape=true", "FOJS0005")
    }
    if (options.get("liberal").asInstanceOf[BooleanValue].getBooleanValue) {
      flags |= LIBERAL
      flags |= ALLOW_ANY_TOP_LEVEL
    }
    var validate = false
    if (allowValidate) {
      validate = options.get("validate").asInstanceOf[BooleanValue].getBooleanValue
      if (validate) {
        if (!context.getController.getExecutable.isSchemaAware)
          error("Requiring validation on non-schema-aware processor", ERR_SCHEMA)
        flags |= VALIDATE
      }
    }
    if (options.containsKey("duplicates")) {
      val duplicates = options.get("duplicates").asInstanceOf[StringValue].getStringValue
      duplicates match {
        case "reject" =>
          flags |= DUPLICATES_REJECTED
        case "use-last" =>
          flags |= DUPLICATES_LAST
        case "use-first" =>
          flags |= DUPLICATES_FIRST
        case "retain" =>
          flags |= DUPLICATES_RETAINED
        case _ =>
          error("Invalid value for 'duplicates' option", ERR_OPTIONS)
      }
      if (validate && "retain" == duplicates)
        error("The options validate:true and duplicates:retain cannot be used together", ERR_OPTIONS)
    }
    flags
  }

  /**
   * Unescape a JSON string literal,
   *
   * @param literal   the string literal to be processed
   * @param flags     parsing options
   * @param errorCode Error code
   * @return the result of parsing and conversion to XDM
   */
  @throws[XPathException]
  def unescape(literal: String, flags: Int, errorCode: String, lineNumber: Int): String = {
    if (literal.indexOf('\\') < 0)
      return literal
    val liberal = (flags & LIBERAL) != 0
    val buffer = new FastStringBuffer(literal.length)
    var i = 0
    while (i < literal.length) {
      val c = literal.charAt(i)
      if (c == '\\') {
        if ({
          i += 1
          i - 1
        } == literal.length - 1)
          throw new XPathException("Invalid JSON escape: String " + Err.wrap(literal) + " ends in backslash", errorCode)
        literal.charAt(i) match {
          case '"' =>
            buffer.cat('"')
          case '\\' =>
            buffer.cat('\\')
          case '/' =>
            buffer.cat('/')
          case 'b' =>
            buffer.cat('\b')
          case 'f' =>
            buffer.cat('\f')
          case 'n' =>
            buffer.cat('\n')
          case 'r' =>
            buffer.cat('\r')
          case 't' =>
            buffer.cat('\t')
          case 'u' =>
            try {
              val hex = literal.substring(i + 1, i + 5)
              val code = Integer.parseInt(hex, 16)
              buffer.cat(code.toChar)
              i += 4
            } catch {
              case _: Exception =>
                if (liberal)
                  buffer.append("\\u")
                else
                  throw new XPathException("Invalid JSON escape: \\u must be followed by four hex characters", errorCode)
            }
          case _ =>
            if (liberal) buffer.cat(literal.charAt(i))
            else {
              val next = literal.charAt(i)
              val xx =
                if (next < 256)
                  next.toString
                else
                  "x" + Integer.toHexString(next)
              throw new XPathException(s"Unknown escape sequence \\$xx", errorCode)
            }
        }
      }
      else buffer.cat(c)
      i += 1
    }
    buffer.toString
  }

  /**
   * Throw an error
   *
   * @param message the error message
   * @param code    the error code to be used
   */
  @throws[XPathException]
  private def error(message: String, code: String) = throw new XPathException(message, code)

  object JsonToken extends Enumeration {

    val LSQB            : JsonToken = new JsonToken
    val RSQB            : JsonToken = new JsonToken
    val LCURLY          : JsonToken = new JsonToken
    val RCURLY          : JsonToken = new JsonToken
    val STRING_LITERAL  : JsonToken = new JsonToken
    val NUMERIC_LITERAL : JsonToken = new JsonToken
    val TRUE            : JsonToken = new JsonToken
    val FALSE           : JsonToken = new JsonToken
    val NULL            : JsonToken = new JsonToken
    val COLON           : JsonToken = new JsonToken
    val COMMA           : JsonToken = new JsonToken
    val UNQUOTED_STRING : JsonToken = new JsonToken
    val EOF             : JsonToken = new JsonToken

    class JsonToken
  }

  import JsonToken._

  def toString(token: JsonToken, currentTokenValue: String): String =
    token match {
      case LSQB =>
        "["
      case RSQB =>
        "]"
      case LCURLY =>
        "{"
      case RCURLY =>
        "}"
      case STRING_LITERAL =>
        "string (\"" + currentTokenValue + "\")"
      case NUMERIC_LITERAL =>
        "number (" + currentTokenValue + ")"
      case TRUE =>
        "true"
      case FALSE =>
        "false"
      case NULL =>
        "null"
      case COLON =>
        ":"
      case COMMA =>
        ","
      case EOF =>
        "<eof>"
      case _ =>
        "<" + token + ">"
    }
}


/**
 * Create a JSON parser
 */
class JsonParser {
  /**
   * Parse the JSON string according to supplied options
   *
   * @param input   JSON input string
   * @param flags   options for the conversion as a map of xs:string : value pairs
   * @param handler event handler to which parsing events are notified
   * @param context XPath evaluation context
   * @throws XPathException if the syntax of the input is incorrect
   */
  @throws[XPathException]
  def parse(input: String, flags: Int, handler: JsonHandler, context: XPathContext): Unit = {
    if (input.isEmpty)
      invalidJSON("An empty string is not valid JSON", JsonParser.ERR_GRAMMAR, 1)
    val t = new JsonTokenizer(input)
    t.next()
    parseConstruct(handler, t, flags, context)
    if (t.next != JsonParser.JsonToken.EOF) invalidJSON("Unexpected token beyond end of JSON input", JsonParser.ERR_GRAMMAR, t.lineNumber)
  }

  /**
   * Parse a JSON construct (top-level or nested)
   *
   * @param handler   the handler to generate the result
   * @param tokenizer the tokenizer, positioned at the first token of the construct to be read
   * @param flags     parsing options
   * @param context   XPath evaluation context
   */
  @throws[XPathException]
  private def parseConstruct(handler: JsonHandler, tokenizer: JsonParser#JsonTokenizer, flags: Int, context: XPathContext): Unit = {
    val debug = (flags & JsonParser.DEBUG) != 0
    if (debug) System.err.println("token:" + tokenizer.currentToken + " :" + tokenizer.currentTokenValue)
    tokenizer.currentToken match {
      case JsonParser.JsonToken.LCURLY =>
        parseObject(handler, tokenizer, flags, context)
      case JsonParser.JsonToken.LSQB =>
        parseArray(handler, tokenizer, flags, context)
      case JsonParser.JsonToken.NUMERIC_LITERAL =>
        val lexical = tokenizer.currentTokenValue.toString
        val d = parseNumericLiteral(lexical, flags, tokenizer.lineNumber)
        handler.writeNumeric(lexical, d)
      case JsonParser.JsonToken.TRUE =>
        handler.writeBoolean(true)
      case JsonParser.JsonToken.FALSE =>
        handler.writeBoolean(false)
      case JsonParser.JsonToken.NULL =>
        handler.writeNull()
      case JsonParser.JsonToken.STRING_LITERAL =>
        val literal = tokenizer.currentTokenValue.toString
        handler.writeString(JsonParser.unescape(literal, flags, JsonParser.ERR_GRAMMAR, tokenizer.lineNumber))
      case _ =>
        invalidJSON("Unexpected symbol: " + tokenizer.currentTokenValue, JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
    }
  }

  /**
   * Parse a JSON object (or map), i.e. construct delimited by curly braces
   *
   * @param handler   the handler to generate the result
   * @param tokenizer the tokenizer, positioned at the object to be read
   * @param flags     parsing options as a set of flags
   * @param context   XPath evaluation context
   */
  @throws[XPathException]
  private def parseObject(handler: JsonHandler, tokenizer: JsonParser#JsonTokenizer, flags: Int, context: XPathContext): Unit = {
    val liberal = (flags & JsonParser.LIBERAL) != 0
    handler.startMap()
    var tok = tokenizer.next()
    breakable {
      while (tok != JsonParser.JsonToken.RCURLY) {
        if ((tok ne JsonParser.JsonToken.STRING_LITERAL) && !((tok eq JsonParser.JsonToken.UNQUOTED_STRING) && liberal))
          invalidJSON("Property name must be a string literal", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
        var key = tokenizer.currentTokenValue.toString
        key = JsonParser.unescape(key, flags, JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
        val reEscaped = handler.reEscape(key)
        tok = tokenizer.next()
        if (tok ne JsonParser.JsonToken.COLON)
          invalidJSON("Missing colon after \"" + Err.wrap(key) + "\"", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
        tokenizer.next()
        val duplicate = handler.setKey(key, reEscaped)
        if (duplicate && ((flags & JsonParser.DUPLICATES_REJECTED) != 0))
          invalidJSON("Duplicate key value \"" + Err.wrap(key) + "\"", JsonParser.ERR_DUPLICATE, tokenizer.lineNumber)
        try
            if (!duplicate || ((flags & (JsonParser.DUPLICATES_LAST | JsonParser.DUPLICATES_RETAINED)) != 0))
              parseConstruct(handler, tokenizer, flags, context)
          else { // retain first: parse the duplicate value but discard it
            val h2 = new JsonHandler
            h2.setContext(context)
            parseConstruct(h2, tokenizer, flags, context)
          }
        catch {
          case _: StackOverflowError =>
            invalidJSON("Objects are too deeply nested", JsonParser.ERR_LIMITS, tokenizer.lineNumber)
        }
        tok = tokenizer.next()
        if (tok eq JsonParser.JsonToken.COMMA) {
          tok = tokenizer.next()
          if (tok eq JsonParser.JsonToken.RCURLY)
            if (liberal)
              break()
            else
              invalidJSON("Trailing comma after entry in object", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
        }
        else if (tok eq JsonParser.JsonToken.RCURLY) break()
        else invalidJSON("Unexpected token after value of \"" + Err.wrap(key) + "\" property", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
      }
    }
    handler.endMap()
  }

  /**
   * Parse a JSON array, i.e. construct delimited by square brackets
   *
   * @param handler   the handler to generate the result
   * @param tokenizer the tokenizer, positioned at the object to be read
   * @param flags     parsing options
   * @param context   XPath evaluation context
   */
  @throws[XPathException]
  private def parseArray(handler: JsonHandler, tokenizer: JsonParser#JsonTokenizer, flags: Int, context: XPathContext): Unit = {
    val liberal = (flags & JsonParser.LIBERAL) != 0
    handler.startArray()
    var tok = tokenizer.next()
    if (tok eq JsonParser.JsonToken.RSQB) {
      handler.endArray()
      return
    }
    breakable {
      while (true) {
        try
          parseConstruct(handler, tokenizer, flags, context)
        catch {
          case _: StackOverflowError =>
            invalidJSON("Arrays are too deeply nested", JsonParser.ERR_LIMITS, tokenizer.lineNumber)
        }
        tok = tokenizer.next()
        if (tok eq JsonParser.JsonToken.COMMA) {
          tok = tokenizer.next()
          if (tok eq JsonParser.JsonToken.RSQB)
            if (liberal)
              break()
            else
              invalidJSON("Trailing comma after entry in array", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
        } else if (tok eq JsonParser.JsonToken.RSQB)
          break()
        else
          invalidJSON("Unexpected token (" + JsonParser.toString(tok, tokenizer.currentTokenValue.toString) +
          ") after entry in array", JsonParser.ERR_GRAMMAR, tokenizer.lineNumber)
      }
    }
    handler.endArray()
  }

  /**
   * Parse a JSON numeric literal,
   *
   * @param token the numeric literal to be parsed and converted
   * @param flags parsing options
   * @return the result of parsing and conversion to XDM
   */
  @throws[XPathException]
  private def parseNumericLiteral(token: String, flags: Int, lineNumber: Int): Double = try {
    if ((flags & JsonParser.LIBERAL) == 0) {
      // extra checks on the number disabled by choosing spec="liberal"
      if (token.startsWith("+"))
        invalidJSON("Leading + sign not allowed: " + token, JsonParser.ERR_GRAMMAR, lineNumber)
      else {
        var t = token
        if (t.startsWith("-")) t = t.substring(1)
        if (t.startsWith("0") && !(t == "0" || t.startsWith("0.") || t.startsWith("0e") || t.startsWith("0E")))
          invalidJSON("Redundant leading zeroes not allowed: " + token, JsonParser.ERR_GRAMMAR, lineNumber)
        if (t.endsWith(".") || t.contains(".e") || t.contains(".E"))
          invalidJSON("Empty fractional part not allowed", JsonParser.ERR_GRAMMAR, lineNumber)
        if (t.startsWith("."))
          invalidJSON("Empty integer part not allowed", JsonParser.ERR_GRAMMAR, lineNumber)
      }
    }
    StringToDouble.getInstance.stringToNumber(token)
  } catch {
    case e: NumberFormatException =>
      invalidJSON("Invalid numeric literal: " + e.getMessage, JsonParser.ERR_GRAMMAR, lineNumber)
      Double.NaN
  }

  @throws[XPathException]
  private def invalidJSON(message: String, code: String, lineNumber: Int): Nothing =
    JsonParser.error("Invalid JSON input on line " + lineNumber + ": " + message, code)

  /**
   * Inner class to do the tokenization
   */
  class JsonTokenizer(var input: String) {
    private var position = 0
    // Ignore a leading BOM
    if (input.nonEmpty && input.charAt(0) == 65279)
      position += 1
    var lineNumber              = 1
    var currentToken: JsonToken = null
    var currentTokenValue = new FastStringBuffer(FastStringBuffer.C64)

    @throws[XPathException]
    def next(): JsonToken = {
      currentToken = readToken
      currentToken
    }

    @throws[XPathException]
    private def readToken: JsonToken = {
      val jsonToken = new JsonToken
      if (position >= input.length)
        return JsonParser.JsonToken.EOF
      while (true) {
        val c = input.charAt(position)
        c match {
          case '\n' | '\r' | ' ' | '\t' =>
            if (c == '\n' || c == '\r')
              lineNumber += 1
            if ( {
              position += 1
              position
            } >= input.length) return JsonParser.JsonToken.EOF
          case _ =>
        }
      }
      val ch = input.charAt({
        position += 1
        position - 1
      })
      ch match {
        case '[' =>
          JsonParser.JsonToken.LSQB
        case '{' =>
          JsonParser.JsonToken.LCURLY
        case ']' =>
          JsonParser.JsonToken.RSQB
        case '}' =>
          JsonParser.JsonToken.RCURLY
        case '"' =>
          currentTokenValue.setLength(0)
          var afterBackslash = false
          breakable {
            while (true) {
              if (position >= input.length)
                invalidJSON("Unclosed quotes in string literal", JsonParser.ERR_GRAMMAR, lineNumber)
              val c = input.charAt({
                position += 1
                position - 1
              })
              if (c < 32)
                invalidJSON("Unescaped control character (x" + Integer.toHexString(c) + ")", JsonParser.ERR_GRAMMAR, lineNumber)
              if (afterBackslash && c == 'u')
                try {
                  val hex = input.substring(position, position + 4)
                  //noinspection ResultOfMethodCallIgnored
                  Integer.parseInt(hex, 16)
                } catch {
                  case _: Exception =>
                    invalidJSON("\\u must be followed by four hex characters", JsonParser.ERR_GRAMMAR, lineNumber)
                }
              if (c == '"' && !afterBackslash)
                break()
              else {
                currentTokenValue.cat(c)
                afterBackslash = c == '\\' && !afterBackslash
              }
            }
          }
          JsonParser.JsonToken.STRING_LITERAL
        case ':' =>
          JsonParser.JsonToken.COLON
        case ',' =>
          JsonParser.JsonToken.COMMA
        case '-' => jsonToken
        case '+' => jsonToken
        case '.' => jsonToken
        case '0' => jsonToken
        case '1' => jsonToken
        case '2' => jsonToken
        case '3' => jsonToken
        case '4' => jsonToken
        case '5' => jsonToken
        case '6' => jsonToken
        case '7' => jsonToken
        case '8' => jsonToken
        case '9' =>
          currentTokenValue.setLength(0)
          currentTokenValue.cat(ch)
          if (position < input.length) {
            // We could be in ECMA mode when there is a single digit
            breakable {
              while (true) {
                val c = input.charAt(position)
                if ((c >= '0' && c <= '9') || c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E') {
                  currentTokenValue.cat(c)
                  if ( {
                    position += 1
                    position
                  } >= input.length)
                    break()
                }
                else
                  break()
              }
            }
          }
          JsonParser.JsonToken.NUMERIC_LITERAL
        case _ =>
          // Allow unquoted strings in liberal mode
          if (NameChecker.isNCNameChar(ch)) {
            currentTokenValue.setLength(0)
            currentTokenValue.cat(ch)
            breakable {
              while (position < input.length) {
                val c = input.charAt(position)
                if (NameChecker.isNCNameChar(c)) {
                  currentTokenValue.cat(c)
                  position += 1
                } else
                  break()
              }
            }
            currentTokenValue.toString match {
              case "true" =>
                JsonParser.JsonToken.TRUE
              case "false" =>
                JsonParser.JsonToken.FALSE
              case "null" =>
                JsonParser.JsonToken.NULL
              case _ =>
                JsonParser.JsonToken.UNQUOTED_STRING
            }
          }
          else {
            val c = input.charAt({
              position -= 1
              position
            })
            invalidJSON("Unexpected character '" + c + "' (\\u" + Integer.toHexString(c) + ") at position " + position, JsonParser.ERR_GRAMMAR, lineNumber)
            JsonParser.JsonToken.EOF
          }
      }
    }
  }

}