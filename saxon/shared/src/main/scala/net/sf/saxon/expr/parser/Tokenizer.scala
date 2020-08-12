////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.parser


import net.sf.saxon.om.NameChecker
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Whitespace
import java.util

import scala.util.control.Breaks._

/**
 * Tokenizer for expressions and inputs.
 * <p>This code was originally derived from James Clark's xt, though it has been greatly modified since.
 * See copyright notice at end of file.</p>
 */
object Tokenizer {
  /**
   * Initial default state of the Tokenizer
   */
  val DEFAULT_STATE = 0
  /**
   * State in which a name is NOT to be merged with what comes next, for example "("
   */
  val BARE_NAME_STATE = 1
  /**
   * State in which the next thing to be read is a SequenceType
   */
  val SEQUENCE_TYPE_STATE = 2
  /**
   * State in which the next thing to be read is an operator
   */
  val OPERATOR_STATE = 3
}

final class Tokenizer() {
  private var state = Tokenizer.DEFAULT_STATE
  /**
   * The number identifying the most recently read token
   */
  var currentToken: Int = Token.EOF
  /**
   * The string value of the most recently read token
   */
  /*@Nullable*/ var currentTokenValue: String = null
  /**
   * The position in the input expression where the current token starts
   */
  var currentTokenStartOffset = 0
  /**
   * The number of the next token to be returned
   */
  private var nextToken = Token.EOF
  /**
   * The string value of the next token to be returned
   */
  private var nextTokenValue: String = null
  /**
   * The position in the expression of the start of the next token
   */
  private var nextTokenStartOffset = 0
  /**
   * The string being parsed
   */
  var input: String = null
  /**
   * The current position within the input string
   */
  var inputOffset = 0
  /**
   * The length of the input string
   */
  private var inputLength = 0
  /**
   * The line number (within the expression) of the current token
   */
  private var lineNumber = 1
  /**
   * The line number (within the expression) of the next token
   */
  private var nextLineNumber = 1
  /**
   * List containing the positions (offsets in the input string) at which newline characters
   * occur
   */
  private var newlineOffsets: util.List[Integer] = null
  /**
   * The token number of the token that preceded the current token
   */
  private var precedingToken = Token.UNKNOWN
  /**
   * The content of the preceding token
   */
  private var precedingTokenValue = ""
  /**
   * Flag to disallow "union" as a synonym for "|" when parsing XSLT 2.0 patterns
   */
  var disallowUnionKeyword = false
  /**
   * Flag to indicate that this is XQuery as distinct from XPath
   */
  var isXQuery = false
  /**
   * XPath language level: e.g. 2.0, 3.0, or 3.1
   */
  var languageLevel = 20
  /**
   * Flag to allow Saxon extensions
   */
  var allowSaxonExtensions: Boolean = false

  /**
   * Get the current tokenizer state
   *
   * @return the current state
   */
  def getState = state

  /**
   * Set the tokenizer into a special state
   *
   * @param state the new state
   */
  def setState(state: Int) = {
    this.state = state
    if (state == Tokenizer.DEFAULT_STATE) { // force the followsOperator() test to return true
      precedingToken = Token.UNKNOWN
      precedingTokenValue = ""
      currentToken = Token.UNKNOWN
    }
    else if (state == Tokenizer.OPERATOR_STATE) {
      precedingToken = Token.RPAR
      precedingTokenValue = ")"
      currentToken = Token.RPAR
    }
  }

  /**
   * Prepare a string for tokenization.
   * The actual tokens are obtained by calls on next()
   *
   * @param input the string to be tokenized
   * @param start start point within the string
   * @param end   end point within the string (last character not read):
   *              -1 means end of string
   * @throws XPathException if a lexical error occurs, e.g. unmatched
   *                        string quotes
   */
  @throws[XPathException]
  def tokenize(input: String, start: Int, end: Int) = {
    nextToken = Token.EOF
    nextTokenValue = null
    nextTokenStartOffset = 0
    inputOffset = start
    this.input = input
    this.lineNumber = 0
    nextLineNumber = 0
    if (end == -1) inputLength = input.length
    else inputLength = end
    // The tokenizer actually reads one token ahead. The raw lexical analysis performed by
    // the lookAhead() method does not (in general) distinguish names used as QNames from names
    // used for operators, axes, and functions. The next() routine further refines names into the
    // correct category, by looking at the following token. In addition, it combines compound tokens
    // such as "instance of" and "cast as".
    lookAhead()
    next()
  }

  /**
   * Get the next token from the input expression. The type of token is returned in the
   * currentToken variable, the string value of the token in currentTokenValue.
   *
   * @throws XPathException if a lexical error is detected
   */
  @throws[XPathException]
  def next(): Unit = {
    precedingToken = currentToken
    precedingTokenValue = currentTokenValue
    currentToken = nextToken
    currentTokenValue = nextTokenValue
    if (currentTokenValue == null) currentTokenValue = ""
    currentTokenStartOffset = nextTokenStartOffset
    lineNumber = nextLineNumber
    // disambiguate the current token based on the tokenizer state
    currentToken match {
      case Token.NAME =>
        val optype = getBinaryOp(currentTokenValue)
        if (optype != Token.UNKNOWN && !followsOperator(precedingToken)) currentToken = optype
      case Token.LT =>
        if (isXQuery && followsOperator(precedingToken)) currentToken = Token.TAG
      case Token.STAR =>
        if (!followsOperator(precedingToken)) currentToken = Token.MULT
    }
    if (currentToken == Token.TAG || currentToken == Token.RCURLY) { // No lookahead after encountering "<" at the start of an XML-like tag.
      // After an RCURLY, the parser must do an explicit lookahead() to continue
      // tokenizing; otherwise it can continue with direct character reading
      return
    }
    val oldPrecedingToken = precedingToken
    lookAhead()
    if (currentToken == Token.NAME) {
      if (state == Tokenizer.BARE_NAME_STATE) return
      if (oldPrecedingToken == Token.DOLLAR) return
      nextToken match {
        case Token.LPAR =>
          val op = getBinaryOp(currentTokenValue)
          // the test on followsOperator() is to cater for an operator being used as a function name,
          // e.g. is(): see XQTS test K-FunctionProlog-66
          if (op == Token.UNKNOWN || followsOperator(oldPrecedingToken)) {
            currentToken = getFunctionType(currentTokenValue).asInstanceOf[Int]
            lookAhead() // swallow the "("
          }
          else currentToken = op
        case Token.LCURLY =>
          if (!(state == Tokenizer.SEQUENCE_TYPE_STATE)) {
            currentToken = Token.KEYWORD_CURLY
            lookAhead() // swallow the "{"
          }
        case Token.COLONCOLON =>
          lookAhead()
          currentToken = Token.AXIS
        case Token.HASH =>
          lookAhead()
          currentToken = Token.NAMED_FUNCTION_REF
        case Token.COLONSTAR =>
          lookAhead()
          currentToken = Token.PREFIX
        case Token.DOLLAR =>
          currentTokenValue match {
            case "for" =>
              currentToken = Token.FOR
            case "some" =>
              currentToken = Token.SOME
            case "every" =>
              currentToken = Token.EVERY
            case "let" =>
              currentToken = Token.LET
            case "count" =>
              currentToken = Token.COUNT
            case "copy" =>
              currentToken = Token.COPY
          }
        case Token.PERCENT =>
          if (currentTokenValue == "declare") currentToken = Token.DECLARE_ANNOTATED
        case Token.NAME =>
          var candidate = -1
          currentTokenValue match {
            case "element" =>
              candidate = Token.ELEMENT_QNAME
            case "attribute" =>
              candidate = Token.ATTRIBUTE_QNAME
            case "processing-instruction" =>
              candidate = Token.PI_QNAME
            case "namespace" =>
              candidate = Token.NAMESPACE_QNAME
          }
          if (candidate != -1) { // <'element' QName '{'> constructor
            // <'attribute' QName '{'> constructor
            // <'processing-instruction' QName '{'> constructor
            // <'namespace' QName '{'> constructor
            val qname = nextTokenValue
            val saveTokenValue = currentTokenValue
            val savePosition = inputOffset
            lookAhead()
            if (nextToken == Token.LCURLY) {
              currentToken = candidate
              currentTokenValue = qname
              lookAhead()
              return
            }
            else { // backtrack (we don't have 2-token lookahead; this is the
              // only case where it's needed. So we backtrack instead.)
              currentToken = Token.NAME
              currentTokenValue = saveTokenValue
              inputOffset = savePosition
              nextToken = Token.NAME
              nextTokenValue = qname
            }
          }
          val composite = currentTokenValue + ' ' + nextTokenValue
          val `val` = Token.doubleKeywords.get(composite)
          if (`val` == null) {
          }
          else {
            currentToken = `val`
            currentTokenValue = composite
            // some tokens are actually triples
            if (currentToken == Token.REPLACE_VALUE) { // this one's a quadruplet - "replace value of node"
              lookAhead()
              if (nextToken != Token.NAME || !(nextTokenValue == "of")) throw new XPathException("After '" + composite + "', expected 'of'")
              lookAhead()
              if (nextToken != Token.NAME || !(nextTokenValue == "node")) throw new XPathException("After 'replace value of', expected 'node'")
              nextToken = currentToken // to reestablish after-operator state
            }
            lookAhead()
            return
          }
        case _ =>
        // no action needed
      }
    }
  }

  /**
   * Peek ahead at the next token
   */
  private[parser] def peekAhead = nextToken

  /**
   * Force the current token to be treated as an operator if possible
   */
  def treatCurrentAsOperator() = currentToken match {
    case Token.NAME =>
      val optype = getBinaryOp(currentTokenValue)
      if (optype != Token.UNKNOWN) currentToken = optype
    case Token.STAR =>
      currentToken = Token.MULT
  }

  /**
   * Look ahead by one token. This method does the real tokenization work.
   * The method is normally called internally, but the XQuery parser also
   * calls it to resume normal tokenization after dealing with pseudo-XML
   * syntax.
   *
   * @throws XPathException if a lexical error occurs
   */
  @throws[XPathException]
  def lookAhead(): Unit = {
    precedingToken = nextToken
    precedingTokenValue = nextTokenValue
    nextTokenValue = null
    nextTokenStartOffset = inputOffset
    breakable {
      while (true) {
        if (inputOffset >= inputLength) {
          nextToken = Token.EOF
          return
        }
        var c = input.charAt({
          inputOffset += 1;
          inputOffset - 1
        })
        c match {
          case '/' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '/') {
              inputOffset += 1
              nextToken = Token.SLASH_SLASH
              return
            }
            nextToken = Token.SLASH
            return
          case ':' =>
            if (inputOffset < inputLength) if (input.charAt(inputOffset) == ':') {
              inputOffset += 1
              nextToken = Token.COLONCOLON
              return
            }
            else if (input.charAt(inputOffset) == '=') {
              nextToken = Token.ASSIGN
              inputOffset += 1
              return
            }
            else { // if (input.charAt(inputOffset) == ' ') ??
              nextToken = Token.COLON
              return
            }
            throw new XPathException("Unexpected colon at start of token")
          case '@' =>
            nextToken = Token.AT
            return
          case '?' =>
            nextToken = Token.QMARK
            return
          case '[' =>
            nextToken = Token.LSQB
            return
          case ']' =>
            nextToken = Token.RSQB
            return
          case '{' =>
            nextToken = Token.LCURLY
            return
          case '}' =>
            nextToken = Token.RCURLY
            return
          case ';' =>
            nextToken = Token.SEMICOLON
            state = Tokenizer.DEFAULT_STATE
            return
          case '%' =>
            nextToken = Token.PERCENT
            return
          case '(' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '#') {
              inputOffset += 1
              val pragmaStart = inputOffset
              var nestingDepth = 1
              while ( {
                nestingDepth > 0 && inputOffset < (inputLength - 1)
              }) {
                if (input.charAt(inputOffset) == '\n') incrementLineNumber()
                else if (input.charAt(inputOffset) == '#' && input.charAt(inputOffset + 1) == ')') {
                  nestingDepth -= 1
                  inputOffset += 1
                }
                else if (input.charAt(inputOffset) == '(' && input.charAt(inputOffset + 1) == '#') {
                  nestingDepth += 1
                  inputOffset += 1
                }
                inputOffset += 1
              }
              if (nestingDepth > 0) throw new XPathException("Unclosed XQuery pragma")
              nextToken = Token.PRAGMA
              nextTokenValue = input.substring(pragmaStart, inputOffset - 2)
              return
            }
            if (inputOffset < inputLength && input.charAt(inputOffset) == ':') { // XPath comment syntax is (: .... :)
              // Comments may be nested, and may now be empty
              inputOffset += 1
              var nestingDepth = 1
              while ( {
                nestingDepth > 0 && inputOffset < (inputLength - 1)
              }) {
                if (input.charAt(inputOffset) == '\n') incrementLineNumber()
                else if (input.charAt(inputOffset) == ':' && input.charAt(inputOffset + 1) == ')') {
                  nestingDepth -= 1
                  inputOffset += 1
                }
                else if (input.charAt(inputOffset) == '(' && input.charAt(inputOffset + 1) == ':') {
                  nestingDepth += 1
                  inputOffset += 1
                }
                inputOffset += 1
              }
              if (nestingDepth > 0) throw new XPathException("Unclosed XPath comment")
              lookAhead()
            }
            else nextToken = Token.LPAR
            return
          case ')' =>
            nextToken = Token.RPAR
            return
          case '+' =>
            nextToken = Token.PLUS
            return
          case '-' =>
            nextToken = Token.MINUS // not detected if part of a name
            return
          case '=' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '>') {
              inputOffset += 1
              nextToken = Token.ARROW
              return
            }
            nextToken = Token.EQUALS
            return
          case '!' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
              inputOffset += 1
              nextToken = Token.NE
              return
            }
            nextToken = Token.BANG
            return
          case '*' =>
            // disambiguation of MULT and STAR is now done later
            if (inputOffset < inputLength && input.charAt(inputOffset) == ':' && inputOffset + 1 < inputLength && (input.charAt(inputOffset + 1) > 127 || NameChecker.isNCNameStartChar(input.charAt(inputOffset + 1)))) {
              inputOffset += 1
              nextToken = Token.SUFFIX
              return
            }
            nextToken = Token.STAR
            return
          case ',' =>
            nextToken = Token.COMMA
            return
          case '$' =>
            nextToken = Token.DOLLAR
            return
          case '|' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '|') {
              inputOffset += 1
              nextToken = Token.CONCAT
              return
            }
            nextToken = Token.UNION
            return
          case '#' =>
            nextToken = Token.HASH
            return
          case '<' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
              inputOffset += 1
              nextToken = Token.LE
              return
            }
            if (inputOffset < inputLength && input.charAt(inputOffset) == '<') {
              inputOffset += 1
              nextToken = Token.PRECEDES
              return
            }
            nextToken = Token.LT
            return
          case '>' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '=') {
              inputOffset += 1
              nextToken = Token.GE
              return
            }
            if (inputOffset < inputLength && input.charAt(inputOffset) == '>') {
              inputOffset += 1
              nextToken = Token.FOLLOWS
              return
            }
            nextToken = Token.GT
            return
          case '.' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '.') {
              inputOffset += 1
              nextToken = Token.DOTDOT
              return
            }
            if (inputOffset < inputLength && input.charAt(inputOffset) == '{') {
              inputOffset += 1
              nextTokenValue = "."
              nextToken = Token.KEYWORD_CURLY
              return
            }
            if (inputOffset == inputLength || input.charAt(inputOffset) < '0' || input.charAt(inputOffset) > '9') {
              nextToken = Token.DOT
              return
            }
          // otherwise drop through: we have a number starting with a decimal point
          case '0' =>
          case '1' =>
          case '2' =>
          case '3' =>
          case '4' =>
          case '5' =>
          case '6' =>
          case '7' =>
          case '8' =>
          case '9' =>
            var allowE = true
            var allowSign = false
            var allowDot = true
            // numloop //todo: labels are not supporte
            breakable {
              while (true) {
                c match {
                  case '0' =>
                  case '1' =>
                  case '2' =>
                  case '3' =>
                  case '4' =>
                  case '5' =>
                  case '6' =>
                  case '7' =>
                  case '8' =>
                  case '9' =>
                    allowSign = false
                  case '.' =>
                    if (allowDot) {
                      allowDot = false
                      allowSign = false
                    }
                    else {
                      inputOffset -= 1
                      break
                    }
                  case 'E' =>
                  case 'e' =>
                    if (allowE) {
                      allowSign = true
                      allowE = false
                    }
                    else {
                      inputOffset -= 1
                      break
                    }
                  case '+' =>
                  case '-' =>
                    if (allowSign) allowSign = false
                    else {
                      inputOffset -= 1
                      break
                    }
                  case _ =>
                    if (('a' <= c && c <= 'z') || c > 127) { // this prevents the famous "10div 3"
                      throw new XPathException("Separator needed after numeric literal")
                    }
                    inputOffset -= 1
                    break
                }
                if (inputOffset >= inputLength) break
                c = input.charAt({
                  inputOffset += 1;
                  inputOffset - 1
                })
              }
            }
            nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
            nextToken = Token.NUMBER
            return
          case '"' =>
          case '\'' =>
            nextTokenValue = ""
            while ( {
              true
            }) {
              inputOffset = input.indexOf(c, inputOffset)
              if (inputOffset < 0) {
                inputOffset = nextTokenStartOffset + 1
                throw new XPathException("Unmatched quote in expression")
              }
              nextTokenValue += input.substring(nextTokenStartOffset + 1, {
                inputOffset += 1;
                inputOffset - 1
              })
              if (inputOffset < inputLength) {
                val n = input.charAt(inputOffset)
                if (n == c) { // Doubled delimiters
                  nextTokenValue += c
                  nextTokenStartOffset = inputOffset
                  inputOffset += 1
                }
                else break //todo: break is not supported
              }
              else break //todo: break is not supported
            }
            // maintain line number if there are newlines in the string
            if (nextTokenValue.indexOf('\n') >= 0) for (i <- 0 until nextTokenValue.length) {
              if (nextTokenValue.charAt(i) == '\n') incrementLineNumber(nextTokenStartOffset + i + 1)
            }
            //nextTokenValue = nextTokenValue.intern();
            nextToken = Token.STRING_LITERAL
            return
          case '`' =>
            if (isXQuery && inputOffset < inputLength - 1 && input.charAt(inputOffset) == '`' && input.charAt(inputOffset + 1) == '[') {
              inputOffset += 2
              var j = inputOffset
              var newlines = 0
              while ( {
                true
              }) {
                if (j >= inputLength) throw new XPathException("Unclosed string template in expression")
                if (input.charAt(j) == '\n') newlines += 1
                else if (input.charAt(j) == '`' && j + 1 < inputLength && input.charAt(j + 1) == '{') {
                  nextToken = Token.STRING_CONSTRUCTOR_INITIAL
                  nextTokenValue = input.substring(inputOffset, j)
                  inputOffset = j + 2
                  incrementLineNumber(newlines)
                  return
                }
                else if (input.charAt(j) == ']' && j + 2 < inputLength && input.charAt(j + 1) == '`' && input.charAt(j + 2) == '`') {
                  nextToken = Token.STRING_LITERAL_BACKTICKED
                  // Can't return STRING_LITERAL because it's not accepted everywhere that a string literal is
                  nextTokenValue = input.substring(inputOffset, j)
                  inputOffset = j + 3
                  incrementLineNumber(newlines)
                  return
                }
                j += 1
              }
            }
            else throw new XPathException("Invalid character '`' (backtick) in expression")
          case '\n' =>
            incrementLineNumber()
          // drop through
          case ' ' =>
          case '\t' =>
          case '\r' =>
            nextTokenStartOffset = inputOffset
          case '\u00B6' =>
          case 'Q' =>
            if (inputOffset < inputLength && input.charAt(inputOffset) == '{') { // EQName, revised syntax as per bug 15399
              val close = input.indexOf('}', {
                inputOffset += 1;
                inputOffset - 1
              })
              if (close < inputOffset) throw new XPathException("Missing closing brace in EQName")
              var uri = input.substring(inputOffset, close)
              uri = Whitespace.collapseWhitespace(uri).toString // Bug 29708
              if (uri.contains("{")) throw new XPathException("EQName must not contain opening brace")
              inputOffset = close + 1
              val start = inputOffset
              var isStar = false
              while ( {
                inputOffset < inputLength
              }) {
                val c2 = input.charAt(inputOffset)
                if (c2 > 0x80 || Character.isLetterOrDigit(c2) || c2 == '_' || c2 == '.' || c2 == '-') inputOffset += 1
                else if (c2 == '*' && (start == inputOffset)) {
                  inputOffset += 1
                  isStar = true
                  break //todo: break is not supported
                }
                else break //todo: break is not supported
              }
              val localName = input.substring(start, inputOffset)
              nextTokenValue = "Q{" + uri + "}" + localName
              // Reuse Token.NAME because EQName is allowed anywhere that QName is allowed
              nextToken = if (isStar) Token.PREFIX
              else Token.NAME
              return
            }
          /* else fall through */ case _ =>
            if (c < 0x80 && !Character.isLetter(c)) throw new XPathException("Invalid character '" + c + "' in expression")
          /* fall through */ case '_' =>
            var foundColon = false
            // loop //todo: labels are not supported
            breakable {
              while (inputOffset < inputLength) {
                c = input.charAt(inputOffset)
                c match {
                  case ':' =>
                    if (!foundColon) {
                      if (precedingToken == Token.QMARK || precedingToken == Token.SUFFIX) { // only NCName allowed after "? in a lookup expression, or after *:
                        nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                        nextToken = Token.NAME
                        return
                      }
                      if (inputOffset + 1 < inputLength) {
                        val nc = input.charAt(inputOffset + 1)
                        if (nc == ':') {
                          nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                          nextToken = Token.AXIS
                          inputOffset += 2
                          return
                        }
                        else if (nc == '*') {
                          nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                          nextToken = Token.PREFIX
                          inputOffset += 2
                          return
                        }
                        else if (!(nc == '_' || nc > 127 || Character.isLetter(nc))) { // for example: "let $x:=2", "x:y:z", "x:2"
                          // end the token before the colon
                          nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
                          nextToken = Token.NAME
                          return
                        }
                      }
                      foundColon = true
                    }
                    else break
                  case '.' =>
                  case '-' =>
                    // If the name up to the "-" or "." is a valid operator, and if the preceding token
                    // is such that an operator is valid here and an NCName isn't, then quit here (bug 2715)
                    if (precedingToken > Token.LAST_OPERATOR && !(precedingToken == Token.QMARK || precedingToken == Token.SUFFIX) && getBinaryOp(input.substring(nextTokenStartOffset, inputOffset)) != Token.UNKNOWN && !(precedingToken == Token.NAME && getBinaryOp(precedingTokenValue) != Token.UNKNOWN)) {
                      nextToken = getBinaryOp(input.substring(nextTokenStartOffset, inputOffset))
                      return
                    }
                  // else fall through
                  case '_' =>
                  case _ =>
                    if (c < 0x80 && !Character.isLetterOrDigit(c)) break
                }
                inputOffset += 1
              }
            }
            nextTokenValue = input.substring(nextTokenStartOffset, inputOffset)
            nextToken = Token.NAME
            return
        }
      }
    }
  }

  /**
   * Identify a binary operator
   *
   * @param s String representation of the operator - must be interned
   * @return the token number of the operator, or UNKNOWN if it is not a
   *         known operator
   */
  private[parser] def getBinaryOp(s: String) = s match {
    case "after" =>
      Token.AFTER
    case "and" =>
      Token.AND
    case "as" =>
      Token.AS
    case "before" =>
      Token.BEFORE
    case "case" =>
      Token.CASE
    case "default" =>
      Token.DEFAULT
    case "div" =>
      Token.DIV
    case "else" =>
      Token.ELSE
    case "eq" =>
      Token.FEQ
    case "except" =>
      Token.EXCEPT
    case "ge" =>
      Token.FGE
    case "gt" =>
      Token.FGT
    case "idiv" =>
      Token.IDIV
    case "in" =>
      Token.IN
    case "intersect" =>
      Token.INTERSECT
    case "into" =>
      Token.INTO
    case "is" =>
      Token.IS
    case "le" =>
      Token.FLE
    case "lt" =>
      Token.FLT
    case "mod" =>
      Token.MOD
    case "modify" =>
      Token.MODIFY
    case "ne" =>
      Token.FNE
    case "or" =>
      Token.OR
    case "otherwise" =>
      Token.OTHERWISE
    case "return" =>
      Token.RETURN
    case "satisfies" =>
      Token.SATISFIES
    case "then" =>
      Token.THEN
    case "to" =>
      Token.TO
    case "union" =>
      Token.UNION
    case "where" =>
      Token.WHERE
    case "with" =>
      Token.WITH
    case "orElse" =>
      if (allowSaxonExtensions) Token.OR_ELSE
      else Token.UNKNOWN
    case "andAlso" =>
      if (allowSaxonExtensions) Token.AND_ALSO
      else Token.UNKNOWN
    case _ =>
      Token.UNKNOWN
  }

  /**
   * Distinguish nodekind names, "if", and function names, which are all
   * followed by a "("
   *
   * @param s the name - must be interned
   * @return the token number
   */
  private def getFunctionType(s: String) = s match {
    case "if" =>
      Token.IF
    case "map" =>
    case "namespace-node" =>
    case "array" =>
    case "function" =>
      if (languageLevel == 20) Token.FUNCTION
      else Token.NODEKIND
    case "node" =>
    case "schema-attribute" =>
    case "schema-element" =>
    case "processing-instruction" =>
    case "empty-sequence" =>
    case "document-node" =>
    case "comment" =>
    case "element" =>
    case "item" =>
    case "text" =>
    case "attribute" =>
      Token.NODEKIND
    case "atomic" =>
    case "tuple" =>
    case "type" =>
    case "union" =>
      if (allowSaxonExtensions) Token.NODEKIND
      else Token.FUNCTION // Saxon extension types
    case "switch" =>
      // Reserved in XPath 3.0, even though only used in XQuery
      if (languageLevel == 20) Token.FUNCTION
      else Token.SWITCH
    case "otherwise" =>
      Token.OTHERWISE
    case "typeswitch" =>
      Token.TYPESWITCH
    case _ =>
      Token.FUNCTION
  }

  /**
   * Test whether the previous token is an operator
   *
   * @param precedingToken the token to be tested
   * @return true if the previous token is an operator token
   */
  private def followsOperator(precedingToken: Int) = precedingToken <= Token.LAST_OPERATOR

  /**
   * Read next character directly. Used by the XQuery parser when parsing pseudo-XML syntax
   *
   * @return the next character from the input
   * @throws StringIndexOutOfBoundsException
   * if an attempt is made to read beyond
   * the end of the string. This will only occur in the event of a syntax error in the
   * input.
   */
  @throws[StringIndexOutOfBoundsException]
  def nextChar = {
    val c = input.charAt({
      inputOffset += 1;
      inputOffset - 1
    })
    //c = normalizeLineEnding(c);
    if (c == '\n') {
      incrementLineNumber()
      lineNumber += 1
    }
    c
  }

  /**
   * Increment the line number, making a record of where in the input string the newline character occurred.
   */
  private def incrementLineNumber() = {
    nextLineNumber += 1
    if (newlineOffsets == null) newlineOffsets = new util.ArrayList[Integer](20)
    newlineOffsets.add(inputOffset - 1)
  }

  /**
   * Increment the line number, making a record of where in the input string the newline character occurred.
   *
   * @param offset the place in the input string where the newline occurred
   */
  def incrementLineNumber(offset: Int) = {
    nextLineNumber += 1
    if (newlineOffsets == null) newlineOffsets = new util.ArrayList[Integer](20)
    newlineOffsets.add(offset)
  }

  /**
   * Step back one character. If this steps back to a previous line, adjust the line number.
   */
  def unreadChar() = if (input.charAt({
    inputOffset -= 1;
    inputOffset
  }) == '\n') {
    nextLineNumber -= 1
    lineNumber -= 1
    if (newlineOffsets != null) newlineOffsets.remove(newlineOffsets.size - 1)
  }

  /**
   * Get the most recently read text (for use in an error message)
   *
   * @param offset the offset of the offending token, if known, or -1 to use the current offset
   * @return a chunk of text leading up to the error
   */
  private[parser] def recentText(offset: Int): String = if (offset == -1) { // if no offset was supplied, we want the text immediately before the current reading position
    if (inputOffset > inputLength) inputOffset = inputLength
    if (inputOffset < 34) input.substring(0, inputOffset)
    else Whitespace.collapseWhitespace("..." + input.substring(inputOffset - 30, inputOffset)).toString
  }
  else { // if a specific offset was supplied, we want the text *starting* at that offset
    var end = offset + 30
    if (end > inputLength) end = inputLength
    Whitespace.collapseWhitespace((if (offset > 0) "..."
    else "") + input.substring(offset, end)).toString
  }

  /**
   * Get the line number of the current token
   *
   * @return the line number. Line numbers reported by the tokenizer start at zero.
   */
  def getLineNumber = lineNumber

  /**
   * Get the column number of the current token
   *
   * @return the column number. Column numbers reported by the tokenizer start at zero.
   */
  def getColumnNumber = (getLineAndColumn(currentTokenStartOffset) & 0x7fffffff).toInt

  /**
   * Get the line and column number corresponding to a given offset in the input expression,
   * as a long value with the line number in the top half and the column number in the lower half.
   * Line and column numbers reported by the tokenizer start at zero.
   *
   * @param offset the byte offset in the expression
   * @return the line and column number, packed together
   */
  private def getLineAndColumn(offset: Int): Long = {
    if (newlineOffsets == null) return offset
    for (line <- newlineOffsets.size - 1 to 0 by -1) {
      val nloffset = newlineOffsets.get(line)
      if (offset > nloffset) return ((line + 1).toLong << 32) | (offset - nloffset).toLong
    }
    offset
  }

  /**
   * Return the line number corresponding to a given offset in the expression
   *
   * @param offset the byte offset in the expression
   * @return the line number. Line and column numbers reported by the tokenizer start at zero.
   */
  def getLineNumber(offset: Int) = (getLineAndColumn(offset) >> 32).toInt

  /**
   * Return the column number corresponding to a given offset in the expression
   *
   * @param offset the byte offset in the expression
   * @return the column number. Line and column numbers reported by the tokenizer start at zero.
   */
  def getColumnNumber(offset: Int) = (getLineAndColumn(offset) & 0x7fffffff).toInt
}

/*

The following copyright notice is copied from the licence for xt, from which the
original version of this module was derived:
--------------------------------------------------------------------------------
Copyright (c) 1998, 1999 James Clark

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL JAMES CLARK BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of James Clark shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from James Clark.
---------------------------------------------------------------------------
*/