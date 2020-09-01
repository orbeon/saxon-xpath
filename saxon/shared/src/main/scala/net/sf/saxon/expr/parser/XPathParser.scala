////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.parser

import java.util
import java.util.function.IntPredicate

import net.sf.saxon.expr._
import net.sf.saxon.expr.flwor.Clause
import net.sf.saxon.expr.instruct.{Block, Choose, ForEach}
import net.sf.saxon.expr.parser.XPathParser.ParsedLanguage
import net.sf.saxon.functions._
import net.sf.saxon.functions.registry.VendorFunctionSetHE
import net.sf.saxon.lib.{Feature, NamespaceConstant}
import net.sf.saxon.ma.arrays.{ArrayFunctionSet, ArrayItemType, SimpleArrayItem, SquareArrayConstructor}
import net.sf.saxon.ma.map._
import net.sf.saxon.model.BuiltInAtomicType._
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern._
import net.sf.saxon.query.AnnotationList
import net.sf.saxon.s9api.{Location, UnprefixedElementMatchingPolicy}
import net.sf.saxon.trans.{Err, SymbolicName, XPathException}
import net.sf.saxon.utils.{Configuration, Version}
import net.sf.saxon.value._
import net.sf.saxon.z.{IntArraySet, IntSet}

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * Parser for XPath expressions and XSLT patterns.
 * <p>This code was originally inspired by James Clark's xt but has been totally rewritten (several times)</p>
 * <p>The base class handles parsing of XPath 2.0, XPath 3.0 and XPath 3.1 syntax (switched by a languageVersion variable).
 * Subclasses refine this to handle XQuery syntax (1.0, 3.0 and 3.1) and XQuery Update syntax.</p>
 *
 */
object XPathParser {

  object ParsedLanguage extends Enumeration {

    val XPATH: ParsedLanguage = new ParsedLanguage()

    val XSLT_PATTERN: ParsedLanguage = new ParsedLanguage()

    val SEQUENCE_TYPE: ParsedLanguage = new ParsedLanguage()

    val XQUERY: ParsedLanguage = new ParsedLanguage()

    val EXTENDED_ITEM_TYPE: ParsedLanguage = new ParsedLanguage()

    class ParsedLanguage extends Val

    implicit def convertValue(v: Value): ParsedLanguage =
      v.asInstanceOf[ParsedLanguage]

  }

  trait Accelerator {
    /**
     * Attempt fast parsing of an expression, provided it is sufficiently simple.
     *
     * @param t          the tokenizer
     * @param env        the static context
     * @param expression the string containing expression to be parsed
     * @param start      start position within the input string
     * @param terminator either EOF or RCURLY, indicating how parsing should end
     * @return either the parsed expression, or null if it is erroneous or too
     *         complex to parse.
     */
    def parse(t: Tokenizer, env: StaticContext, expression: String, start: Int, terminator: Int): Expression
  }

  /**
   * Get the precedence associated with a given operator
   *
   * @param operator the operator in question
   * @return a higher number for higher precedence (closer binding)
   */
  def operatorPrecedence(operator: Int): Int = operator match {
    case Token.OR | Token.OR_ELSE =>
      4
    case Token.AND | Token.AND_ALSO =>
      5
    case Token.FEQ | Token.FNE | Token.FLE | Token.FLT | Token.FGE | Token.FGT | Token.EQUALS | Token.NE |
         Token.LE | Token.LT | Token.GE | Token.GT | Token.IS | Token.PRECEDES | Token.FOLLOWS =>
      6
    case Token.CONCAT =>
      7
    case Token.TO =>
      8
    case Token.PLUS | Token.MINUS =>
      9
    case Token.MULT | Token.DIV | Token.IDIV | Token.MOD =>
      10
    case Token.OTHERWISE =>
      11
    case Token.UNION =>
      12
    case Token.INTERSECT | Token.EXCEPT =>
      13
    case Token.INSTANCE_OF =>
      14
    case Token.TREAT_AS =>
      15
    case Token.CASTABLE_AS =>
      16
    case Token.CAST_AS =>
      17
    case Token.ARROW =>
      18
    case _ =>
      -1
  }

  /**
   * Determine whether a given built-in type is disallowed in a given environment, and in the
   * case where it is disallowed, return a string explaining why
   *
   * @param pack the containing package
   * @param type the built-in type to be tested
   * @return null if the type is OK to be used; or a string explaining why not.
   */
  def whyDisallowedType(pack: PackageData, `type`: BuiltInAtomicType): String = {
    if (!`type`.isAllowedInXSD10 && pack.getConfiguration.getXsdVersion == Configuration.XSD10) return "The built-in atomic type " + `type`.getDisplayName + " is not recognized unless XSD 1.1 is enabled"
    null
  }

  /**
   * Supporting code for lookup expressions (A?B)
   *
   * @param parser the XPath parser
   * @param lhs    the LHS operand of the lookup expression
   * @param rhs    the RHS operand of the lookup expression
   * @return the result of parsing the expression
   */
  private def lookup(parser: XPathParser, lhs: Expression, rhs: Expression) = new LookupExpression(lhs, rhs)

  /**
   * Supporting code for lookup expressions (A?B) where B is the wildcard "*"
   *
   * @param lhs the LHS operand of the lookup expression
   * @return the result of parsing the expression
   */
  private def lookupStar(lhs: Expression) = new LookupAllExpression(lhs)

  /**
   * Get a message containing suggestions as to why a requested function might not be available
   *
   * @param functionName the name of the required function
   * @param config       the Saxon configuration
   * @return a suggestion as to why the function was not found; or null if no suggestions can be offered.
   */
  def getMissingFunctionExplanation(functionName: StructuredQName, config: Configuration): String = {
    val actualURI = functionName.getURI
    val similarNamespace = NamespaceConstant.findSimilarNamespace(actualURI)
    if (similarNamespace != null) if (similarNamespace == actualURI) similarNamespace match {
      case NamespaceConstant.FN =>
        return null
      case NamespaceConstant.SAXON =>
        if (config.getEditionCode == "HE")
          return "Saxon extension functions are not available under Saxon-HE"
        else if (!config.isLicensedFeature(Configuration.LicenseFeature.PROFESSIONAL_EDITION))
          return "Saxon extension functions require a Saxon-PE or Saxon-EE license"
      case NamespaceConstant.XSLT =>
        if (functionName.getLocalPart == "original")
          return "Function name xsl:original is only available within an overriding function"
        else
          return "There are no functions defined in the XSLT namespace"
    } else
      return "Perhaps the intended namespace was '" + similarNamespace + "'"
    else if (actualURI.contains("java"))
      if (config.getEditionCode == "HE")
        return "Reflexive calls to Java methods are not available under Saxon-HE"
    else if (!config.isLicensedFeature(Configuration.LicenseFeature.PROFESSIONAL_EDITION))
        return "Reflexive calls to Java methods require a Saxon-PE or Saxon-EE license, and none was found"
    else
        return "For diagnostics on calls to Java methods, use the -TJ command line option " + "or set the Configuration property FeatureKeys.TRACE_EXTERNAL_FUNCTIONS"
    else if (actualURI.startsWith("clitype:"))
      if (config.getEditionCode == "HE")
        return "Reflexive calls to external .NET methods are not available under Saxon-HE"
    else if (!config.isLicensedFeature(Configuration.LicenseFeature.PROFESSIONAL_EDITION))
        return "Reflexive calls to external .NET methods require a Saxon-PE or Saxon-EE license, and none was found"
    else
        return "For diagnostics on calls to .NET methods, use the -TJ command line option " + "or call processor.SetProperty(\"http://saxon.sf.net/feature/trace-external-functions\", \"true\")"
    null
  }

  private val reservedFunctionNames30 = Array[AnyRef]("attribute", "comment", "document-node", "element", "empty-sequence", "function", "if", "item", "namespace-node", "node", "processing-instruction", "schema-attribute", "schema-element", "switch", "text", "typeswitch")

  /**
   * Check whether a function name is reserved in XPath 3.0 (when unprefixed)
   *
   * @param name the function name (the local-name as a string)
   * @return true if the function name is reserved
   */
  def isReservedFunctionName30(name: String): Boolean = {
    val x = util.Arrays.binarySearch(reservedFunctionNames30, name)
    x >= 0
  }

  private val reservedFunctionNames31 = Array[AnyRef]("array", "attribute", "comment", "document-node", "element", "empty-sequence", "function", "if", "item", "map", "namespace-node", "node", "processing-instruction", "schema-attribute", "schema-element", "switch", "text", "typeswitch")

  /**
   * Check whether a function name is reserved in XPath 3.1 (when unprefixed)
   *
   * @param name the function name (the local-name as a string)
   * @return true if the function name is reserved
   */
  def isReservedFunctionName31(name: String): Boolean = {
    val x = util.Arrays.binarySearch(reservedFunctionNames31, name)
    x >= 0
  }

  /**
   * A nested location: for use with XPath expressions and queries nested within some
   * larger document. The location information comes in two parts: the location of the query
   * or expression within the containing document, and the location of an error within the
   * query or XPath expression.
   */
  class NestedLocation extends Location {
    final private var containingLocation: Location = null
    final private var localLineNumber: Int = 0
    final private var localColumnNumber: Int = 0
    private var nearbyText: String = null

    /**
     * Create a NestedLocation
     *
     * @param containingLocation the location of the containing construct, typically an attribute or
     *                           text node in an XML document
     * @param localLineNumber    the line number within the containing construct, starting at zero
     * @param localColumnNumber  the column number within the containing construct, starting at zero
     */
    def this(containingLocation: Location, localLineNumber: Int, localColumnNumber: Int) {
      this()
      this.containingLocation = containingLocation.saveLocation
      this.localLineNumber = localLineNumber
      this.localColumnNumber = localColumnNumber
    }

    /**
     * Create a NestedLocation
     *
     * @param containingLocation the location of the containing construct, typically an attribute or
     *                           text node in an XML document
     * @param localLineNumber    the line number within the containing construct, starting at zero
     * @param localColumnNumber  the column number within the containing construct, starting at zero
     * @param nearbyText         text appearing in the vicinity of the error location
     */
    def this(containingLocation: Location, localLineNumber: Int, localColumnNumber: Int, nearbyText: String) {
      this()
      this.containingLocation = containingLocation.saveLocation
      this.localLineNumber = localLineNumber
      this.localColumnNumber = localColumnNumber
      this.nearbyText = nearbyText
    }

    /**
     * Get the location of the container. This is normally used for expressions nested within
     * an XML document, where the container location gives the location of the attribute or text
     * node holding the XPath expression as a whole
     *
     * @return the location of the containing expression or query
     */
    def getContainingLocation: Location = containingLocation

    /**
     * Get the column number of the error within the expression or query
     *
     * @return the column number. This is generally maintained only during parsing,
     *         so it will be returned as -1 (meaning not available) in the case of dynamic
     * errors. Column numbers start at 0. For expressions held within XML attributes,
     *         the position is within the attribute after XML attribute-value normalization,
     *         which replaces newlines by spaces and expands entity references.
     */
    override def getColumnNumber: Int = localColumnNumber

    /**
     * Get the system identifier of the expression's container. This will normally
     * be the URI of the document (or external entity) in which the expression appears.
     *
     * @return the system identifier of the expression's container, or null if not known
     */
    override def getSystemId: String = containingLocation.getSystemId

    /**
     * Get the public identifier. This will normally be null, but is provided for
     * compatibility with SAX and JAXP interfaces
     *
     * @return the public identifier - usually null
     */
    override def getPublicId: String = containingLocation.getPublicId

    /**
     * Get the local line number, that is the line number relative to the start of the
     * expression or query. For expressions held within XML attributes,
     * the position is within the attribute after XML attribute-value normalization,
     * which replaces newlines by spaces and expands entity references; the value
     * will therefore in many cases not be usable. Local line numbers start at 0.
     *
     * @return the local line number within the expression or query. Set to -1
     *         if not known.
     */
    def getLocalLineNumber: Int = localLineNumber

    /**
     * Get the line number within the containing entity. This is the sum of the containing
     * location's line number, plus the local line number. Returns -1 if unknown.
     *
     * @return the line number within the containing entity, or -1 if unknown.
     */
    override def getLineNumber: Int = containingLocation.getLineNumber + localLineNumber

    /**
     * Get text appearing near to the error (typically a syntax error) within the source
     * text of the expression or query.
     *
     * @return nearby text to the error. May be null.
     */
    def getNearbyText: String = nearbyText

    /**
     * Save an immutable copy of the location information. This implementation does
     * nothing, because the object is already immutable
     *
     * @return immutable location information.
     */
    override def saveLocation: Location = this
  }

}


/**
 * Create an expression parser
 */
class XPathParser() {
  var t: Tokenizer = _

  var env: StaticContext = _

  var rangeVariables: mutable.Stack[LocalBinding] = new mutable.Stack[LocalBinding]()

  var qNameParser: QNameParser = _

  var parserExtension: ParserExtension = new ParserExtension()

  var charChecker: IntPredicate = _

  var allowXPath30Syntax: Boolean = false

  var allowXPath30XSLTExtensions: Boolean = false

  var allowXPath31Syntax: Boolean = false

  var allowSaxonExtensions: Boolean = false

  var scanOnly: Boolean = false

  private var allowAbsentExpression: Boolean = false

  var codeInjector: CodeInjector = null

  private var accelerator: XPathParser.Accelerator = null

  var language: ParsedLanguage.ParsedLanguage = ParsedLanguage.XPATH

  var languageVersion: Int = 20

  var catchDepth: Int = 0

  /**
   * Set a CodeInjector which can be used to modify or wrap expressions on the tree
   * as the expression is parsed and the tree is constructed. This is typically used
   * to add tracing code.
   *
   * @param injector the code injector to be used
   */
  def setCodeInjector(injector: CodeInjector): Unit = this.codeInjector = injector

  /**
   * Set a CodeInjector which can be used to modify or wrap expressions on the tree
   * as the expression is parsed and the tree is constructed. This is typically used
   * to add tracing code.
   *
   * @return the code injector in use, if any; or null otherwise
   */
  def getCodeInjector: CodeInjector = codeInjector

  /**
   * Set an accelerator which can be used for fast parsing of special cases
   *
   * @param accelerator a parsing accelerator
   */
  def setAccelerator(accelerator: XPathParser.Accelerator): Unit = this.accelerator = accelerator

  /**
   * Get the tokenizer (the lexical analyzer)
   *
   * @return the tokenizer (the lexical analyzer)
   */
  def getTokenizer: Tokenizer = t

  /**
   * Get the static context used by this expression parser
   *
   * @return the static context
   */
  def getStaticContext: StaticContext = env

  /**
   * Set a parser extension which can handle extensions to the XPath syntax, e.g. for
   * XQuery update extensions
   *
   * @param extension a parser extension
   */
  def setParserExtension(extension: ParserExtension): Unit = this.parserExtension = extension

  /**
   * Set the depth of nesting within try/catch
   *
   * @param depth the depth of nesting
   */
  def setCatchDepth(depth: Int): Unit = catchDepth = depth

  /**
   * Read the next token, catching any exception thrown by the tokenizer
   *
   * @throws XPathException if an invalid token is found
   */
  @throws[XPathException]
  def nextToken(): Unit = try t.next()
  catch {
    case e: XPathException =>
      grumble(e.getMessage)
  }

  /**
   * Expect a given token; fail if the current token is different. Note that this method
   * does not read any tokens.
   *
   * @param token the expected token
   * @throws XPathException if the current token is not the expected
   *                        token
   */
  @throws[XPathException]
  def expect(token: Int): Unit = if (t.currentToken != token) grumble("expected \"" + Token.tokens(token) + "\", found " + currentTokenDisplay)

  /**
   * Report a syntax error (a static error with error code XPST0003)
   *
   * @param message the error message
   * @throws XPathException always thrown: an exception containing the
   *                        supplied message
   */
  @throws[XPathException]
  def grumble(message: String): Unit = grumble(message, if (language eq ParsedLanguage.XSLT_PATTERN) "XTSE0340"
  else "XPST0003")

  /**
   * Report a static error
   *
   * @param message   the error message
   * @param errorCode the error code
   * @throws XPathException always thrown: an exception containing the
   *                        supplied message
   */
  @throws[XPathException]
  def grumble(message: String, errorCode: String): Unit = grumble(message, new StructuredQName("", NamespaceConstant.ERR, errorCode), -1)

  /**
   * Report a static error, with location information
   *
   * @param message   the error message
   * @param errorCode the error code
   * @param offset    the coded location of the error, or -1 if the location of the current token should be used
   * @throws XPathException always thrown: an exception containing the
   *                        supplied message
   */
  @throws[XPathException]
  def grumble(message: String, errorCode: String, offset: Int): Unit = grumble(message, new StructuredQName("", NamespaceConstant.ERR, errorCode), offset)

  /**
   * Report a static error
   *
   * @param message   the error message
   * @param errorCode the error code
   * @param offset    the coded location of the error, or -1 if the location of the current token should be used
   * @throws XPathException always thrown: an exception containing the
   *                        supplied message
   */
  @throws[XPathException]
  def grumble(message: String, errorCode: StructuredQName, offset: Int): Unit = {
    var errCode = errorCode
    if (errCode == null)
      errCode = new StructuredQName("err", NamespaceConstant.ERR, "XPST0003")
    val nearbyText = t.recentText(-1)
    var line = 0
    var column = 0
    if (offset == -1) {
      line = t.getLineNumber
      column = t.getColumnNumber
    }
    else {
      line = t.getLineNumber(offset)
      column = t.getColumnNumber(offset)
    }
    val loc = makeNestedLocation(env.getContainingLocation, line, column, nearbyText)
    val err = new XPathException(message)
    err.setLocation(loc)
    err.setIsSyntaxError("XPST0003" == errCode.getLocalPart)
    err.setIsStaticError(true)
    err.setHostLanguage(getLanguage)
    //        err.setAdditionalLocationText(prefix);
    err.setErrorCodeQName(errCode)
    throw err
  }

  /**
   * Output a warning message
   *
   * @param message the text of the message
   */
  def warning(message: String): Unit = if (!env.getConfiguration.getBooleanProperty(Feature.SUPPRESS_XPATH_WARNINGS)) {
    val s = t.recentText(-1)
    val prefix = (if (message.startsWith("...")) "near"
    else "in") + ' ' + Err.wrap(s) + ":\n    "
    env.issueWarning(prefix + message, makeLocation)
  }

  import XPathParser.ParsedLanguage._

  def setLanguage(language: ParsedLanguage, version: Int): Unit = {
    var verson = version
    if (verson == 0) verson = 30 // default
    if (verson == 305) {
      verson = 30
      allowXPath30XSLTExtensions = true
    }
    language match {
      case XPATH =>
        if (!(verson == 20 || verson == 30 || verson == 31)) throw new IllegalArgumentException("Unsupported language version " + verson)
      case XSLT_PATTERN | SEQUENCE_TYPE =>
        if (!(verson == 20 || verson == 30 || verson == 31)) throw new IllegalArgumentException("Unsupported language version " + verson)
      case XQUERY =>
        if (!(verson == 10 || verson == 30 || verson == 31)) throw new IllegalArgumentException("Unsupported language version " + verson)
      case _ =>
        throw new IllegalArgumentException("Unknown language " + language)
    }
    this.language = language
    this.languageVersion = verson
    this.allowXPath30Syntax = languageVersion >= 30
    this.allowXPath31Syntax = languageVersion >= 31
  }

  /**
   * Get the current language (XPath or XQuery)
   *
   * @return a string representation of the language being parsed, for use in error messages
   */
  def getLanguage: String = language match {
    case XPATH =>
      "XPath"
    case XSLT_PATTERN =>
      "XSLT Pattern"
    case SEQUENCE_TYPE =>
      "SequenceType"
    case XQUERY =>
      "XQuery"
    case EXTENDED_ITEM_TYPE =>
      "Extended ItemType"
    case _ =>
      "XPath"
  }

  /**
   * Ask if XPath 3.1 is in use
   *
   * @return true if XPath 3.1 syntax (and therefore XQuery 3.1 syntax) is permitted
   */
  def isAllowXPath31Syntax: Boolean = allowXPath31Syntax

  /**
   * Set the QNameParser to be used while parsing
   *
   * @param qp the QNameParser
   */
  def setQNameParser(qp: QNameParser): Unit = this.qNameParser = qp

  /**
   * Get the QNameParser to be used while parsing
   *
   * @return the QNameParser
   */
  def getQNameParser: QNameParser = qNameParser

  /**
   * Display the current token in an error message
   *
   * @return the display representation of the token
   */
  /*@NotNull*/
  def currentTokenDisplay: String = if (t.currentToken == Token.NAME) "name \"" + t.currentTokenValue + '\"'
  else if (t.currentToken == Token.UNKNOWN) "(unknown token)"
  else "\"" + Token.tokens(t.currentToken).toString() + "\""

  /**
   * Parse a string representing an expression. This will accept an XPath expression if called on an
   * ExpressionParser, or an XQuery expression if called on a QueryParser.
   *
   * @param expression the expression expressed as a String
   * @param start      offset within the string where parsing is to start
   * @param terminator token to treat as terminating the expression
   * @param env        the static context for the expression
   * @return an Expression object representing the result of parsing
   * @throws XPathException if the expression contains a syntax error
   */
  @throws[XPathException]
  def parse(expression: String, start: Int, terminator: Int, env: StaticContext): Expression = {
    // System.err.println("Parse expression: " + expression);
    this.env = env
    var languageVersion = env.getXPathVersion
    if (languageVersion == 20 && (language eq ParsedLanguage.XQUERY)) languageVersion = 10
    setLanguage(language, languageVersion)
    var exp: Expression = null
    var offset: Int = start
    if (accelerator != null && (env.getUnprefixedElementMatchingPolicy eq UnprefixedElementMatchingPolicy.DEFAULT_NAMESPACE)
      && terminator != Token.IMPLICIT_EOF && (expression.length - start < 30 || terminator == Token.RCURLY)) {
      t = new Tokenizer
      t.languageLevel = env.getXPathVersion
      exp = accelerator.parse(t, env, expression, start, terminator)
    }
    if (exp == null) {
      qNameParser = new QNameParser(env.getNamespaceResolver).withAcceptEQName(allowXPath30Syntax).withErrorOnBadSyntax(if (language eq ParsedLanguage.XSLT_PATTERN) "XTSE0340"
      else "XPST0003").withErrorOnUnresolvedPrefix("XPST0081")
      charChecker = env.getConfiguration.getValidCharacterChecker
      t = new Tokenizer
      t.languageLevel = env.getXPathVersion
      t.allowSaxonExtensions = env.getConfiguration.getBooleanProperty(Feature.ALLOW_SYNTAX_EXTENSIONS)
      allowSaxonExtensions = t.allowSaxonExtensions
      offset = t.currentTokenStartOffset
      customizeTokenizer(t)
      try t.tokenize(expression, start, -1)
      catch {
        case err: XPathException =>
          grumble(err.getMessage)
      }

      if (t.currentToken == terminator)
        if (allowAbsentExpression) {
          val result = Literal.makeEmptySequence
          result.setRetainedStaticContext(env.makeRetainedStaticContext)
          setLocation(result)
          return result
        } else
          grumble("The expression is empty")

      exp = parseExpression
      if (t.currentToken != terminator && terminator != Token.IMPLICIT_EOF)
        if (t.currentToken == Token.EOF && terminator == Token.RCURLY)
          grumble("Missing curly brace after expression in value template", "XTSE0350")
        else
          grumble("Unexpected token " + currentTokenDisplay + " beyond end of expression")
      setLocation(exp, offset)
    }
    exp.setRetainedStaticContextThoroughly(env.makeRetainedStaticContext)
    //exp.verifyParentPointers();
    exp
  }

  /**
   * Callback to tailor the tokenizer
   *
   * @param t the Tokenizer to be customized
   */
  def customizeTokenizer(t: Tokenizer): Unit = {
    // do nothing
  }

  /**
   * Parse a string representing a sequence type
   *
   * @param input the string, which should conform to the XPath SequenceType
   *              production
   * @param env   the static context
   * @return a SequenceType object representing the type
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseSequenceType(input: String, env: StaticContext): SequenceType = {
    this.env = env
    if (qNameParser == null) {
      qNameParser = new QNameParser(env.getNamespaceResolver)
      if (languageVersion >= 30) qNameParser = qNameParser.withAcceptEQName(true)
    }
    language = ParsedLanguage.SEQUENCE_TYPE
    t = new Tokenizer
    t.languageLevel = env.getXPathVersion
    t.allowSaxonExtensions = env.getConfiguration.getBooleanProperty(Feature.ALLOW_SYNTAX_EXTENSIONS)
    allowSaxonExtensions = t.allowSaxonExtensions
    try t.tokenize(input, 0, -1)
    catch {
      case err: XPathException =>
        grumble(err.getMessage)
    }
    val req: SequenceType = parseSequenceType
    if (t.currentToken != Token.EOF) grumble("Unexpected token " + currentTokenDisplay + " beyond end of SequenceType")
    req
  }

  /**
   * Parse a string representing an extended item type: specifically, the content of itemType
   * or nodeTest attributes in an exported package. As well as regular itemType syntax, these
   * allow combined node tests separated with "|", "except", or "intersect" operators. Expressions
   * using these operators will always be parenthesized.
   *
   * @param input the string, which should conform to the XPath SequenceType
   *              production
   * @param env   the static context
   * @return a SequenceType object representing the type
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseExtendedItemType(input: String, env: StaticContext): ItemType = {
    this.env = env
    language = ParsedLanguage.EXTENDED_ITEM_TYPE
    t = new Tokenizer
    t.languageLevel = env.getXPathVersion
    t.allowSaxonExtensions = true
    allowSaxonExtensions = t.allowSaxonExtensions
    try t.tokenize(input, 0, -1)
    catch {
      case err: XPathException =>
        grumble(err.getMessage)
    }
    val req = parseItemType
    if (t.currentToken != Token.EOF) grumble("Unexpected token " + currentTokenDisplay + " beyond end of ItemType")
    req
  }

  /**
   * Parse a string representing a sequence type with syntax extensions used in exported stylesheets.
   * Also allows the extensions permitted in saxon:as, e.g. tuple types, type aliases
   *
   * @param input the string, which should conform to the XPath SequenceType
   *              production
   * @param env   the static context
   * @return a SequenceType object representing the type
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseExtendedSequenceType(input: String, env: StaticContext): SequenceType = {
    this.env = env
    language = ParsedLanguage.EXTENDED_ITEM_TYPE
    t = new Tokenizer
    t.languageLevel = env.getXPathVersion
    t.allowSaxonExtensions = true
    allowSaxonExtensions = t.allowSaxonExtensions
    try t.tokenize(input, 0, -1)
    catch {
      case err: XPathException =>
        grumble(err.getMessage)
    }
    val req = parseSequenceType
    if (t.currentToken != Token.EOF) grumble("Unexpected token " + currentTokenDisplay + " beyond end of SequenceType")
    req
  }

  /**
   * Parse a top-level Expression:
   * ExprSingle ( ',' ExprSingle )*
   *
   * @return the Expression object that results from parsing
   * @throws XPathException if the expression contains a syntax error
   */
  @throws[XPathException]
  def parseExpression: Expression = {
    val offset = t.currentTokenStartOffset
    var exp = parseExprSingle
    var list: util.List[Expression] = null
    while (t.currentToken == Token.COMMA) {
      // An expression containing a comma often contains many, so we accumulate all the
      // subexpressions into a list before creating the Block expression which reduces it to an array
      if (list == null) {
        list = new util.ArrayList[Expression](10)
        list.add(exp)
      }
      nextToken()
      val next = parseExprSingle
      setLocation(next)
      list.add(next)
    }
    if (list != null) {
      exp = Block.makeBlock(list)
      setLocation(exp, offset)
    }
    exp
  }

  /**
   * Parse an ExprSingle
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseExprSingle: Expression = {
    val e = parserExtension.parseExtendedExprSingle(this)
    if (e != null)
      return e
    // Short-circuit for a single-token expression
    val peek = t.peekAhead
    if (peek == Token.EOF || peek == Token.COMMA || peek == Token.RPAR || peek == Token.RSQB)
      t.currentToken match {
        case Token.STRING_LITERAL =>
          return parseStringLiteral(true)
        case Token.NUMBER =>
          return parseNumericLiteral(true)
        case Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR =>
          return parseBasicStep(true)
        case Token.DOT =>
          nextToken()
          val cie = new ContextItemExpression
          setLocation(cie)
          return cie
        case Token.DOTDOT =>
          nextToken()
          val pne = new AxisExpression(AxisInfo.PARENT, null)
          setLocation(pne)
          return pne
        case Token.EOF =>
        case _ =>
      }

    t.currentToken match {
      case Token.EOF =>
        grumble("Expected an expression, but reached the end of the input")
        null
      case Token.FOR |  Token.LET |  Token.FOR_SLIDING |  Token.FOR_TUMBLING =>
        parseFLWORExpression
      case Token.SOME | Token.EVERY =>
        parseQuantifiedExpression
      case Token.FOR_MEMBER =>
        parserExtension.parseForMemberExpression(this)
      case Token.IF =>
        parseIfExpression
      case Token.SWITCH =>
        parseSwitchExpression
      case Token.TYPESWITCH =>
        parseTypeswitchExpression
      case Token.VALIDATE | Token.VALIDATE_STRICT | Token.VALIDATE_LAX | Token.VALIDATE_TYPE =>
        parseValidateExpression
      case Token.PRAGMA =>
        parseExtensionExpression // XQuery only
      case Token.KEYWORD_CURLY =>
        if (t.currentTokenValue == "try") parseTryCatchExpression else null
      case _ =>
        parseBinaryExpression(parseUnaryExpression, 4)
    }
  }

  /**
   * Parse a binary expression, using operator precedence parsing. This is used
   * to parse the part of the grammar consisting largely of binary operators
   * distinguished by precedence: from "or expressions" down to "unary expressions".
   * Algorithm for the mainstream binary operators is from Wikipedia article
   * on precedence parsing;  operator precedences are from the XQuery specification
   * appendix B.
   *
   * @param lhs           Left-hand side "basic expression"
   * @param minPrecedence the minimum precedence of an operator that is to be treated as not terminating the
   *                      current expression
   * @return the parsed expression
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseBinaryExpression(lhs: Expression, minPrecedence: Int): Expression = {
    var lhsExp = lhs
    while (getCurrentOperatorPrecedence >= minPrecedence) {
      val offset = t.currentTokenStartOffset
      val operator = t.currentToken
      val prec = getCurrentOperatorPrecedence
      operator match {
        case Token.INSTANCE_OF | Token.TREAT_AS =>
          nextToken()
          val seq = parseSequenceType
          lhsExp = makeSequenceTypeExpression(lhsExp, operator, seq)
          setLocation(lhsExp, offset)
          if (getCurrentOperatorPrecedence >= prec)
            grumble("Left operand of '" + Token.tokens(t.currentToken) + "' needs parentheses")
        case Token.CAST_AS | Token.CASTABLE_AS =>
          nextToken()
          var at: CastingTarget = null
          if (allowSaxonExtensions && t.currentToken == Token.NODEKIND && t.currentTokenValue == "union") {
            // Saxon 9.8 extension
            at = parseItemType.asInstanceOf[CastingTarget]
          } else {
            expect(Token.NAME)
            at = getSimpleType(t.currentTokenValue)
            if (at eq ANY_ATOMIC)
              grumble("No value is castable to xs:anyAtomicType", "XPST0080")
            if (at eq NOTATION)
              grumble("No value is castable to xs:NOTATION", "XPST0080")
            nextToken()
          }
          val allowEmpty = t.currentToken == Token.QMARK
          if (allowEmpty)
            nextToken()
          lhsExp = makeSingleTypeExpression(lhsExp, operator, at, allowEmpty)
          setLocation(lhsExp, offset)
          if (getCurrentOperatorPrecedence >= prec)
            grumble("Left operand of '" + Token.tokens(t.currentToken) + "' needs parentheses")
        case Token.ARROW =>
          lhsExp = parseArrowPostfix(lhsExp)
        case _ =>
          nextToken()
          var rhs = parseUnaryExpression
          while (getCurrentOperatorPrecedence > prec)
            rhs = parseBinaryExpression(rhs, getCurrentOperatorPrecedence)

          if (getCurrentOperatorPrecedence == prec && ! allowMultipleOperators) {
            val tok = Token.tokens(t.currentToken)
            var message = "Left operand of '" + Token.tokens(t.currentToken) + "' needs parentheses"
            if (tok == "<" || tok == ">") {
              // Example input: return <a>3</a><b>4</b> - bug 2659
              message += ". Or perhaps an XQuery element constructor appears where it is not allowed"
            }
            grumble(message)
          }
          lhsExp = makeBinaryExpression(lhsExp, operator, rhs)
          setLocation(lhsExp, offset)
      }
    }
    lhsExp
  }

  private def allowMultipleOperators: Boolean = t.currentToken match {
    case Token.FEQ | Token.FNE | Token.FLE | Token.FLT | Token.FGE | Token.FGT | Token.EQUALS | Token.NE |
         Token.LE | Token.LT | Token.GE | Token.GT | Token.IS | Token.PRECEDES | Token.FOLLOWS | Token.TO =>
      false
    case _ =>
      true
  }

  private def getCurrentOperatorPrecedence: Int = XPathParser.operatorPrecedence(t.currentToken)

  @throws[XPathException]
  private def makeBinaryExpression(lhs: Expression, operator: Int, rhs: Expression): Expression = {

    operator match {
      case Token.OR =>
        new OrExpression(lhs, rhs)
      case Token.AND =>
        new AndExpression(lhs, rhs)
      case Token.FEQ | Token.FNE | Token.FLE | Token.FLT | Token.FGE | Token.FGT =>
        new ValueComparison(lhs, operator, rhs)
      case Token.EQUALS | Token.NE | Token.LE | Token.LT | Token.GE | Token.GT =>
        env.getConfiguration.getTypeChecker(env.isInBackwardsCompatibleMode).makeGeneralComparison(lhs, operator, rhs)
      case Token.IS | Token.PRECEDES | Token.FOLLOWS =>
        new IdentityComparison(lhs, operator, rhs)
      case Token.TO =>
        new RangeExpression(lhs, rhs)
      case Token.CONCAT =>
        if (! allowXPath30Syntax)
          grumble("Concatenation operator ('||') requires XPath 3.0 to be enabled")
        val rsc = new RetainedStaticContext(env)
        if (lhs.isCallOn(classOf[Concat])) {
          val args = lhs.asInstanceOf[SystemFunctionCall].getArguments
          val newArgs = new Array[Expression](args.length + 1)
          System.arraycopy(args, 0, newArgs, 0, args.length)
          newArgs(args.length) = rhs
          SystemFunction.makeCall("concat", rsc, newArgs.toIndexedSeq: _*)
        } else
          SystemFunction.makeCall("concat", rsc, lhs, rhs)
      case Token.PLUS | Token.MINUS | Token.MULT | Token.DIV | Token.IDIV | Token.MOD =>
        env.getConfiguration.getTypeChecker(env.isInBackwardsCompatibleMode).makeArithmeticExpression(lhs, operator, rhs)
      case Token.OTHERWISE =>
        makeOtherwiseExpression(lhs, rhs)
      case Token.UNION | Token.INTERSECT | Token.EXCEPT =>
        new VennExpression(lhs, operator, rhs)
      case Token.OR_ELSE =>
        val rsc = new RetainedStaticContext(env)
        Choose.makeConditional(lhs, Literal.makeLiteral(BooleanValue.TRUE), SystemFunction.makeCall("boolean", rsc, rhs))
      case Token.AND_ALSO =>
        val rsc = new RetainedStaticContext(env)
        Choose.makeConditional(lhs, SystemFunction.makeCall("boolean", rsc, rhs), Literal.makeLiteral(BooleanValue.FALSE))
      case _ =>
        throw new IllegalArgumentException(Token.tokens(operator))
    }
  }

  /**
   * Saxon extension: A otherwise B, returns if (exists(A)) then A else B
   *
   * @param lhs the A expression
   * @param rhs the B expression
   * @return a conditional expression with the correct semantics
   */
  private def makeOtherwiseExpression(lhs: Expression, rhs: Expression) = {
    val let = new LetExpression
    let.setVariableQName(new StructuredQName("vv", NamespaceConstant.ANONYMOUS, "n" + lhs.hashCode))
    let.setSequence(lhs)
    let.setRequiredType(SequenceType.ANY_SEQUENCE)
    val v1 = new LocalVariableReference(let.getVariableQName)
    v1.setBinding(let)
    let.addReference(v1, isLoopingReference = false)
    val v2 = new LocalVariableReference(let.getVariableQName)
    v2.setBinding(let)
    let.addReference(v2, isLoopingReference = false)
    val rsc = new RetainedStaticContext(env)
    val conditions = Array[Expression](SystemFunction.makeCall("exists", rsc, v1), Literal.makeLiteral(BooleanValue.TRUE, lhs))
    val actions = Array[Expression](v2, rhs)
    let.setAction(new Choose(conditions, actions))
    let
  }

  private def makeSequenceTypeExpression(lhs: Expression, operator: Int, `type`: SequenceType) = operator match {
    case Token.INSTANCE_OF =>
      new InstanceOfExpression(lhs, `type`)
    case Token.TREAT_AS =>
      TreatExpression.make(lhs, `type`)
    case _ =>
      throw new IllegalArgumentException
  }

  @throws[XPathException]
  private def makeSingleTypeExpression(lhs: Expression, operator: Int, `type`: CastingTarget, allowEmpty: Boolean): Expression = if (`type`.isInstanceOf[AtomicType] && !(`type` eq ErrorType.getInstance)) operator match {
    case Token.CASTABLE_AS =>
      val castable = new CastableExpression(lhs, `type`.asInstanceOf[AtomicType], allowEmpty)
      if (lhs.isInstanceOf[StringLiteral]) castable.setOperandIsStringLiteral(true)
      castable
    case Token.CAST_AS =>
      val cast = new CastExpression(lhs, `type`.asInstanceOf[AtomicType], allowEmpty)
      if (lhs.isInstanceOf[StringLiteral]) cast.setOperandIsStringLiteral(true)
      cast
    case _ =>
      throw new IllegalArgumentException
  }
  else if (allowXPath30Syntax) {
    operator match {
      case Token.CASTABLE_AS =>
        if (`type`.isInstanceOf[UnionType]) {
          val resolver = env.getNamespaceResolver
          val ucf = new UnionCastableFunction(`type`.asInstanceOf[UnionType], resolver, allowEmpty)
          return new StaticFunctionCall(ucf, Array[Expression](lhs))
        }
        else if (`type`.isInstanceOf[ListType]) {
          val resolver = env.getNamespaceResolver
          val lcf = new ListCastableFunction(`type`.asInstanceOf[ListType], resolver, allowEmpty)
          return new StaticFunctionCall(lcf, Array[Expression](lhs))
        }
      case Token.CAST_AS =>
        if (`type`.isInstanceOf[UnionType]) {
          val resolver = env.getNamespaceResolver
          val ucf = new UnionConstructorFunction(`type`.asInstanceOf[UnionType], resolver, allowEmpty)
          return new StaticFunctionCall(ucf, Array[Expression](lhs))
        }
        else if (`type`.isInstanceOf[ListType]) {
          val resolver = env.getNamespaceResolver
          val lcf = new ListConstructorFunction(`type`.asInstanceOf[ListType], resolver, allowEmpty)
          return new StaticFunctionCall(lcf, Array[Expression](lhs))
        }
      case _ =>
        throw new IllegalArgumentException
    }
    //            if (type == AnySimpleType()) {
    //                throw new XPathException("Cannot cast to xs:anySimpleType", "XPST0080");
    //            } else {
    throw new XPathException("Cannot cast to " + `type`.getClass, "XPST0051")
    //            }
  }
  else throw new XPathException("Casting to list or union types requires XPath 3.0 to be enabled", "XPST0051")

  /**
   * Parse a Typeswitch Expression.
   * This construct is XQuery-only, so the XPath version of this
   * method throws an error unconditionally
   *
   * @return the expression that results from the parsing
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseTypeswitchExpression: Expression = {
    grumble("typeswitch is not allowed in XPath")
    new ErrorExpression
  }

  /**
   * Parse a Switch Expression.
   * This construct is XQuery-only.
   * SwitchExpr ::= "switch" "(" Expr ")" SwitchCaseClause+ "default" "return" ExprSingle
   * SwitchCaseClause ::= ("case" ExprSingle)+ "return" ExprSingle
   *
   * @return the parsed expression
   * @throws XPathException in the event of a syntax error
   */
  @throws[XPathException]
  def parseSwitchExpression: Expression = {
    grumble("switch is not allowed in XPath")
    new ErrorExpression
  }

  /**
   * Parse a Validate Expression.
   * This construct is XQuery-only, so the XPath version of this
   * method throws an error unconditionally
   *
   * @return the parsed expression; except that this version of the method always
   *         throws an exception
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseValidateExpression: Expression = {
    grumble("validate{} expressions are not allowed in XPath")
    new ErrorExpression
  }

  /**
   * Parse an Extension Expression
   * This construct is XQuery-only, so the XPath version of this
   * method throws an error unconditionally
   *
   * @return the parsed expression; except that this version of the method
   *         always throws an exception
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseExtensionExpression: Expression = {
    grumble("extension expressions (#...#) are not allowed in XPath")
    new ErrorExpression
  }

  /**
   * Parse a try/catch Expression
   * This construct is XQuery-3.0 only, so the XPath version of this
   * method throws an error unconditionally
   *
   * @return the parsed expression; except that this version of the method
   *         always throws an exception
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseTryCatchExpression: Expression = {
    grumble("try/catch expressions are not allowed in XPath")
    new ErrorExpression
  }

  /**
   * Parse a FOR or LET expression:
   * for $x in expr (',' $y in expr)* 'return' expr
   * let $x := expr (', $y := expr)* 'return' expr
   * This version of the method handles the subset of the FLWOR syntax allowed in XPath
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseFLWORExpression: Expression = {
    if (t.currentToken == Token.LET && ! allowXPath30Syntax)
      grumble("'let' is not permitted in XPath 2.0")
    if (t.currentToken == Token.FOR_SLIDING || t.currentToken == Token.FOR_TUMBLING)
      grumble("sliding/tumbling windows can only be used in XQuery")
    var clauses = 0
    var offset = 0
    val operator: Int = t.currentToken
    var first: Assignation = null
    var previous: Assignation = null
    do {
      offset = t.currentTokenStartOffset
      nextToken()
      expect(Token.DOLLAR)
      nextToken()
      expect(Token.NAME)
      val `var` = t.currentTokenValue
      // declare the range variable
      var v: Assignation = null
      if (operator == Token.FOR) {
        v = new ForExpression
        v.setRequiredType(SequenceType.SINGLE_ITEM)
      } else {
        v = new LetExpression
        v.setRequiredType(SequenceType.ANY_SEQUENCE)
      }
      clauses += 1
      setLocation(v, offset)
      v.setVariableQName(makeStructuredQName(`var`, ""))
      nextToken()
      // process the "in" or ":=" clause
      expect(if (operator == Token.LET) Token.ASSIGN else Token.IN)
      nextToken()
      v.setSequence(parseExprSingle)
      declareRangeVariable(v)
      if (previous == null)
        first = v
      else
        previous.setAction(v)
      previous = v
    } while (t.currentToken == Token.COMMA)

    // process the "return" expression (called the "action")
    expect(Token.RETURN)

    nextToken()
    previous.setAction(parseExprSingle)
    // undeclare all the range variables
    for (i <- 0 until clauses) {
      undeclareRangeVariable()
    }
    makeTracer(first, first.getVariableQName)
  }

  /**
   * Parse a quantified expression:
   * (some|every) $x in expr (',' $y in expr)* 'satisfies' expr
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  private def parseQuantifiedExpression = {
    var clauses = 0
    val operator = t.currentToken
    var first: QuantifiedExpression = null
    var previous: QuantifiedExpression = null
    do {
      val offset = t.currentTokenStartOffset
      nextToken()
      expect(Token.DOLLAR)
      nextToken()
      expect(Token.NAME)
      val `var` = t.currentTokenValue
      clauses += 1
      val v: QuantifiedExpression = new QuantifiedExpression
      v.setRequiredType(SequenceType.SINGLE_ITEM)
      v.setOperator(operator)
      setLocation(v, offset)
      v.setVariableQName(makeStructuredQName(`var`, ""))
      nextToken()
      if (t.currentToken == Token.AS && (language eq ParsedLanguage.XQUERY)) { // We use this path for quantified expressions in XQuery, which permit an "as" clause
        nextToken()
        var `type` = parseSequenceType
        if (`type`.getCardinality != StaticProperty.EXACTLY_ONE) {
          warning("Occurrence indicator on singleton range variable has no effect")
          `type` = SequenceType.makeSequenceType(`type`.getPrimaryType, StaticProperty.EXACTLY_ONE)
        }
        v.setRequiredType(`type`)
      }
      // process the "in" clause
      expect(Token.IN)
      nextToken()
      v.setSequence(parseExprSingle)
      declareRangeVariable(v)
      if (previous != null) previous.setAction(v)
      else first = v
      previous = v
    } while ( {
      t.currentToken == Token.COMMA
    })
    // process the "return/satisfies" expression (called the "action")
    expect(Token.SATISFIES)
    nextToken()
    previous.setAction(parseExprSingle)
    for (i <- 0 until clauses) {
      undeclareRangeVariable()
    }
    makeTracer(first, first.getVariableQName)
  }

  /**
   * Parse an IF expression:
   * if '(' expr ')' 'then' expr 'else' expr
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  private def parseIfExpression = { // left paren already read
    val ifoffset = t.currentTokenStartOffset
    nextToken()
    val condition = parseExpression
    expect(Token.RPAR)
    nextToken()
    val thenoffset = t.currentTokenStartOffset
    expect(Token.THEN)
    nextToken()
    val thenExp = makeTracer(parseExprSingle, null)
    val elseoffset = t.currentTokenStartOffset
    expect(Token.ELSE)
    nextToken()
    val elseExp = makeTracer(parseExprSingle, null)
    val ifExp = Choose.makeConditional(condition, thenExp, elseExp)
    setLocation(ifExp, ifoffset)
    makeTracer(ifExp, null)
  }

  /**
   * Analyze a token whose expected value is the name of an atomic type,
   * or in XPath 3.0 a "plain" union type and return the object representing the atomic or union type.
   *
   * @param qname The lexical QName of the atomic type; alternatively, a Clark name
   * @return The atomic type
   * @throws XPathException if the QName is invalid or if no atomic type of that
   *                        name exists as a built-in type or a type in an imported schema
   */
  @throws[XPathException]
  private def getPlainType(qname: String): ItemType = {
    if (scanOnly) return STRING
    var sq: StructuredQName = null
    try sq = qNameParser.parse(qname, env.getDefaultElementNamespace)
    catch {
      case e: XPathException =>
        grumble(e.getMessage, e.getErrorCodeLocalPart)
        return null
    }
    getPlainType(sq)
  }

  @throws[XPathException]
  def getPlainType(sq: StructuredQName): ItemType = {
    val config = env.getConfiguration
    var uri = sq.getURI
    if (uri.isEmpty) uri = env.getDefaultElementNamespace
    val local = sq.getLocalPart
    val qname = sq.getDisplayName
    val builtInNamespace = uri == NamespaceConstant.SCHEMA
    if (builtInNamespace) {
      val t = Type.getBuiltInItemType(uri, local)
      if (t == null) {
        grumble("Unknown atomic type " + qname, "XPST0051")
        assert(false)
      }
      if (t.isInstanceOf[BuiltInAtomicType]) {
        checkAllowedType(env, t.asInstanceOf[BuiltInAtomicType])
        return t
      }
      else if (t.isPlainType) return t
      else {
        grumble("The type " + qname + " is not atomic", "XPST0051")
        assert(false)
      }
    }
    else if (uri == NamespaceConstant.JAVA_TYPE) {
      var theClass: Class[_] = null
      try {
        val className = JavaExternalObjectType.localNameToClassName(local)
        theClass = config.getConfClass(className, tracing = false, null)
      } catch {
        case err: XPathException =>
          grumble("Unknown Java class " + local, "XPST0051")
          return AnyItemType
      }
      return config.getJavaExternalObjectType(theClass)
    }
    else if (uri == NamespaceConstant.DOT_NET_TYPE) return Version.platform.getExternalObjectType(config, uri, local)
    else {
      val st = config.getSchemaType(sq)
      if (st == null) grumble("Unknown simple type " + qname, "XPST0051")
      else if (st.isAtomicType) {
        if (!env.isImportedSchema(uri)) grumble("Atomic type " + qname + " exists, but its schema definition has not been imported", "XPST0051")
        return st.asInstanceOf[AtomicType]
      }
      else if (st.isInstanceOf[ItemType] && st.asInstanceOf[ItemType].isPlainType && allowXPath30Syntax) {
        if (!env.isImportedSchema(uri)) grumble("Type " + qname + " exists, but its schema definition has not been imported", "XPST0051")
        return st.asInstanceOf[ItemType]
      }
      else if (st.isComplexType) {
        grumble("Type (" + qname + ") is a complex type", "XPST0051")
        return ANY_ATOMIC
      }
      else if (st.asInstanceOf[SimpleType].isListType) {
        grumble("Type (" + qname + ") is a list type", "XPST0051")
        return ANY_ATOMIC
      }
      else if (allowXPath30Syntax) {
        grumble("Type (" + qname + ") is a union type that cannot be used as an item type", "XPST0051")
        return ANY_ATOMIC
      }
      else {
        grumble("The union type (" + qname + ") cannot be used as an item type unless XPath 3.0 is enabled", "XPST0051")
        return ANY_ATOMIC
      }
    }
    grumble("Unknown atomic type " + qname, "XPST0051")
    ANY_ATOMIC
  }

  @throws[XPathException]
  private def checkAllowedType(env: StaticContext, `type`: BuiltInAtomicType): Unit = {
    val s = XPathParser.whyDisallowedType(env.getPackageData, `type`)
    if (s != null) grumble(s, "XPST0080")
  }

  /**
   * Analyze a token whose expected value is the name of a simple type: any type name
   * allowed as the operand of "cast" or "castable".
   *
   * @param qname The lexical QName of the atomic type; alternatively, a Clark name
   * @return The atomic type
   * @throws XPathException if the QName is invalid or if no atomic type of that
   *                        name exists as a built-in type or a type in an imported schema
   */
  @throws[XPathException]
  private def getSimpleType(qname: String): CastingTarget = {
    if (scanOnly) return STRING
    var sq: StructuredQName = null
    try sq = qNameParser.parse(qname, env.getDefaultElementNamespace)
    catch {
      case e: XPathException =>
        grumble(e.getMessage, e.getErrorCodeLocalPart)
        assert(false)
    }
    val uri = sq.getURI
    val local = sq.getLocalPart
    val builtInNamespace = uri == NamespaceConstant.SCHEMA
    if (builtInNamespace) {
      val target = Type.getBuiltInSimpleType(uri, local)
      if (target == null) grumble("Unknown simple type " + qname, if (allowXPath30Syntax) "XQST0052"
      else "XPST0051")
      else if (!target.isInstanceOf[CastingTarget]) grumble("Unsuitable type for cast: " + target.getDescription, "XPST0080")
      val t = target.asInstanceOf[CastingTarget]
      if (t.isInstanceOf[BuiltInAtomicType]) checkAllowedType(env, t.asInstanceOf[BuiltInAtomicType])
      t
    }
    else if (uri == NamespaceConstant.DOT_NET_TYPE) Version.platform.getExternalObjectType(env.getConfiguration, uri, local).asInstanceOf[AtomicType]
    else {
      val st = env.getConfiguration.getSchemaType(new StructuredQName("", uri, local))
      if (st == null) {
        if (allowXPath30Syntax) grumble("Unknown simple type " + qname, "XQST0052")
        else grumble("Unknown simple type " + qname, "XPST0051")
        return ANY_ATOMIC
      }
      if (allowXPath30Syntax) { // XPath 3.0
        if (!env.isImportedSchema(uri)) grumble("Simple type " + qname + " exists, but its target namespace has not been imported in the static context")
        st.asInstanceOf[CastingTarget]
      }
      else { // XPath 2.0
        if (st.isAtomicType) {
          if (!env.isImportedSchema(uri)) grumble("Atomic type " + qname + " exists, but its target namespace has not been imported in the static context")
          st.asInstanceOf[AtomicType]
        }
        else if (st.isComplexType) {
          grumble("Cannot cast to a complex type (" + qname + ")", "XPST0051")
          ANY_ATOMIC
        }
        else if (st.asInstanceOf[SimpleType].isListType) {
          grumble("Casting to a list type (" + qname + ") requires XPath 3.0", "XPST0051")
          ANY_ATOMIC
        }
        else {
          grumble("casting to a union type (" + qname + ") requires XPath 3.0", "XPST0051")
          ANY_ATOMIC
        }
      }
    }
  }

  /**
   * Parse the sequence type production.
   * The QName must be the name of a built-in schema-defined data type.
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseSequenceType: SequenceType = {
    val disallowIndicator = t.currentTokenValue == "empty-sequence"
    val primaryType = parseItemType
    if (disallowIndicator) { // No occurrence indicator allowed
      return SequenceType.makeSequenceType(primaryType, StaticProperty.EMPTY)
    }
    var occurrenceFlag = 0
    t.currentToken match {
      case Token.STAR | Token.MULT =>
        // "*" will be tokenized different ways depending on what precedes it
        occurrenceFlag = StaticProperty.ALLOWS_ZERO_OR_MORE
        // Make the tokenizer ignore the occurrence indicator when classifying the next token
        t.currentToken = Token.RPAR
        nextToken()
      case Token.PLUS =>
        occurrenceFlag = StaticProperty.ALLOWS_ONE_OR_MORE
        t.currentToken = Token.RPAR
        nextToken()
      case Token.QMARK =>
        occurrenceFlag = StaticProperty.ALLOWS_ZERO_OR_ONE
        t.currentToken = Token.RPAR
        nextToken()
      case _ =>
        occurrenceFlag = StaticProperty.EXACTLY_ONE
    }
    SequenceType.makeSequenceType(primaryType, occurrenceFlag)
  }

  /**
   * Parse an ItemType within a SequenceType
   *
   * @return the ItemType after parsing
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseItemType: ItemType = {
    val extended = parserExtension.parseExtendedItemType(this)
    if (extended == null) parseSimpleItemType
    else extended
  }

  @throws[XPathException]
  private def parseSimpleItemType: ItemType = {
    var primaryType: ItemType = null
    if (t.currentToken == Token.LPAR) {
      primaryType = parseParenthesizedItemType
      //nextToken();
    }
    else if (t.currentToken == Token.NAME) {
      primaryType = getPlainType(t.currentTokenValue)
      nextToken()
    }
    else if (t.currentToken == Token.NODEKIND) { // Which includes things such as "map" and "array"...
      t.currentTokenValue match {
        case "item" =>
          nextToken()
          expect(Token.RPAR)
          nextToken()
          primaryType = AnyItemType
        case "function" =>
          checkLanguageVersion30()
          val annotations = AnnotationList.EMPTY
          primaryType = parseFunctionItemType(annotations)
        case "map" =>
          primaryType = parseMapItemType
        case "array" =>
          primaryType = parseArrayItemType
        case "empty-sequence" =>
          nextToken()
          expect(Token.RPAR)
          nextToken()
          primaryType = ErrorType.getInstance
        case _ =>
          primaryType = parseKindTest
      }
    }
    else if (t.currentToken == Token.PERCENT) {
      val annotations = parseAnnotationsList
      if (t.currentTokenValue == "function") primaryType = parseFunctionItemType(annotations)
      else {
        grumble("Expected 'function' to follow annotation assertions, found " + Token.tokens(t.currentToken))
        return null
      }
    }
    else if ((language eq ParsedLanguage.EXTENDED_ITEM_TYPE) && t.currentToken == Token.PREFIX) {
      val tokv = t.currentTokenValue
      nextToken()
      return makeNamespaceTest(Type.ELEMENT, tokv)
    }
    else if ((language eq ParsedLanguage.EXTENDED_ITEM_TYPE) && t.currentToken == Token.SUFFIX) {
      nextToken()
      expect(Token.NAME)
      val tokv = t.currentTokenValue
      nextToken()
      return makeLocalNameTest(Type.ELEMENT, tokv)
    }
    else if ((language eq ParsedLanguage.EXTENDED_ITEM_TYPE) && t.currentToken == Token.AT) {
      nextToken()
      if (t.currentToken == Token.PREFIX) {
        val tokv = t.currentTokenValue
        nextToken()
        return makeNamespaceTest(Type.ATTRIBUTE, tokv)
      }
      else if (t.currentToken == Token.SUFFIX) {
        nextToken()
        expect(Token.NAME)
        val tokv = t.currentTokenValue
        nextToken()
        return makeLocalNameTest(Type.ATTRIBUTE, tokv)
      }
      else {
        grumble("Expected NodeTest after '@'")
        return ANY_ATOMIC
      }
    }
    else {
      grumble("Expected type name in SequenceType, found " + Token.tokens(t.currentToken))
      return ANY_ATOMIC
    }
    primaryType
  }

  @throws[XPathException]
  def parseFunctionItemType(annotations: AnnotationList): ItemType = parserExtension.parseFunctionItemType(this, annotations)

  /**
   * Parse the item type used for maps (XSLT extension to XPath 3.0)
   * Syntax:
   * map '(' '*' ') |
   * map '(' ItemType ',' SeqType ')' 'as' SeqType
   * The "map(" has already been read
   *
   * @return the item type of the map
   * @throws XPathException if a parsing error occurs or if the map syntax
   *                        is not available
   */
  @throws[XPathException]
  def parseMapItemType: ItemType = {
    checkMapExtensions()
    val t = getTokenizer
    nextToken()
    if (t.currentToken == Token.STAR || t.currentToken == Token.MULT) { // Allow both to be safe
      nextToken()
      expect(Token.RPAR)
      nextToken()
      MapType.ANY_MAP_TYPE
    }
    else {
      val keyType = parseItemType
      expect(Token.COMMA)
      nextToken()
      val valueType = parseSequenceType
      expect(Token.RPAR)
      nextToken()
      if (!keyType.isInstanceOf[AtomicType]) {
        grumble("Key type of a map must be atomic")
        return null
      }
      new MapType(keyType.asInstanceOf[AtomicType], valueType)
    }
  }

  /**
   * Get the item type used for array items (XPath 3.1)
   * Syntax:
   * array '(' '*' ') |
   * array '(' SeqType ')'
   * The "array(" has already been read
   *
   * @return the item type of the array
   * @throws XPathException if a parsing error occurs or if the array syntax
   *                        is not available
   */
  @throws[XPathException]
  def parseArrayItemType: ArrayItemType = {
    checkLanguageVersion31()
    val t = getTokenizer
    nextToken()
    if (t.currentToken == Token.STAR || t.currentToken == Token.MULT) {
      nextToken()
      expect(Token.RPAR)
      nextToken()
      ArrayItemType.ANY_ARRAY_TYPE
    }
    else {
      val memberType = parseSequenceType
      expect(Token.RPAR)
      nextToken()
      new ArrayItemType(memberType)
    }
  }

  /**
   * Parse a parenthesized item type (allowed in XQuery 3.0 and XPath 3.0 only)
   *
   * @return the item type
   * @throws XPathException in the event of a syntax error (or if 3.0 is not enabled)
   */
  @throws[XPathException]
  private def parseParenthesizedItemType = {
    if (!allowXPath30Syntax) grumble("Parenthesized item types require 3.0 to be enabled")
    nextToken()
    var primaryType = parseItemType
    while ( {
      primaryType.isInstanceOf[NodeTest] && (language eq ParsedLanguage.EXTENDED_ITEM_TYPE) && t.currentToken != Token.RPAR
    }) t.currentToken match {
      case Token.UNION | Token.EXCEPT | Token.INTERSECT =>
        val op = t.currentToken
        nextToken()
        primaryType = new CombinedNodeTest(primaryType.asInstanceOf[NodeTest], op, parseItemType.asInstanceOf[NodeTest])
    }
    expect(Token.RPAR)
    nextToken()
    primaryType
  }

  /**
   * Parse a UnaryExpr:<br>
   * ('+'|'-')* ValueExpr
   * parsed as ('+'|'-')? UnaryExpr
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  private def parseUnaryExpression: Expression = {
    var exp: Expression = null
    t.currentToken match {
      case Token.MINUS =>
        nextToken()
        val operand = parseUnaryExpression
        exp = makeUnaryExpression(Token.NEGATE, operand)
      case Token.PLUS =>
        nextToken()
        // Unary plus: can't ignore it completely, it might be a type error, or it might
        // force conversion to a number which would affect operations such as "=".
        val operand = parseUnaryExpression
        exp = makeUnaryExpression(Token.PLUS, operand)
      case Token.VALIDATE | Token.VALIDATE_STRICT | Token.VALIDATE_LAX | Token.VALIDATE_TYPE =>
        exp = parseValidateExpression
      case Token.PRAGMA =>
        exp = parseExtensionExpression
      case Token.KEYWORD_CURLY if t.currentTokenValue == "validate" =>
        exp = parseValidateExpression
      case _ =>
        exp = parseSimpleMappingExpression
    }
    setLocation(exp)
    exp
  }

  private def makeUnaryExpression(operator: Int, operand: Expression): Expression = {
    if (Literal.isAtomic(operand)) { // very early evaluation of expressions like "-1", so they are treated as numeric literals
      var `val` = operand.asInstanceOf[Literal].value.asInstanceOf[AtomicValue]
      if (`val`.isInstanceOf[NumericValue]) {
        if (env.isInBackwardsCompatibleMode) `val` = new DoubleValue(`val`.asInstanceOf[NumericValue].getDoubleValue)
        val value = if (operator == Token.NEGATE) `val`.asInstanceOf[NumericValue].negate
        else `val`.asInstanceOf[NumericValue]
        return Literal.makeLiteral(value)
      }
    }
    env.getConfiguration.getTypeChecker(env.isInBackwardsCompatibleMode).makeArithmeticExpression(Literal.makeLiteral(Int64Value.ZERO), operator, operand)
  }

  /**
   * Test whether the current token is one that can start a RelativePathExpression
   *
   * @return the resulting subexpression
   */
  def atStartOfRelativePath: Boolean = t.currentToken match {
    case Token.AXIS | Token.AT | Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND |
         Token.DOT | Token.DOTDOT | Token.FUNCTION | Token.STRING_LITERAL | Token.NUMBER | Token.LPAR |
         Token.DOLLAR | Token.PRAGMA | Token.ELEMENT_QNAME | Token.ATTRIBUTE_QNAME | Token.PI_QNAME |
         Token.NAMESPACE_QNAME | Token.NAMED_FUNCTION_REF =>
      true
    case Token.KEYWORD_CURLY =>
      t.currentTokenValue == "ordered" || t.currentTokenValue == "unordered"
    case _ =>
      false
  }

  /**
   * Test whether the current token is one that is disallowed after a "leading lone slash".
   * These composite tokens have been parsed as operators, but are not allowed after "/" under the
   * rules of erratum E24
   *
   * @return the resulting subexpression
   */
  def disallowedAtStartOfRelativePath: Boolean = t.currentToken match {
    case Token.CAST_AS | Token.CASTABLE_AS | Token.INSTANCE_OF | Token.TREAT_AS => true
    case _ => false
  }

  /**
   * Parse a PathExpresssion. This includes "true" path expressions such as A/B/C, and also
   * constructs that may start a path expression such as a variable reference $name or a
   * parenthesed expression (A|B). Numeric and string literals also come under this heading.
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parsePathExpression: Expression = {
    val offset = t.currentTokenStartOffset
    t.currentToken match {
      case Token.SLASH =>
        nextToken()
        val start = new RootExpression
        setLocation(start)
        if (disallowedAtStartOfRelativePath) grumble("Operator '" + Token.tokens(t.currentToken) + "' is not allowed after '/'")
        if (atStartOfRelativePath) {
          val path = parseRemainingPath(start)
          setLocation(path, offset)
          path
        }
        else start
      case Token.SLASH_SLASH =>
        nextToken()
        val start2 = new RootExpression
        setLocation(start2, offset)
        val axisExp = new AxisExpression(AxisInfo.DESCENDANT_OR_SELF, null)
        setLocation(axisExp, offset)
        val slashExp = ExpressionTool.makePathExpression(start2, axisExp)
        setLocation(slashExp, offset)
        val exp = parseRemainingPath(slashExp)
        setLocation(exp, offset)
        exp
      case _ =>
        if (t.currentToken == Token.NAME && (t.currentTokenValue == "true" || t.currentTokenValue == "false"))
          warning("The expression is looking for a child element named '" + t.currentTokenValue + "' - perhaps "
            + t.currentTokenValue + "() was intended? To avoid this warning, use child::" + t.currentTokenValue + " or ./" + t.currentTokenValue + ".")
        if (t.currentToken == Token.NAME && t.getBinaryOp(t.currentTokenValue) != Token.UNKNOWN &&
          (language ne ParsedLanguage.XSLT_PATTERN) && (offset > 0 || t.peekAhead != Token.EOF)) {
          val s = t.currentTokenValue
          warning("The keyword '" + s + "' in this context means 'child::" + s + "'. If this was intended, use 'child::" + s + "' or './" + s + "' to avoid this warning.")
        }
        parseRelativePath
    }
  }

  /**
   * Parse an XPath 3.0 simple mapping expression ("!" operator)
   *
   * @return the parsed expression
   * @throws XPathException in the event of a syntax error
   */
  @throws[XPathException]
  def parseSimpleMappingExpression: Expression = {
    val offset = t.currentTokenStartOffset
    var exp = parsePathExpression
    while ( {
      t.currentToken == Token.BANG
    }) {
      if (!allowXPath30Syntax) grumble("XPath '!' operator requires XPath 3.0 to be enabled")
      nextToken()
      val next = parsePathExpression
      exp = new ForEach(exp, next)
      setLocation(exp, offset)
    }
    exp
  }

  /**
   * Parse a relative path (a sequence of steps). Called when the current token immediately
   * follows a separator (/ or //), or an implicit separator (XYZ is equivalent to ./XYZ)
   *
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseRelativePath: Expression = {
    val offset = t.currentTokenStartOffset
    var exp = parseStepExpression(language eq ParsedLanguage.XSLT_PATTERN)
    while ( {
      t.currentToken == Token.SLASH || t.currentToken == Token.SLASH_SLASH
    }) {
      val op = t.currentToken
      nextToken()
      val next = parseStepExpression(false)
      if (op == Token.SLASH) { //return new RawSlashExpression(start, step);
        exp = new HomogeneityChecker(new SlashExpression(exp, next))
      }
      else {
        /* (op == Token.SLASH_SLASH)*/
        // add implicit descendant-or-self::node() step
        val ae = new AxisExpression(AxisInfo.DESCENDANT_OR_SELF, null)
        setLocation(ae, offset)
        val one = ExpressionTool.makePathExpression(exp, ae)
        setLocation(one, offset)
        exp = ExpressionTool.makePathExpression(one, next)
        exp = new HomogeneityChecker(exp)
      }
      setLocation(exp, offset)
    }
    exp
  }

  /**
   * Parse the remaining steps of an absolute path expression (one starting in "/" or "//"). Note that the
   * token immediately after the "/" or "//" has already been read, and in the case of "/", it has been confirmed
   * that we have a path expression starting with "/" rather than a standalone "/" expression.
   *
   * @param start the initial implicit expression: root() in the case of "/", root()/descendant-or-self::node in
   *              the case of "//"
   * @return the completed path expression
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parseRemainingPath(start: Expression): Expression = {
    val offset = t.currentTokenStartOffset
    var exp = start
    var op = Token.SLASH
    breakable {
      while (true) {
        val next = parseStepExpression(false)
        if (op == Token.SLASH) exp = new HomogeneityChecker(new SlashExpression(exp, next))
        else if (op == Token.SLASH_SLASH) {
          val descOrSelf = new AxisExpression(AxisInfo.DESCENDANT_OR_SELF, null)
          setLocation(descOrSelf)
          val step = ExpressionTool.makePathExpression(descOrSelf, next)
          setLocation(step)
          exp = ExpressionTool.makePathExpression(exp, step)
          exp = new HomogeneityChecker(exp)
        }
        else {
          /*if (op == Token.BANG)*/ if (!allowXPath30Syntax) grumble("XPath '!' operator requires XPath 3.0 to be enabled")
          exp = new ForEach(exp, next)
        }
        setLocation(exp, offset)
        op = t.currentToken
        if (op != Token.SLASH && op != Token.SLASH_SLASH && op != Token.BANG) break()
        nextToken()
      }
    }
    exp
  }

  /**
   * Parse a step (including an optional sequence of predicates)
   *
   * @param firstInPattern true only if we are parsing the first step in a
   *                       RelativePathPattern in the XSLT Pattern syntax
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseStepExpression(firstInPattern: Boolean): Expression = {
    var step = parseBasicStep(firstInPattern)
    // When the filter is applied to an Axis step, the nodes are considered in
    // axis order. In all other cases they are considered in document order
    val reverse = step.isInstanceOf[AxisExpression] && !AxisInfo.isForwards((step.asInstanceOf[AxisExpression]).getAxis)
    breakable {
      while (true) if (t.currentToken == Token.LSQB) step = parsePredicate(step)
      else if (t.currentToken == Token.LPAR) { // dynamic function call (XQuery 3.0/XPath 3.0 syntax)
        step = parseDynamicFunctionCall(step, null)
        setLocation(step)
      }
      else if (t.currentToken == Token.QMARK) {
        step = parseLookup(step)
        setLocation(step)
      }
      else break()
    }
    if (reverse) { // An AxisExpression such as preceding-sibling::x delivers nodes in axis
      // order, so that positional predicate like preceding-sibling::x[1] work
      // correctly. To satisfy the XPath semantics we turn preceding-sibling::x
      // into reverse(preceding-sibling::x), and preceding-sibling::x[3] into
      // reverse(preceding-sibling::x[3]). The call on reverse() will be eliminated
      // later in the case where the predicate selects a singleton.
      val rsc = env.makeRetainedStaticContext
      step = SystemFunction.makeCall("reverse", rsc, step)
      assert(step != null)
      step
    }
    else step
  }

  @throws[XPathException]
  def parsePredicate(step: Expression): Expression = {
    var stepExp = step
    nextToken()
    val predicate = parsePredicate
    expect(Token.RSQB)
    nextToken()
    stepExp = new FilterExpression(stepExp, predicate)
    setLocation(stepExp)
    stepExp
  }

  /**
   * Parse an XPath 3.1 arrow operator ("=&gt;")
   *
   * @return the expression that results from the parsing
   */
  @throws[XPathException]
  def parseArrowPostfix(lhs: Expression): Expression = {
    checkLanguageVersion31()
    nextToken()
    val token = getTokenizer.currentToken
    if (token == Token.NAME || token == Token.FUNCTION) parseFunctionCall(lhs)
    else if (token == Token.DOLLAR) {
      val `var` = parseVariableReference
      expect(Token.LPAR)
      parseDynamicFunctionCall(`var`, lhs)
    }
    else if (token == Token.LPAR) {
      val `var` = parseParenthesizedExpression
      expect(Token.LPAR)
      parseDynamicFunctionCall(`var`, lhs)
    }
    else {
      grumble("Unexpected " + Token.tokens(token) + " after '=>'")
      null
    }
  }

  /**
   * Parse the expression within a predicate. A separate method so it can be overridden
   *
   * @return the expression within the predicate
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def parsePredicate: Expression = parseExpression

  def isReservedInQuery(uri: String): Boolean = NamespaceConstant.isReservedInQuery31(uri)

  /**
   * Parse a basic step expression (without the predicates)
   *
   * @param firstInPattern true only if we are parsing the first step in a
   *                       RelativePathPattern in the XSLT Pattern syntax
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseBasicStep(firstInPattern: Boolean): Expression = {
    t.currentToken match {
      case Token.DOLLAR =>
        return parseVariableReference
      case Token.LPAR =>
        return parseParenthesizedExpression
      case Token.LSQB =>
        return parseArraySquareConstructor
      case Token.STRING_LITERAL =>
        return parseStringLiteral(true)
      case Token.STRING_LITERAL_BACKTICKED =>
        return parseStringLiteral(true)
      case Token.STRING_CONSTRUCTOR_INITIAL =>
        return parseStringConstructor
      case Token.NUMBER =>
        return parseNumericLiteral(true)
      case Token.FUNCTION =>
        return parseFunctionCall(null)
      case Token.QMARK =>
        return parseLookup(new ContextItemExpression)
      case Token.DOT =>
        nextToken()
        val cie = new ContextItemExpression
        setLocation(cie)
        return cie
      case Token.DOTDOT =>
        nextToken()
        val pne = new AxisExpression(AxisInfo.PARENT, null)
        setLocation(pne)
        return pne
      case Token.PERCENT =>
        val annotations = parseAnnotationsList
        if (!(t.currentTokenValue == "function")) grumble("Expected 'function' to follow the annotation assertion")
        annotations.check(env.getConfiguration, "IF")
        return parseInlineFunction(annotations)
      case Token.NODEKIND if t.currentTokenValue == "function" =>
        val annotations = AnnotationList.EMPTY
        return parseInlineFunction(annotations)
          //                } else if (t.currentTokenValue.equals("map")) {
          //                    return parseFunctionCall(null);

      case Token.NODEKIND| Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR =>
        var defaultAxis = AxisInfo.CHILD
        if (t.currentToken == Token.NODEKIND && (t.currentTokenValue == "attribute" || t.currentTokenValue == "schema-attribute")) defaultAxis = AxisInfo.ATTRIBUTE
        else if (t.currentToken == Token.NODEKIND && t.currentTokenValue == "namespace-node") {
          defaultAxis = AxisInfo.NAMESPACE
          testPermittedAxis(AxisInfo.NAMESPACE, "XQST0134")
        }
        else if (firstInPattern && t.currentToken == Token.NODEKIND && t.currentTokenValue == "document-node") defaultAxis = AxisInfo.SELF
        var test = parseNodeTest(Type.ELEMENT)
        if (test.isInstanceOf[AnyNodeTest]) { // handles patterns of the form match="node()"
          test = if (defaultAxis == AxisInfo.CHILD) MultipleNodeKindTest.CHILD_NODE
          else NodeKindTest.ATTRIBUTE
        }
        val ae = new AxisExpression(defaultAxis, test)
        setLocation(ae)
        return ae
      case Token.AT =>
        nextToken()
        t.currentToken match {
          case Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND =>
            val ae2 = new AxisExpression(AxisInfo.ATTRIBUTE, parseNodeTest(Type.ATTRIBUTE))
            setLocation(ae2)
            return ae2
          case _ =>
            grumble("@ must be followed by a NodeTest")
        }
      case Token.AXIS =>
        var axis = 0
        try axis = AxisInfo.getAxisNumber(t.currentTokenValue)
        catch {
          case err: XPathException =>
            grumble(err.getMessage)
            axis = AxisInfo.CHILD // error recovery
        }
        testPermittedAxis(axis, "XPST0003")
        val principalNodeType = AxisInfo.principalNodeType(axis)
        nextToken()
        t.currentToken match {
          case Token.NAME | Token.PREFIX | Token.SUFFIX | Token.STAR | Token.NODEKIND =>
            val ax = new AxisExpression(axis, parseNodeTest(principalNodeType))
            setLocation(ax)
            return ax
          case _ =>
            grumble("Unexpected token " + currentTokenDisplay + " after axis name")
        }
      case Token.KEYWORD_CURLY =>
        t.currentTokenValue match {
          case "map" =>
            return parseMapExpression
          case "array" =>
            return parseArrayCurlyConstructor
          case "fn" | "." =>
            return parserExtension.parseDotFunction(this)
          case "_" =>
            return parserExtension.parseUnderscoreFunction(this)
          case _ =>
            return parseConstructor
        }
      case Token.ELEMENT_QNAME | Token.ATTRIBUTE_QNAME | Token.NAMESPACE_QNAME | Token.PI_QNAME | Token.TAG =>
        return parseConstructor
      case Token.NAMED_FUNCTION_REF =>
        return parseNamedFunctionReference
      case _ => grumble("Unexpected token " + currentTokenDisplay + " at start of expression")
    }
    new ErrorExpression
  }

  @throws[XPathException]
  def parseParenthesizedExpression: Expression = {
    nextToken()
    if (t.currentToken == Token.RPAR) {
      nextToken()
      return Literal.makeEmptySequence
    }
    val seq = parseExpression
    expect(Token.RPAR)
    nextToken()
    seq
  }

  @throws[XPathException]
  def testPermittedAxis(axis: Int, errorCode: String): Unit =
    if (axis == AxisInfo.PRECEDING_OR_ANCESTOR)
      grumble("The preceding-or-ancestor axis is for internal use only", errorCode)

  @throws[XPathException]
  def parseNumericLiteral(traceable: Boolean): Expression = {
    val offset = t.currentTokenStartOffset
    val number = NumericValue.parseNumber(t.currentTokenValue)
    if (number.isNaN)
      grumble("Invalid numeric literal " + Err.wrap(t.currentTokenValue, Err.VALUE))
    nextToken()
    val lit = Literal.makeLiteral(number)
    setLocation(lit, offset)
    //lit.setRetainedStaticContext(env.makeRetainedStaticContext());
    if (traceable) makeTracer(lit, null) else lit
  }

  @throws[XPathException]
  def parseStringLiteral(traceable: Boolean): Expression = {
    val literal = makeStringLiteral(t.currentTokenValue)
    nextToken()
    if (traceable) makeTracer(literal, null)
    else literal
  }

  @throws[XPathException]
  def parseStringConstructor: Expression = {
    grumble("String constructor expressions are allowed only in XQuery")
    null
  }

  @throws[XPathException]
  def parseVariableReference: Expression = {
    val offset = t.currentTokenStartOffset
    nextToken()
    if (t.currentToken == Token.NUMBER) { // Saxon extension: $1, $2 etc as parameter references
      return parserExtension.bindNumericParameterReference(this)
    }
    expect(Token.NAME)
    val `var` = t.currentTokenValue
    nextToken()

    if (scanOnly) {
      return new ContextItemExpression
      // don't do any semantic checks during a prescan
    }

    //int vtest = makeNameCode(var, false) & 0xfffff;
    val vtest = makeStructuredQName(`var`, "")
    assert(vtest != null)

    // See if it's a range variable or a variable in the context
    val b = findRangeVariable(vtest)
    var ref: Expression = null
    if (b != null) {
      ref = new LocalVariableReference(b)
    } else {
      if (catchDepth > 0)
        for (errorVariable <- StandardNames.errorVariables) {
          if (errorVariable.getLocalPart == vtest.getLocalPart) {
            val functionName = new StructuredQName("saxon", NamespaceConstant.SAXON, "dynamic-error-info")
            val sn = new SymbolicName.F(functionName, 1)
            val args = Array[Expression](new StringLiteral(vtest.getLocalPart))
            return VendorFunctionSetHE.getInstance.bind(sn, args, env, new util.ArrayList[String])
          }
        }
      try ref = env.bindVariable(vtest)
      catch {
        case err: XPathException =>
          err.maybeSetLocation(makeLocation)
          throw err
      }
    }
    setLocation(ref, offset)
    ref
  }

  @throws[XPathException]
  def makeStringLiteral(currentTokenValue: String): Literal = {
    val literal = new StringLiteral(currentTokenValue)
    setLocation(literal)
    literal
  }

  /**
   * Unescape character references and built-in entity references in a string. Does nothing
   * in XPath, because XPath does not recognize entity references in string literals
   *
   * @param token the input string, which may include XML-style character references or built-in
   *              entity references
   * @return the string with character references and built-in entity references replaced by their expansion
   * @throws XPathException if a malformed character or entity reference is found
   */
  @throws[XPathException]
  def unescape(token: String): CharSequence = token

  @throws[XPathException]
  def parseConstructor: Expression = {
    grumble("Node constructor expressions are allowed only in XQuery, not in XPath")
    new ErrorExpression
  }

  @throws[XPathException]
  def parseDynamicFunctionCall(functionItem: Expression, prefixArgument: Expression): Expression = {
    checkLanguageVersion30()
    val args = new util.ArrayList[Expression](10)
    if (prefixArgument != null) args.add(prefixArgument)
    var placeMarkers: IntSet = null
    // the "(" has already been read by the Tokenizer: now parse the arguments
    nextToken()
    if (t.currentToken != Token.RPAR) {
      breakable {
        while (true) {
          var arg = parseFunctionArgument
          if (arg == null) { // this is a "?" placemarker
            if (placeMarkers == null) placeMarkers = new IntArraySet
            placeMarkers.add(args.size)
            arg = Literal.makeEmptySequence // a convenient fiction
          }
          args.add(arg)
          if (t.currentToken == Token.COMMA)
            nextToken()
          else
            break()
        }
      }
      expect(Token.RPAR)
    }
    nextToken()
    if (placeMarkers == null) generateApplyCall(functionItem, args)
    else parserExtension.createDynamicCurriedFunction(this, functionItem, args, placeMarkers)
  }

  @throws[XPathException]
  def generateApplyCall(functionItem: Expression, args: util.ArrayList[Expression]): Expression = {
    val block = new SquareArrayConstructor(args)
    val rsc = new RetainedStaticContext(getStaticContext)
    val fn = VendorFunctionSetHE.getInstance.makeFunction("apply", 2)
    fn.setRetainedStaticContext(rsc)
    val call = fn.makeFunctionCall(functionItem, block)
    fn.asInstanceOf[ApplyFn].setDynamicFunctionCall(functionItem.toShortString)
    setLocation(call, t.currentTokenStartOffset)
    call
  }

  @throws[XPathException]
  def parseLookup(lhs: Expression): Expression = {
    checkLanguageVersion31()
    val t = getTokenizer
    val offset = t.currentTokenStartOffset
    t.setState(Tokenizer.BARE_NAME_STATE) // Prevent mis-recognition of x?f(2)
    t.currentToken = Token.LPAR // Hack to force following symbol to be recognised in post-operator mode
    nextToken()
    val token = t.currentToken
    t.setState(Tokenizer.OPERATOR_STATE)
    var result: Expression = null
    if (token == Token.NAME) {
      val name = t.currentTokenValue
      if (!NameChecker.isValidNCName(name)) grumble("The name following '?' must be a valid NCName")
      nextToken()
      result = lookupName(lhs, name)
    }
    else if (token == Token.NUMBER) {
      val number = NumericValue.parseNumber(t.currentTokenValue)
      if (!number.isInstanceOf[IntegerValue]) grumble("Number following '?' must be an integer")
      nextToken()
      result = XPathParser.lookup(this, lhs, Literal.makeLiteral(number))
    }
    else if (token == Token.MULT || token == Token.STAR) {
      nextToken()
      result = XPathParser.lookupStar(lhs)
    }
    else if (token == Token.LPAR) result = XPathParser.lookup(this, lhs, parseParenthesizedExpression)
    else if (token == Token.STRING_LITERAL) {
      checkSyntaxExtensions("string literal after '?'")
      result = lookupName(lhs, t.currentTokenValue)
      nextToken()
    }
    else if (token == Token.DOLLAR) {
      checkSyntaxExtensions("variable reference after '?'")
      result = XPathParser.lookup(this, lhs, parseVariableReference)
      nextToken()
    }
    else {
      grumble("Unexpected " + Token.tokens(token) + " after '?'")
      return null
    }
    setLocation(result, offset)
    result
  }

  /**
   * Supporting code for lookup expressions (A?B) where B is an NCName
   *
   * @param lhs the LHS operand of the lookup expression
   * @param rhs the RHS operand of the lookup expression
   * @return the result of parsing the expression
   */
  private def lookupName(lhs: Expression, rhs: String) = new LookupExpression(lhs, new StringLiteral(rhs))

  /**
   * Parse a NodeTest.
   * One of QName, prefix:*, *:suffix, *, text(), node(), comment(), or
   * processing-instruction(literal?), or element(~,~), attribute(~,~), etc.
   *
   * @param nodeType the node type being sought if one is specified
   * @return the resulting NodeTest object
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseNodeTest(nodeType: Short): NodeTest = {
    val tok = t.currentToken
    var tokv = t.currentTokenValue
    tok match {
      case Token.NAME =>
        nextToken()
        makeNameTest(nodeType, tokv, nodeType == Type.ELEMENT)
      case Token.PREFIX =>
        nextToken()
        makeNamespaceTest(nodeType, tokv)
      case Token.SUFFIX =>
        nextToken()
        tokv = t.currentTokenValue
        expect(Token.NAME)
        nextToken()
        makeLocalNameTest(nodeType, tokv)
      case Token.STAR =>
        nextToken()
        NodeKindTest.makeNodeKindTest(nodeType)
      case Token.NODEKIND =>
        parseKindTest
      case _ =>
        grumble("Unrecognized node test")
        throw new XPathException("") // unreachable instruction
    }
  }

  @throws[XPathException]
  private def parseKindTest: NodeTest = {
    val pool = env.getConfiguration.getNamePool
    val typeName = t.currentTokenValue
    val schemaDeclaration = typeName.startsWith("schema-")
    val primaryType = getSystemType(typeName)
    var fp = -1
    var empty = false
    nextToken()
    if (t.currentToken == Token.RPAR) {
      if (schemaDeclaration) {
        grumble("schema-element() and schema-attribute() require a name to be supplied")
        return null
      }
      empty = true
      nextToken()
    }
    primaryType match {
      case Type.ITEM =>
        grumble("item() is not allowed in a path expression")
        null
      case Type.NODE =>
        if (empty) AnyNodeTest.getInstance
        else {
          grumble("Expected ')': no arguments are allowed in node()")
          null
        }
      case Type.TEXT =>
        if (empty) NodeKindTest.TEXT
        else {
          grumble("Expected ')': no arguments are allowed in text()")
          null
        }
      case Type.COMMENT =>
        if (empty) NodeKindTest.COMMENT
        else {
          grumble("Expected ')': no arguments are allowed in comment()")
          null
        }
      case Type.NAMESPACE =>
        if (empty) {
          if (!isNamespaceTestAllowed) grumble("namespace-node() test is not allowed in XPath 2.0/XQuery 1.0")
          NodeKindTest.NAMESPACE
        }
        else if ((language eq ParsedLanguage.EXTENDED_ITEM_TYPE) && t.currentToken == Token.NAME) {
          val nsName = t.currentTokenValue
          nextToken()
          expect(Token.RPAR)
          nextToken()
          new NameTest(Type.NAMESPACE, "", nsName, pool)
        }
        else {
          grumble("No arguments are allowed in namespace-node()")
          null
        }
      case Type.DOCUMENT =>
        if (empty) NodeKindTest.DOCUMENT
        else {
          var innerType = 0
          try innerType = getSystemType(t.currentTokenValue)
          catch {
            case err: XPathException =>
              innerType = Type.ITEM
          }
          if (innerType != Type.ELEMENT) {
            grumble("Argument to document-node() must be an element type descriptor")
            return null
          }
          val inner = parseKindTest
          expect(Token.RPAR)
          nextToken()
          new DocumentNodeTest(inner)
        }
      case Type.PROCESSING_INSTRUCTION =>
        if (empty) return NodeKindTest.PROCESSING_INSTRUCTION
        else if (t.currentToken == Token.STRING_LITERAL) {
          val piName = Whitespace.trim(unescape(t.currentTokenValue))
          if (!NameChecker.isValidNCName(piName)) { // Became an error as a result of XPath erratum XP.E7
            grumble("Processing instruction name must be a valid NCName", "XPTY0004")
          }
          else fp = pool.allocateFingerprint("", piName)
        }
        else if (t.currentToken == Token.NAME) try {
          val parts = NameChecker.getQNameParts(t.currentTokenValue)
          if (parts(0).isEmpty) fp = pool.allocateFingerprint("", parts(1))
          else grumble("Processing instruction name must not contain a colon")
        } catch {
          case e: QNameException =>
            grumble("Invalid processing instruction name. " + e.getMessage)
        }
        else grumble("Processing instruction name must be a QName or a string literal")
        nextToken()
        expect(Token.RPAR)
        nextToken()
        new NameTest(Type.PROCESSING_INSTRUCTION, fp, pool)
      case Type.ATTRIBUTE | Type.ELEMENT =>
        var nodeName = ""
        var nodeTest: NodeTest = null
        if (empty) return NodeKindTest.makeNodeKindTest(primaryType)
        else if (t.currentToken == Token.STAR || t.currentToken == Token.MULT) { // allow for both representations of "*" to be safe
          if (schemaDeclaration) {
            grumble("schema-element() and schema-attribute() must specify an actual name, not '*'")
            return null
          }
          nodeTest = NodeKindTest.makeNodeKindTest(primaryType)
          nextToken()
        }
        else if (t.currentToken == Token.NAME) {
          nodeName = t.currentTokenValue
          fp = makeFingerprint(t.currentTokenValue, primaryType == Type.ELEMENT)
          nextToken()
        }
        else if ((t.currentToken == Token.PREFIX || t.currentToken == Token.SUFFIX) && allowSaxonExtensions)
          nodeTest = parseNodeTest(primaryType.toShort)
        else grumble("Unexpected " + Token.tokens(t.currentToken) + " after '(' in SequenceType")
        var suri: String = null
        if (fp != -1) suri = pool.getURI(fp)
        if (t.currentToken == Token.RPAR) {
          nextToken()
          if (fp == -1) { // element(*) or attribute(*)
            return nodeTest
          }
          else if (primaryType == Type.ATTRIBUTE) { // attribute(N) or schema-attribute(N)
            if (schemaDeclaration) { // schema-attribute(N)
              val attributeDecl: SchemaDeclaration = env.getConfiguration.getAttributeDeclaration(fp & 0xfffff)
              if (!env.isImportedSchema(suri)) grumble("No schema has been imported for namespace '" + suri + '\'', "XPST0008")
              if (attributeDecl == null) {
                grumble("There is no declaration for attribute @" + nodeName + " in an imported schema", "XPST0008")
                return null
              }
              else return attributeDecl.makeSchemaNodeTest
            }
            else return new NameTest(Type.ATTRIBUTE, fp, pool)
          }
          else { // element(N) or schema-element(N)
            if (schemaDeclaration) { // schema-element(N)
              if (!env.isImportedSchema(suri)) grumble("No schema has been imported for namespace '" + suri + '\'', "XPST0008")
              val elementDecl: SchemaDeclaration = env.getConfiguration.getElementDeclaration(fp & 0xfffff)
              if (elementDecl == null) {
                grumble("There is no declaration for element <" + nodeName + "> in an imported schema", "XPST0008")
                return null
              }
              else return elementDecl.makeSchemaNodeTest
            }
            else return makeNameTest(Type.ELEMENT, nodeName, useDefault = true)
          }
        }
        else if (t.currentToken == Token.COMMA) {
          if (schemaDeclaration) {
            grumble("schema-element() and schema-attribute() must have one argument only")
            return null
          }
          nextToken()
          var result: NodeTest = null
          if (t.currentToken == Token.STAR) {
            grumble("'*' is no longer permitted as the second argument of element() and attribute()")
            return null
          }
          else if (t.currentToken == Token.NAME) {
            var schemaType: SchemaType = null
            val contentType = makeStructuredQName(t.currentTokenValue, env.getDefaultElementNamespace)
            assert(contentType != null)
            val uri: String = contentType.getURI
            val lname: String = contentType.getLocalPart
            if (uri == NamespaceConstant.SCHEMA) schemaType = env.getConfiguration.getSchemaType(contentType)
            else {
              if (!env.isImportedSchema(uri)) grumble("No schema has been imported for namespace '" + uri + '\'', "XPST0008")
              schemaType = env.getConfiguration.getSchemaType(contentType)
            }
            if (schemaType == null) {
              grumble("Unknown type name " + contentType.getEQName, "XPST0008")
              return null
            }
            if (primaryType == Type.ATTRIBUTE && schemaType.isComplexType) warning("An attribute cannot have a complex type")
            val typeTest = new ContentTypeTest(primaryType, schemaType, env.getConfiguration, false)
            if (fp == -1 && (nodeTest == null || nodeTest.isInstanceOf[NodeKindTest])) { // this represents element(*,T) or attribute(*,T)
              result = typeTest
              if (primaryType == Type.ATTRIBUTE) nextToken()
              else { // assert (primaryType == Type.ELEMENT);
                nextToken()
                if (t.currentToken == Token.QMARK) {
                  typeTest.setNillableBool(true)
                  nextToken()
                }
              }
            }
            else if (primaryType == Type.ATTRIBUTE) {
              if (nodeTest == null) nodeTest = new NameTest(Type.ATTRIBUTE, fp, pool)
              if ((schemaType eq AnyType.getInstance) || (schemaType eq AnySimpleType)) result = nodeTest
              else result = new CombinedNodeTest(nodeTest, Token.INTERSECT, typeTest)
              nextToken()
            }
            else {
              if (nodeTest == null) nodeTest = new NameTest(Type.ELEMENT, fp, pool)
              result = new CombinedNodeTest(nodeTest, Token.INTERSECT, typeTest)
              nextToken()
              if (t.currentToken == Token.QMARK) {
                typeTest.setNillableBool(true)
                nextToken()
              }
            }
          }
          else {
            grumble("Unexpected " + Token.tokens(t.currentToken) + " after ',' in SequenceType")
            return null
          }
          expect(Token.RPAR)
          nextToken()
          return result
        }
        else grumble("Expected ')' or ',' in SequenceType")
        null
      case _ =>
        // can't happen!
        grumble("Unknown node kind")
        null
    }
  }

  /**
   * Ask whether the syntax namespace-node() is allowed in a node kind test.
   *
   * @return true unless XPath 2.0 / XQuery 1.0 syntax is required
   */
  def isNamespaceTestAllowed: Boolean = allowXPath30Syntax

  /**
   * Get a system type - that is, one whose name is a keyword rather than a QName. This includes the node
   * kinds such as element and attribute, and the generic types node() and item()
   *
   * @param name the name of the system type, for example "element" or "comment"
   * @return the integer constant denoting the type, for example { @link Type#ITEM} or { @link Type#ELEMENT}
   * @throws XPathException if the name is not recognized
   */
  @throws[XPathException]
  private def getSystemType(name: String): Int = if ("item" == name) Type.ITEM
  else if ("document-node" == name) Type.DOCUMENT
  else if ("element" == name) Type.ELEMENT
  else if ("schema-element" == name) Type.ELEMENT
  else if ("attribute" == name) Type.ATTRIBUTE
  else if ("schema-attribute" == name) Type.ATTRIBUTE
  else if ("text" == name) Type.TEXT
  else if ("comment" == name) Type.COMMENT
  else if ("processing-instruction" == name) Type.PROCESSING_INSTRUCTION
  else if ("namespace-node" == name) Type.NAMESPACE
  else if ("node" == name) Type.NODE
  else {
    grumble("Unknown type " + name)
    -1
  }

  @throws[XPathException]
  def checkLanguageVersion30(): Unit = if (!allowXPath30Syntax) grumble("To use XPath 3.0 syntax, you must configure the XPath parser to handle it")

  @throws[XPathException]
  def checkLanguageVersion31(): Unit = if (!allowXPath31Syntax) grumble("The XPath parser is not configured to allow use of XPath 3.1 syntax")

  @throws[XPathException]
  def checkMapExtensions(): Unit = if (!(allowXPath31Syntax || allowXPath30XSLTExtensions)) grumble("The XPath parser is not configured to allow use of the map syntax from XSLT 3.0 or XPath 3.1")

  @throws[XPathException]
  def checkSyntaxExtensions(construct: String): Unit = if (!allowSaxonExtensions) grumble("Saxon XPath syntax extensions have not been enabled: " + construct + " is not allowed")

  /**
   * Parse a map expression. Requires XPath/XQuery 3.0
   * Provisional syntax
   * map { expr : expr (, expr : expr )*} }
   *
   * @return the map expression
   * @throws XPathException if parsing fails
   */
  @throws[XPathException]
  def parseMapExpression: Expression = {
    checkMapExtensions()
    // have read the "map {"
    val t = getTokenizer
    val offset = t.currentTokenStartOffset
    val entries = new util.ArrayList[Expression]
    nextToken()
    if (t.currentToken != Token.RCURLY)
      breakable {
        while (true) {
          val key = parseExprSingle
          if (t.currentToken == Token.ASSIGN)
            grumble("The ':=' notation is no longer accepted in map expressions: use ':' instead")
          expect(Token.COLON)
          nextToken()
          val value: Expression = parseExprSingle
          var entry: Expression = null
          if (key.isInstanceOf[Literal] && key.asInstanceOf[Literal].value.isInstanceOf[AtomicValue] && value.isInstanceOf[Literal])
            entry = Literal.makeLiteral(new SingleEntryMap(key.asInstanceOf[Literal].value.asInstanceOf[AtomicValue], value.asInstanceOf[Literal].value))
          else entry = MapFunctionSet.getInstance.makeFunction("entry", 2).makeFunctionCall(key, value)
          entries.add(entry)
          if (t.currentToken == Token.RCURLY)
            break()
          else {
            expect(Token.COMMA)
            nextToken()
          }
        }
      }
    t.lookAhead() //manual lookahead after an RCURLY
    nextToken()
    var result: Expression = null
    entries.size match {
      case 0 =>
        result = Literal.makeLiteral(new HashTrieMap)
      case 1 =>
        result = entries.get(0)
      case _ =>
        val entriesArray = new Array[Expression](entries.size)
        val block = new Block(entries.toArray(entriesArray))
        val options = new DictionaryMap
        options.initialPut("duplicates", new StringValue("reject"))
        options.initialPut("duplicates-error-code", new StringValue("XQDY0137"))
        result = MapFunctionSet.getInstance.makeFunction("merge", 2).makeFunctionCall(block, Literal.makeLiteral(options))
    }
    setLocation(result, offset)
    result
  }

  /**
   * Parse a "square" array constructor
   * "[" (exprSingle ("," exprSingle)* )? "]"
   * Applies to XPath/XQuery 3.1 only
   */
  @throws[XPathException]
  def parseArraySquareConstructor: Expression = {
    checkLanguageVersion31()
    val t = getTokenizer
    val offset = t.currentTokenStartOffset
    val members = new util.ArrayList[Expression]
    nextToken()
    if (t.currentToken == Token.RSQB) {
      nextToken()
      val block = new SquareArrayConstructor(members)
      setLocation(block, offset)
      return block
    }
    while (true) {
      val member = parseExprSingle
      members.add(member)
      breakable {
        if (t.currentToken == Token.COMMA) {
          nextToken()
        }
        else if (t.currentToken == Token.RSQB) {
          nextToken()
          break()
        }
      }
      grumble("Expected ',' or ']', " + "found " + Token.tokens(t.currentToken))
      return new ErrorExpression
    }
    val block = new SquareArrayConstructor(members)
    setLocation(block, offset)
    block
  }

  /**
   * Parse a "curly" array constructor
   * array "{" expr "}"
   * Applies to XPath/XQuery 3.1 only
   *
   * @return the parsed expression
   * @throws XPathException if the syntax is invalid or the construct is not permitted
   */
  @throws[XPathException]
  def parseArrayCurlyConstructor: Expression = {
    checkLanguageVersion31()
    val t = getTokenizer
    val offset = t.currentTokenStartOffset
    nextToken()
    if (t.currentToken == Token.RCURLY) {
      t.lookAhead()
      nextToken()
      return Literal.makeLiteral(SimpleArrayItem.EMPTY_ARRAY)
    }
    val body = parseExpression
    expect(Token.RCURLY)
    t.lookAhead()
    nextToken()
    val sf = ArrayFunctionSet.getInstance.makeFunction("_from-sequence", 1)
    val result = sf.makeFunctionCall(body)
    setLocation(result, offset)
    result
  }

  /**
   * Parse a function call.
   * function-name '(' ( Expression (',' Expression )* )? ')'
   *
   * @param prefixArgument left hand operand of arrow operator,
   *                       or null in the case of a conventional function call
   * @return the resulting subexpression
   * @throws XPathException if any error is encountered
   */
  @throws[XPathException]
  def parseFunctionCall(prefixArgument: Expression): Expression = {
    val fname = t.currentTokenValue
    val offset = t.currentTokenStartOffset
    val args = new util.ArrayList[Expression](10)
    if (prefixArgument != null)
      args.add(prefixArgument)
    val functionName = resolveFunctionName(fname)
    var placeMarkers: IntSet = null
    nextToken()
    if (t.currentToken != Token.RPAR) {
      breakable {
        while (true) {
          var arg = parseFunctionArgument
          if (arg == null) {
            if (placeMarkers == null) placeMarkers = new IntArraySet
            placeMarkers.add(args.size)
            arg = Literal.makeEmptySequence
          }
          args.add(arg)
          if (t.currentToken == Token.COMMA)
            nextToken()
          else
            break()
        }
      }
      expect(Token.RPAR)
    }
    nextToken()
    if (scanOnly)
      return new StringLiteral(StringValue.EMPTY_STRING)
    val arguments = new Array[Expression](args.size)
    args.toArray(arguments)

    if (placeMarkers != null)
      return parserExtension.makeCurriedFunction(this, offset, functionName, arguments, placeMarkers)

    var fcall: Expression = null
    val sn = new SymbolicName.F(functionName, args.size)
    val reasons = new util.ArrayList[String]

    fcall = env.getFunctionLibrary.bind(sn, arguments, env, reasons)
    if (fcall == null)
      return reportMissingFunction(offset, functionName, arguments, reasons)

    if (language eq ParsedLanguage.XSLT_PATTERN)
      if (fcall.isCallOn(classOf[RegexGroup]))
        return Literal.makeEmptySequence
      else fcall match {
        case _: CurrentGroupCall =>
          grumble("The current-group() function cannot be used in a pattern", "XTSE1060", offset)
          return new ErrorExpression
        case _: CurrentGroupingKeyCall =>
          grumble("The current-grouping-key() function cannot be used in a pattern", "XTSE1070", offset)
          return new ErrorExpression
        case _ =>
      }
//    else if (fcall.isCallOn(classOf[CurrentMergeGroup])) {
//      grumble("The current-merge-group() function cannot be used in a pattern", "XTSE3470", offset)
//      return new ErrorExpression
//    }
//    else if (fcall.isCallOn(classOf[CurrentMergeKey])) {
//      grumble("The current-merge-key() function cannot be used in a pattern", "XTSE3500", offset)
//      return new ErrorExpression
//    }
    setLocation(fcall, offset)
    for (argument <- arguments)
      if ((fcall ne argument) && ! functionName.hasURI(NamespaceConstant.GLOBAL_JS))
        // avoid doing this when the function has already been optimized away, e.g. unordered()
        // Also avoid doing this when a js: function is parsed into an ixsl:call()
        // TODO move the adoptChildExpression into individual function libraries
        fcall.adoptChildExpression(argument)

    makeTracer(fcall, functionName)
  }

  @throws[XPathException]
  def reportMissingFunction(offset: Int, functionName: StructuredQName, arguments: Array[Expression], reasons: util.List[String]): ErrorExpression = {
    val sb = new StringBuilder
    sb.append("Cannot find a ").append(arguments.length).append("-argument function named ").append(functionName.getEQName).append("()")
    val config = env.getConfiguration
    import scala.jdk.CollectionConverters._
    for (reason <- reasons.asScala) {
      sb.append(". ").append(reason)
    }
    if (config.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      var existsWithDifferentArity = false
      breakable {
        for (i <- 0 until arguments.length + 5) {
          if (i != arguments.length) {
            val sn = new SymbolicName.F(functionName, i)
            if (env.getFunctionLibrary.isAvailable(sn)) {
              existsWithDifferentArity = true
              break()
            }
          }
        }
      }
      if (existsWithDifferentArity)
        sb.append(". The namespace URI and local name are recognized, but the number of arguments is wrong")
      else {
        val supplementary = XPathParser.getMissingFunctionExplanation(functionName, config)
        if (supplementary != null) sb.append(". ").append(supplementary)
      }
    }
    else sb.append(". External function calls have been disabled")
    if (env.isInBackwardsCompatibleMode) {
      // treat this as a dynamic error to be reported only if the function call is executed
      new ErrorExpression(sb.toString, "XTDE1425", false)
    } else {
      grumble(sb.toString, "XPST0017", offset)
      null
    }
  }

  /**
   * Interpret a function name, returning it as a resolved QName
   *
   * @param fname the lexical QName used as the function name; or an EQName presented
   *              by the tokenizer as a name in Clark notation
   * @return the Structured QName obtained by resolving any prefix in the function name
   * @throws XPathException if the supplied name is not a valid QName or if its prefix
   *                        is not in scope
   */
  @throws[XPathException]
  def resolveFunctionName(fname: String): StructuredQName = {

    if (scanOnly)
      return new StructuredQName("", NamespaceConstant.SAXON, "dummy")

    val functionName =
      try
        qNameParser.parse(fname, env.getDefaultFunctionNamespace)
      catch {
        case e: XPathException =>
          grumble(e.getMessage, e.getErrorCodeLocalPart)
          assert(false)
          null
      }

    if (functionName.hasURI(NamespaceConstant.SCHEMA)) {
      Type.getBuiltInItemType(functionName.getURI, functionName.getLocalPart) match {
        case atomicType: BuiltInAtomicType => checkAllowedType(env, atomicType)
        case _ =>
      }
    }
    functionName
  }

  /**
   * Parse an argument to a function call. Separate method so it can
   * be overridden. With higher-order-function syntax in XPath 3.0/XQuery 3.0,
   * this returns null if the pseudo-argument "?" is found.
   *
   * @return the Expression used as the argument, or null if the argument is the place-holder "?"
   * @throws XPathException if the argument expression does not parse correctly
   */
  @throws[XPathException]
  def parseFunctionArgument: Expression = {
    if (t.currentToken == Token.QMARK) {
      val next = t.peekAhead
      if (next == Token.COMMA || next == Token.RPAR) {
        nextToken()
        return parserExtension.makeArgumentPlaceMarker(this)
      }
    }
    parseExprSingle
  }

  @throws[XPathException]
  def parseNamedFunctionReference: Expression = parserExtension.parseNamedFunctionReference(this)

  @throws[XPathException]
  def parseAnnotationsList: AnnotationList = {
    grumble("Inline functions are not allowed in Saxon-HE")
    null
  }

  @throws[XPathException]
  def parseInlineFunction(annotations: AnnotationList): Expression = parserExtension.parseInlineFunction(this, annotations)

  /**
   * Process a function call in which one or more of the argument positions are
   * represented as "?" placemarkers (indicating partial application or currying)
   *
   * @param offset       the position of the expression in the source text
   * @param name         the function name (as if there were no currying)
   * @param args         the arguments (with EmptySequence in the placemarker positions)
   * @param placeMarkers the positions of the placemarkers    @return the curried function
   * @return the curried function
   * @throws XPathException if a static error is found
   */
  @throws[XPathException]
  def makeCurriedFunction(offset: Int, name: StructuredQName, args: Array[Expression], placeMarkers: IntSet): ErrorExpression = {
    grumble("Partial function application is not allowed in Saxon-HE")
    new ErrorExpression
  }

  /**
   * Get the stack of in-scope range variables
   *
   * @return the stack of variables
   */
  def getRangeVariables: mutable.Stack[LocalBinding] = rangeVariables

  /**
   * Set a new stack of in-scope range variables
   *
   * @param variables the stack of variables
   */
  def setRangeVariables(variables: mutable.Stack[LocalBinding]): Unit = this.rangeVariables = variables

  /**
   * Declare a range variable (record its existence within the parser).
   * A range variable is a variable declared within an expression, as distinct
   * from a variable declared in the context.
   *
   * @param declaration the variable declaration to be added to the stack
   */
  def declareRangeVariable(declaration: LocalBinding): mutable.Stack[LocalBinding] =
    rangeVariables.push(declaration)

  /**
   * Note when the most recently declared range variable has gone out of scope
   */
  def undeclareRangeVariable(): LocalBinding = rangeVariables.pop

  /**
   * Locate a range variable with a given name. (By "range variable", we mean a
   * variable declared within the expression where it is used.)
   *
   * @param qName identifies the name of the range variable
   * @return null if not found (this means the variable is probably a
   *         context variable); otherwise the relevant RangeVariable
   */
  def findRangeVariable(qName: StructuredQName): LocalBinding = {
    for (v <- rangeVariables.size - 1 to 0 by -1) {
      val b = rangeVariables(v)
      if (b.getVariableQName == qName) return b
    }
    parserExtension.findOuterRangeVariable(this, qName)
  }

  /**
   * Set the range variable stack. Used when parsing a nested subexpression
   * inside an attribute constructor.
   *
   * @param stack the stack to be used for local variables declared within the expression
   */
  def setRangeVariableStack(stack: mutable.Stack[LocalBinding]): Unit = rangeVariables = stack

  /**
   * Make a NameCode, using the static context for namespace resolution
   *
   * @param qname      The name as written, in the form "[prefix:]localname"; alternatively,
   *                   a QName in Clark notation ({uri}local)
   * @param useDefault Defines the action when there is no prefix. If
   *                   true, use the default namespace URI for element names. If false,
   *                   use no namespace URI (as for attribute names).
   * @return the fingerprint, which can be used to identify this name in the
   *         name pool
   * @throws XPathException if the name is invalid, or the prefix
   *                        undeclared
   */
  @throws[XPathException]
  final def makeFingerprint(qname: String, useDefault: Boolean): Int = {
    if (scanOnly) return StandardNames.XML_SPACE
    try {
      val defaultNS = if (useDefault) env.getDefaultElementNamespace
      else ""
      val sq = qNameParser.parse(qname, defaultNS)
      env.getConfiguration.getNamePool.allocateFingerprint(sq.getURI, sq.getLocalPart)
    } catch {
      case e: XPathException =>
        grumble(e.getMessage, e.getErrorCodeLocalPart)
        -1
    }
  }

  /**
   * Make a NameCode, using the static context for namespace resolution.
   * This variant of the method does not call "grumble" to report any errors
   * to the ErrorListener, it only reports errors by throwing exceptions. This
   * allows the caller to control the message output.
   *
   * @param qname      The name as written, in the form "[prefix:]localname"
   * @param defaultUri Defines the URI to be returned if there is no prefix.
   * @return the structured QName
   * @throws XPathException if the name is invalid, or the prefix
   *                        undeclared or if the name is not a lexically valid QName
   */
  @throws[XPathException]
  final def makeStructuredQNameSilently(qname: String, defaultUri: String): StructuredQName = {
    if (scanOnly)
      new StructuredQName("", NamespaceConstant.SAXON, "dummy")
    else
      qNameParser.parse(qname, defaultUri)
  }

  /**
   * Make a Structured QName, using the static context for namespace resolution
   *
   * @param qname      The name as written, in the form "[prefix:]localname"; alternatively, a QName in
   *                   Clark format ({uri}local)
   * @param defaultUri The URI to be used if the name is written as a localname with no prefix
   * @return the QName as an instance of StructuredQName
   * @throws XPathException if the name is invalid, or the prefix
   *                        undeclared
   */
  @throws[XPathException]
  final def makeStructuredQName(qname: String, defaultUri: String): StructuredQName =
    try {
      makeStructuredQNameSilently(qname, defaultUri)
    } catch {
      case err: XPathException =>
        grumble(err.getMessage, err.getErrorCodeLocalPart)
        new StructuredQName("", "", "error") // Not executed; here to keep the compiler happy
    }

  /**
   * Make a FingerprintedQName, using the static context for namespace resolution
   *
   * @param qname      The name as written, in the form "[prefix:]localname"; alternatively, a QName in
   *                   Clark format ({uri}local)
   * @param useDefault Defines the action when there is no prefix. If
   *                   true, use the default namespace URI for element names. If false,
   *                   use no namespace URI (as for attribute names).
   * @return the fingerprinted QName
   * @throws XPathException if the name is invalid, or the prefix
   *                        undeclared
   */
  @throws[XPathException]
  final def makeNodeName(qname: String, useDefault: Boolean): NodeName = {
    val sq = makeStructuredQNameSilently(qname, if (useDefault) env.getDefaultElementNamespace
    else "")
    val prefix = sq.getPrefix
    val uri = sq.getURI
    val local = sq.getLocalPart
    if (uri.isEmpty) {
      val fp = env.getConfiguration.getNamePool.allocateFingerprint("", qname)
      new NoNamespaceName(qname, fp)
    }
    else {
      val fp = env.getConfiguration.getNamePool.allocateFingerprint(uri, local)
      new FingerprintedQName(prefix, uri, local, fp)
    }
  }

  /**
   * Make a NameTest, using the static context for namespace resolution
   *
   * @param nodeType   the type of node required (identified by a constant in
   *                   class Type)
   * @param qname      the lexical QName of the required node; alternatively,
   *                   a QName in Clark notation ({uri}local)
   * @param useDefault true if the default namespace should be used when
   *                   the QName is unprefixed
   * @return a NameTest, representing a pattern that tests for a node of a
   *         given node kind and a given name
   * @throws XPathException if the QName is invalid
   */
  @throws[XPathException]
  def makeNameTest(nodeType: Short, qname: String, useDefault: Boolean): NodeTest = {
    import UnprefixedElementMatchingPolicy._
    val pool = env.getConfiguration.getNamePool
    var defaultNS = ""
    if (useDefault && nodeType == Type.ELEMENT && !qname.startsWith("Q{") && !qname.contains(":")) {
      val policy = env.getUnprefixedElementMatchingPolicy
      policy match {
        case DEFAULT_NAMESPACE =>
          defaultNS = env.getDefaultElementNamespace
        case DEFAULT_NAMESPACE_OR_NONE =>
          defaultNS = env.getDefaultElementNamespace
          val q = makeStructuredQName(qname, defaultNS)
          val fp1 = pool.allocateFingerprint(q.getURI, q.getLocalPart)
          val test1 = new NameTest(nodeType, fp1, pool)
          val fp2 = pool.allocateFingerprint("", q.getLocalPart)
          val test2 = new NameTest(nodeType, fp2, pool)
          return new CombinedNodeTest(test1, Token.UNION, test2)
        case ANY_NAMESPACE =>
          if (!NameChecker.isValidNCName(qname)) grumble("Invalid name '" + qname + "'")
          return new LocalNameTest(pool, nodeType, qname)
      }
    }
    val q = makeStructuredQName(qname, defaultNS)
    val fp = pool.allocateFingerprint(q.getURI, q.getLocalPart)
    new NameTest(nodeType, fp, pool)
  }

  @throws[XPathException]
  def makeQNameTest(nodeType: Short, qname: String): NameTest = {
    val pool = env.getConfiguration.getNamePool
    val q = makeStructuredQName(qname, "")
    assert(q != null)
    val fp = pool.allocateFingerprint(q.getURI, q.getLocalPart)
    new NameTest(nodeType, fp, pool)
  }

  /**
   * Make a NamespaceTest (name:*)
   *
   * @param nodeType integer code identifying the type of node required
   * @param prefix   the namespace prefix
   * @return the NamespaceTest, a pattern that matches all nodes in this
   *         namespace
   * @throws XPathException if the namespace prefix is not declared
   */
  @throws[XPathException]
  def makeNamespaceTest(nodeType: Short, prefix: String): NamespaceTest = {
    val pool = env.getConfiguration.getNamePool
    if (scanOnly) { // return an arbitrary namespace if we're only doing a syntax check
      return new NamespaceTest(pool, nodeType, NamespaceConstant.SAXON)
    }
    if (prefix.startsWith("Q{")) {
      val uri = prefix.substring(2, prefix.length - 2)
      return new NamespaceTest(pool, nodeType, uri)
    }
    try {
      val sq = qNameParser.parse(prefix + ":dummy", "")
      new NamespaceTest(pool, nodeType, sq.getURI)
    } catch {
      case err: XPathException =>
        grumble(err.getMessage, err.getErrorCodeLocalPart)
        null
    }
  }

  /**
   * Make a LocalNameTest (*:name)
   *
   * @param nodeType  the kind of node to be matched
   * @param localName the requred local name
   * @return a LocalNameTest, a pattern which matches all nodes of a given
   *         local name, regardless of namespace
   * @throws XPathException if the local name is invalid
   */
  @throws[XPathException]
  def makeLocalNameTest(nodeType: Short, localName: String): LocalNameTest = {
    if (!NameChecker.isValidNCName(localName)) grumble("Local name [" + localName + "] contains invalid characters")
    new LocalNameTest(env.getConfiguration.getNamePool, nodeType, localName)
  }

  /**
   * Set location information on an expression. At present this consists of a simple
   * line number. Needed mainly for XQuery.
   *
   * @param exp the expression whose location information is to be set
   */
  def setLocation(exp: Expression): Unit = setLocation(exp, t.currentTokenStartOffset)

  /**
   * Set location information on an expression. At present only the line number
   * is retained. Needed mainly for XQuery. This version of the method supplies an
   * explicit offset (character position within the expression or query), which the tokenizer
   * can convert to a line number and column number.
   *
   * @param exp    the expression whose location information is to be set
   * @param offset the character position within the expression (ignoring newlines)
   */
  def setLocation(exp: Expression, offset: Int): Unit =
    if (exp != null)
      if (exp.getLocation == null || (exp.getLocation eq Loc.NONE))
        exp.setLocation(makeLocation(offset))

  /**
   * Make a location object corresponding to a specified offset in the query
   */
  def makeLocation(offset: Int): Location = {
    val line = t.getLineNumber(offset)
    val column = t.getColumnNumber(offset)
    makeNestedLocation(env.getContainingLocation, line, column, null)
  }

  /**
   * Set location information on a clause of a FLWOR expression. This version of the method supplies an
   * explicit offset (character position within the expression or query), which the tokenizer
   * can convert to a line number and column number.
   *
   * @param clause the clause whose location information is to be set
   * @param offset the character position within the expression (ignoring newlines)
   */
  def setLocation(clause: Clause, offset: Int): Unit = {
    val line = t.getLineNumber(offset)
    val column = t.getColumnNumber(offset)
    val loc = makeNestedLocation(env.getContainingLocation, line, column, null)
    clause.setLocation(loc)
    clause.setPackageData(env.getPackageData)
  }

  private var mostRecentLocation: Location = Loc.NONE

  def makeLocation: Location = if (t.getLineNumber == mostRecentLocation.getLineNumber && t.getColumnNumber == mostRecentLocation.getColumnNumber && ((env.getSystemId == null && mostRecentLocation.getSystemId == null) || env.getSystemId == mostRecentLocation.getSystemId)) mostRecentLocation
  else {
    val line = t.getLineNumber
    val column = t.getColumnNumber
    mostRecentLocation = makeNestedLocation(env.getContainingLocation, line, column, null)
    mostRecentLocation
  }

  /**
   * Make a Location object relative to an existing location
   *
   * @param containingLoc the containing location
   * @param line          the line number relative to the containing location (zero-based)
   * @param column        the column number relative to the containing location (zero-based)
   * @param nearbyText    (maybe null) expression text around the point of the error
   * @return a suitable Location object
   */
  def makeNestedLocation(containingLoc: Location, line: Int, column: Int, nearbyText: String): Location =
    if (containingLoc.isInstanceOf[Loc] && containingLoc.getLineNumber <= 1 && containingLoc.getColumnNumber == -1 && nearbyText == null) {
      new Loc(env.getSystemId, line + 1, column + 1)
    }
    else new XPathParser.NestedLocation(containingLoc, line, column, nearbyText)

  /**
   * If tracing, wrap an expression in a trace instruction
   *
   * @param exp   the expression to be wrapped
   * @param qName the name of the construct (if applicable)
   * @return the expression that does the tracing
   */
  def makeTracer(exp: Expression, qName: StructuredQName): Expression = {
    exp.setRetainedStaticContextLocally(env.makeRetainedStaticContext)
    //        if (codeInjector != null) {
    //            return codeInjector.inject(exp, env, construct, qName);
    //        } else {
    //            return exp;
    exp
  }

  /**
   * Test whether the current token is a given keyword.
   *
   * @param s The string to be compared with the current token
   * @return true if they are the same
   */
  def isKeyword(s: String): Boolean = t.currentToken == Token.NAME && t.currentTokenValue == s

  /**
   * Set that we are parsing in "scan only"
   *
   * @param scanOnly true if parsing is to proceed in scan-only mode. In this mode
   *                 namespace bindings are not yet known, so no attempt is made to look up namespace
   *                 prefixes.
   */
  def setScanOnly(scanOnly: Boolean): Unit = this.scanOnly = scanOnly

  /**
   * Say whether an absent expression is permitted
   *
   * @param allowEmpty true if it is permitted for the expression to consist
   *                   only of whitespace and comments, in which case the result
   *                   of parsing will be an EmptySequence literal
   */
  def setAllowAbsentExpression(allowEmpty: Boolean): Unit = this.allowAbsentExpression = allowEmpty

  /**
   * Ask whether an absent expression is permitted
   *
   * @return true if it is permitted for the expression to consist
   *         only of whitespace and comments, in which case the result
   *         of parsing will be an EmptySequence literal
   */
  def isAllowAbsentExpression(allowEmpty: Boolean): Boolean = this.allowAbsentExpression
}