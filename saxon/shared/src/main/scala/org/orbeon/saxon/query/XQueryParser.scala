


package org.orbeon.saxon.query

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.event.PipelineConfiguration
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.flwor._
import org.orbeon.saxon.expr.instruct._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.GenericAtomicComparer
import org.orbeon.saxon.expr.sort.SortKeyDefinition
import org.orbeon.saxon.functions._
import org.orbeon.saxon.functions.registry.ConstructorFunctionLibrary
import org.orbeon.saxon.lib._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.AnyNodeTest
import org.orbeon.saxon.pattern.QNameTest
import org.orbeon.saxon.pattern.UnionQNameTest
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.serialize.CharacterMapIndex
import org.orbeon.saxon.serialize.SerializationParamsHandler
import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet
import org.orbeon.saxon.trans._
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.tree.util.NamespaceResolverWithDefault
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.IntHashSet
import javax.xml.transform.{OutputKeys, Source, TransformerException}
import java.io.IOException
import java.net.URI
import java.net.URISyntaxException
import java.util
import java.util._
import java.util.function.IntPredicate
import java.util.regex.Pattern

import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.expr.parser.XPathParser.ParsedLanguage
import org.orbeon.saxon.query.XQueryParser.{Import, SortSpec}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks
import scala.util.control.Breaks._

object XQueryParser {
  val XQUERY10 = "1.0"
  val XQUERY30 = "3.0"
  val XQUERY31 = "3.1"


  private def normalizeLineEndings11(in: String): String = {
    if (in.indexOf(0xd.toChar) < 0 && in.indexOf(0x85.toChar) < 0 && in.indexOf(0x2028.toChar) < 0) return in
    val sb = new FastStringBuffer(in.length)
    var i = 0
    while ( {
      i < in.length
    }) {
      val ch = in.charAt(i)
      ch match {
        case 0x85 =>
        case 0x2028 =>
          sb.cat(0xa.toChar)
        case 0xd =>
          if (i < in.length - 1 && (in.charAt(i + 1) == 0xa.toChar || in.charAt(i + 1) == 0x85.toChar)) {
            sb.cat(0xa.toChar)
            i += 1
          }
          else sb.cat(0xa.toChar)
        case _ =>
          sb.cat(ch)
      }
      i += 1
    }
    sb.toString
  }


  private def normalizeLineEndings10(in: String): String = {
    if (in.indexOf(0xd.toChar) < 0) return in
    val sb = new FastStringBuffer(in.length)
    var i = 0
    while ( {
      i < in.length
    }) {
      val ch = in.charAt(i)
      if (ch == 0xd) if (i < in.length - 1 && in.charAt(i + 1) == 0xa.toChar) {
        sb.cat(0xa.toChar)
        i += 1
      }
      else sb.cat(0xa.toChar)
      else sb.cat(ch)
      i += 1
    }
    sb.toString
  }

  private val encNamePattern = Pattern.compile("^[A-Za-z]([A-Za-z0-9._\\x2D])*$")
  val SAXON_MEMO_FUNCTION = new StructuredQName("saxon", NamespaceConstant.SAXON, "memo-function")

   def containsLoopingClause(clauseList: List[Clause]): Boolean = {

    for (c <- clauseList.asScala) {
      if (FLWORExpression.isLoopingClause(c)) return true
    }
    false
  }


  def makeStringJoin(exp: Expression, env: StaticContext): Expression = {
    var express = exp
    express = Atomizer.makeAtomizer(express, null)
    val t = express.getItemType
    if (!(t == BuiltInAtomicType.STRING) && !(t == BuiltInAtomicType.UNTYPED_ATOMIC)) {
      express = new AtomicSequenceConverter(express, BuiltInAtomicType.STRING)
      express.asInstanceOf[AtomicSequenceConverter].allocateConverterStatically(env.getConfiguration, allowNull = false)
    }
    if (express.getCardinality == StaticProperty.EXACTLY_ONE) express
    else {
      val rsc = new RetainedStaticContext(env)
      val fn = SystemFunction.makeCall("string-join", rsc, express, new StringLiteral(StringValue.SINGLE_SPACE))
      ExpressionTool.copyLocationInfo(express, fn)
      fn
    }
  }

  class SortSpec {
    var sortKey: Expression = null
    var ascending: Boolean = false
    var emptyLeast: Boolean = false
    var collation: String = null
  }


  def stringify(exp: Expression, noNodeIfEmpty: Boolean, env: StaticContext): Expression = {
    var express = exp
    if (express.isInstanceOf[StringLiteral]) return express
    if (express.getLocalRetainedStaticContext == null) express.setRetainedStaticContext(env.makeRetainedStaticContext)

    express = Atomizer.makeAtomizer(express, null)

    express = new AtomicSequenceConverter(express, BuiltInAtomicType.STRING)


    express = SystemFunction.makeCall("string-join", express.getRetainedStaticContext, express, new StringLiteral(StringValue.SINGLE_SPACE))
    assert(express != null)
    if (noNodeIfEmpty) (express.asInstanceOf[SystemFunctionCall]).getTargetFunction.asInstanceOf[StringJoin].setReturnEmptyIfEmpty(true)

    express
  }

  class Unescaper(val characterChecker: IntPredicate) {
    @throws[XPathException]
    def unescape(token: String): CharSequence = {
      val sb = new FastStringBuffer(token.length)
      var i = 0
      while ( {
        i < token.length
      }) {
        val c = token.charAt(i)
        if (c == '&') {
          val semic = token.indexOf(';', i)
          if (semic < 0) throw new XPathException("No closing ';' found for entity or character reference", "XPST0003")
          else {
            val entity = token.substring(i + 1, semic)
            sb.append(analyzeEntityReference(entity))
            i = semic
          }
        }
        else sb.cat(c)
        i += 1
      }
      sb
    }

    @throws[XPathException]
    def analyzeEntityReference(entity: String): String = if ("lt" == entity) "<"
    else if ("gt" == entity) ">"
    else if ("amp" == entity) "&"
    else if ("quot" == entity) "\""
    else if ("apos" == entity) "'"
    else if (entity.length < 2 || entity.charAt(0) != '#')
      throw new XPathException("invalid character reference &" + entity + ';', "XPST0003")
    else {
      parseCharacterReference(entity)
    }

    @throws[XPathException]
    private def parseCharacterReference(entity: String): String = {
      var value = 0
      var entityStr = entity
      if (entityStr.charAt(1) == 'x') {
        if (entityStr.length < 3) throw new XPathException("No hex digits in hexadecimal character reference", "XPST0003")
        entityStr = entityStr.toLowerCase
        for (i <- 2 until entityStr.length) {
          val digit = "0123456789abcdef".indexOf(entityStr.charAt(i))
          if (digit < 0) throw new XPathException("Invalid hex digit '" + entityStr.charAt(i) + "' in character reference", "XPST0003")
          value = (value * 16) + digit
          if (value > UTF16CharacterSet.NONBMP_MAX) throw new XPathException("Character reference exceeds Unicode codepoint limit", "XQST0090")
        }
      }
      else for (i <- 1 until entityStr.length) {
        val digit = "0123456789".indexOf(entityStr.charAt(i))
        if (digit < 0) throw new XPathException("Invalid digit '" + entityStr.charAt(i) + "' in decimal character reference", "XPST0003")
        value = (value * 10) + digit
        if (value > UTF16CharacterSet.NONBMP_MAX) throw new XPathException("Character reference exceeds Unicode codepoint limit", "XQST0090")
      }
      if (!characterChecker.test(value)) throw new XPathException("Invalid XML character reference x" + Integer.toHexString(value), "XQST0090")


      if (value <= 0x0000ffff) {
        "" + value.toChar
      }
      else {
        assert(value <= 0x0010ffff)
        value -= 0x10000

        "" + (0xd800 | (value >> 10)).toChar + (0xdc00 | (value & 0x0003ff)).toChar
      }
    }
  }

  class AttributeDetails {
    private[query] var value: String = null
    private[query] var startOffset: Int = 0
  }

  class Import {
    private[query] var namespaceURI: String = null
    private[query] var locationURIs: util.List[String] = null
    private[query] var offset: Int = 0
  }

}

class XQueryParser extends XPathParser {

  setLanguage(ParsedLanguage.XSLT_PATTERN, 31)
  private var memoFunction: Boolean = false

  private var streaming: Boolean = false

  private var errorCount: Int = 0

  private var firstError: XPathException = null

   var executable: Executable = _

  private var foundCopyNamespaces: Boolean = false

  private var foundBoundarySpaceDeclaration: Boolean = false

  private var foundOrderingDeclaration: Boolean = false

  private var foundEmptyOrderingDeclaration: Boolean = false

  private var foundDefaultCollation: Boolean = false

  private var foundConstructionDeclaration: Boolean = false

  private var foundDefaultFunctionNamespace: Boolean = false

  private var foundDefaultElementNamespace: Boolean = false

  private var foundBaseURIDeclaration: Boolean = false

  private var foundContextItemDeclaration: Boolean = false

  private var foundDefaultDecimalFormat: Boolean = false

  private var preambleProcessed: Boolean = false

  val importedModules: Set[String] = new HashSet(5)

  val namespacesToBeSealed: List[String] = new ArrayList(10)

  val schemaImports: List[Import] = new ArrayList(5)

  val moduleImports: List[Import] = new ArrayList(5)

  private val outputPropertiesSeen: Set[StructuredQName] = new HashSet(4)

  private var parameterDocProperties: Properties = null

  private def newParser: XQueryParser = {
    val qp: XQueryParser = new XQueryParser
    qp.setLanguage(language, 31)
    qp.setParserExtension(parserExtension)
    qp
  }


  @throws[XPathException]
  def makeXQueryExpression(query: String, mainModule: QueryModule, config: Configuration): XQueryExpression = try {
    setLanguage(ParsedLanguage.XQUERY, 31)
    var queryStr = query
    if (config.getXMLVersion == Configuration.XML10) queryStr = XQueryParser.normalizeLineEndings10(queryStr)
    else queryStr = XQueryParser.normalizeLineEndings11(queryStr)
    var exec = mainModule.getExecutable
    if (exec == null) {
      exec = new Executable(config)
      exec.setHostLanguage(HostLanguage.XQUERY)
      exec.setTopLevelPackage(mainModule.getPackageData)
      setExecutable(exec)

    }
    val requirement = exec.getGlobalContextRequirement
    if (requirement != null) requirement.addRequiredItemType(mainModule.getRequiredContextItemType)
    else if (mainModule.getRequiredContextItemType != null && (mainModule.getRequiredContextItemType ne AnyItemType)) {
      val req = new GlobalContextRequirement
      req.setExternal(true)
      req.addRequiredItemType(mainModule.getRequiredContextItemType)
      exec.setGlobalContextRequirement(req)
    }

    val outputProps = new Properties(config.getDefaultSerializationProperties)
    if (outputProps.getProperty(OutputKeys.METHOD) == null) outputProps.setProperty(OutputKeys.METHOD, "xml")
    parameterDocProperties = new Properties(outputProps)
    exec.setDefaultOutputProperties(new Properties(parameterDocProperties))

    val libList = new FunctionLibraryList
    libList.addFunctionLibrary(new ExecutableFunctionLibrary(config))
    exec.setFunctionLibrary(libList)

    setExecutable(exec)
    setCodeInjector(mainModule.getCodeInjector)
    val exp = parseQuery(queryStr, mainModule)
    if (streaming) env.getConfiguration.checkLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XQUERY, "streaming", -1)
    exec.fixupQueryModules(mainModule)

    val queryExp = config.makeXQueryExpression(exp, mainModule, streaming)


    val userlib = exec.getFunctionLibrary
    val lib = new FunctionLibraryList
    lib.addFunctionLibrary(mainModule.getBuiltInFunctionSet)
    lib.addFunctionLibrary(config.getBuiltInExtensionLibraryList)
    lib.addFunctionLibrary(new ConstructorFunctionLibrary(config))
    lib.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    lib.addFunctionLibrary(mainModule.getGlobalFunctionLibrary)
    config.addExtensionBinders(lib)
    lib.addFunctionLibrary(userlib)
    exec.setFunctionLibrary(lib)
    queryExp
  } catch {
    case e: XPathException =>
      if (!e.hasBeenReported) reportError(e)
      throw e
  }


  def getExecutable: Executable = executable


  def setExecutable(exec: Executable): Unit = executable = exec


  override  def customizeTokenizer(t: Tokenizer): Unit = t.isXQuery = true


  def setStreaming(option: Boolean): Unit = streaming = option


  def isStreaming: Boolean = streaming


  @throws[XPathException]
  private def parseQuery(queryString: String, env: QueryModule): Expression = {
    this.env = Objects.requireNonNull(env)
    charChecker = env.getConfiguration.getValidCharacterChecker
    language = ParsedLanguage.XQUERY
    t = new Tokenizer
    t.languageLevel = 31
    t.isXQuery = true
    try t.tokenize(Objects.requireNonNull(queryString), 0, -1)
    catch {
      case err: XPathException =>
        grumble(err.getMessage)
    }
    parseVersionDeclaration()
    t.allowSaxonExtensions = env.getConfiguration.getBooleanProperty(Feature.ALLOW_SYNTAX_EXTENSIONS)
    allowSaxonExtensions = t.allowSaxonExtensions
    val qp = new QNameParser(env.getLiveNamespaceResolver).withAcceptEQName(true).
      withUnescaper(new XQueryParser.Unescaper(env.getConfiguration.getValidCharacterChecker))
    setQNameParser(qp)
    parseProlog()
    processPreamble()
    var exp = parseExpression
    exp = makeTracer(exp, null)


    if (t.currentToken != Token.EOF) grumble("Unexpected token " + currentTokenDisplay + " beyond end of query")
    setLocation(exp)
    ExpressionTool.setDeepRetainedStaticContext(exp, env.makeRetainedStaticContext)
    if (errorCount == 0) exp
    else {
      val err = new XPathException("One or more static errors were reported during query analysis")
      err.setHasBeenReported(true)
      err.setErrorCodeQName(firstError.getErrorCodeQName)
      throw err
    }
  }


  @throws[XPathException]
  final def parseLibraryModule(queryString: String, env: QueryModule): Unit = {
    this.env = env
    val config = env.getConfiguration
    var queryStr = queryString
    charChecker = config.getValidCharacterChecker
    if (config.getXMLVersion == Configuration.XML10) queryStr = XQueryParser.normalizeLineEndings10(queryStr)
    else queryStr = XQueryParser.normalizeLineEndings11(queryStr)
    val exec = env.getExecutable
    if (exec == null) throw new IllegalStateException("Query library module has no associated Executable")
    executable = exec

    t = new Tokenizer
    t.languageLevel = 31
    t.isXQuery = true
    val qp = new QNameParser(env.getLiveNamespaceResolver).withAcceptEQName(true).
      withUnescaper(new XQueryParser.Unescaper(config.getValidCharacterChecker))
    setQNameParser(qp)
    try t.tokenize(queryStr, 0, -1)
    catch {
      case err: XPathException =>
        grumble(err.getMessage)
    }
    parseVersionDeclaration()

    parseModuleDeclaration()
    parseProlog()
    processPreamble()
    if (t.currentToken != Token.EOF) grumble("Unrecognized content found after the variable and function declarations in a library module")
    if (errorCount != 0) {
      val err = new XPathException("Static errors were reported in the imported library module")
      err.setErrorCodeQName(firstError.getErrorCodeQName)
      throw err
    }
  }

  @throws[XPathException]
  private def reportError(exception: XPathException) = {
    errorCount += 1
    if (firstError == null) firstError = exception
    env.asInstanceOf[QueryModule].reportStaticError(exception)
    throw exception
  }


  @throws[XPathException]
  private def parseVersionDeclaration(): Unit = if (t.currentToken == Token.XQUERY_VERSION) {
    nextToken()
    expect(Token.STRING_LITERAL)
    val queryVersion = unescape(t.currentTokenValue).toString
    val allowedVersions = Array[String]("1.0", "3.0", "3.1")
    if (util.Arrays.binarySearch(allowedVersions.asInstanceOf[Array[AnyRef]], queryVersion) < 0) {
      grumble("Invalid XQuery version " + queryVersion, "XQST0031")
    }
    nextToken()
    if ("encoding" == t.currentTokenValue) {
      nextToken()
      expect(Token.STRING_LITERAL)
      if (!XQueryParser.encNamePattern.matcher(unescape(t.currentTokenValue)).matches) grumble("Encoding name contains invalid characters", "XQST0087")

      nextToken()
    }
    expect(Token.SEMICOLON)
    nextToken()
  }
  else {

    if (t.currentToken == Token.XQUERY_ENCODING) {
      nextToken()
      expect(Token.STRING_LITERAL)
      if (!XQueryParser.encNamePattern.matcher(t.currentTokenValue).matches) grumble("Encoding name contains invalid characters", "XQST0087")
      nextToken()
      expect(Token.SEMICOLON)
      nextToken()
    }
  }


  @throws[XPathException]
  private def parseModuleDeclaration(): Unit = {
    expect(Token.MODULE_NAMESPACE)
    nextToken()
    expect(Token.NAME)
    val prefix = t.currentTokenValue
    nextToken()
    expect(Token.EQUALS)
    nextToken()
    expect(Token.STRING_LITERAL)
    var uri = uriLiteral(t.currentTokenValue)
    checkProhibitedPrefixes(prefix, uri)
    if (uri.isEmpty) {
      grumble("Module namespace cannot be \"\"", "XQST0088")
      uri = "http:"
    }
    nextToken()
    expect(Token.SEMICOLON)
    nextToken()
    try {
      env.asInstanceOf[QueryModule].setModuleNamespace(uri)
      env.asInstanceOf[QueryModule].declarePrologNamespace(prefix, uri)
      executable.addQueryLibraryModule(env.asInstanceOf[QueryModule])
    } catch {
      case err: XPathException =>
        err.setLocator(makeLocation)
        reportError(err)
    }
  }


  @throws[XPathException]
  private def parseProlog(): Unit = {
    var allowModuleDecl = true
    var allowDeclarations = true
    while ( {
      true
    }) try {
      if (t.currentToken == Token.MODULE_NAMESPACE) {
        val uri = env.asInstanceOf[QueryModule].getModuleNamespace
        if (uri == null) grumble("Module declaration must not be used in a main module")
        else grumble("Module declaration appears more than once")
        if (!allowModuleDecl) grumble("Module declaration must precede other declarations in the query prolog")
      }
      allowModuleDecl = false
      t.currentToken match {
        case Token.DECLARE_NAMESPACE =>
          if (!allowDeclarations) grumble("Namespace declarations cannot follow variables, functions, or options")

          parseNamespaceDeclaration()
        case Token.DECLARE_ANNOTATED =>

          processPreamble()
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          nextToken()
          expect(Token.PERCENT)
          val annotationList = parseAnnotationsList
          if (isKeyword("function")) {
            annotationList.check(env.getConfiguration, "DF")
            parseFunctionDeclaration(annotationList)
          }
          else if (isKeyword("variable")) {
            annotationList.check(env.getConfiguration, "DV")
            parseVariableDeclaration(annotationList)
          }
          else grumble("Annotations can appear only in 'declare variable' and 'declare function'")
        case Token.DECLARE_DEFAULT =>
          nextToken()
          expect(Token.NAME)
          t.currentTokenValue match {
            case "element" =>
              if (!allowDeclarations) grumble("Namespace declarations cannot follow variables, functions, or options")
              parseDefaultElementNamespace()
            case "function" =>
              if (!allowDeclarations) grumble("Namespace declarations cannot follow variables, functions, or options")
              parseDefaultFunctionNamespace()
            case "collation" =>
              if (!allowDeclarations) grumble("Collation declarations must appear earlier in the prolog")
              parseDefaultCollation()
            case "order" =>
              if (!allowDeclarations) grumble("Order declarations must appear earlier in the prolog")
              parseDefaultOrder()
            case "decimal-format" =>
              nextToken()
              parseDefaultDecimalFormat()
            case _ =>
              grumble("After 'declare default', expected 'element', 'function', or 'collation'")
          }
        case Token.DECLARE_BOUNDARY_SPACE =>
          if (!allowDeclarations) grumble("'declare boundary-space' must appear earlier in the query prolog")
          parseBoundarySpaceDeclaration()
        case Token.DECLARE_ORDERING =>
          if (!allowDeclarations) grumble("'declare ordering' must appear earlier in the query prolog")
          parseOrderingDeclaration()
        case Token.DECLARE_COPY_NAMESPACES =>
          if (!allowDeclarations) grumble("'declare copy-namespaces' must appear earlier in the query prolog")
          parseCopyNamespacesDeclaration()
        case Token.DECLARE_BASEURI =>
          if (!allowDeclarations) grumble("'declare base-uri' must appear earlier in the query prolog")
          parseBaseURIDeclaration()
        case Token.DECLARE_DECIMAL_FORMAT =>
          if (!allowDeclarations) grumble("'declare decimal-format' must appear earlier in the query prolog")
          parseDecimalFormatDeclaration()
        case Token.IMPORT_SCHEMA =>
          if (!allowDeclarations) grumble("Import schema must appear earlier in the prolog")
          parseSchemaImport()
        case Token.IMPORT_MODULE =>
          if (!allowDeclarations) grumble("Import module must appear earlier in the prolog")
          parseModuleImport()
        case Token.DECLARE_VARIABLE =>
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          processPreamble()
          parseVariableDeclaration(AnnotationList.EMPTY)
        case Token.DECLARE_CONTEXT =>
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          processPreamble()
          parseContextItemDeclaration()
        case Token.DECLARE_FUNCTION =>
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          processPreamble()
          parseFunctionDeclaration(AnnotationList.EMPTY)
        case Token.DECLARE_UPDATING =>
          nextToken()
          if (!isKeyword("function")) grumble("expected 'function' after 'declare updating")
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          processPreamble()
          parserExtension.parseUpdatingFunctionDeclaration(this)
        case Token.DECLARE_OPTION =>
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          parseOptionDeclaration()
        case Token.DECLARE_TYPE =>
          checkSyntaxExtensions("declare type")
          if (allowDeclarations) {
            sealNamespaces(namespacesToBeSealed, env.getConfiguration)
            allowDeclarations = false
          }
          parseTypeAliasDeclaration()
        case Token.DECLARE_CONSTRUCTION =>
          if (!allowDeclarations) grumble("'declare construction' must appear earlier in the query prolog")
          parseConstructionDeclaration()
        case Token.DECLARE_REVALIDATION =>
          if (!allowDeclarations) grumble("'declare revalidation' must appear earlier in the query prolog")
          parserExtension.parseRevalidationDeclaration(this)
        case Token.EOF =>
          val uri = env.asInstanceOf[QueryModule].getModuleNamespace
          if (uri == null) grumble("The main module must contain a query expression after any declarations in the prolog")
          else return
        case _ =>
          return
      }
      expect(Token.SEMICOLON)
      nextToken()
    } catch {
      case err: XPathException =>
        if (err.getLocator == null) err.setLocator(makeLocation)
        if (!err.hasBeenReported) {
          errorCount += 1
          if (firstError == null) firstError = err
          reportError(err)
        }


        while ( {
          t.currentToken != Token.SEMICOLON
        }) {
          nextToken()
          if (t.currentToken == Token.EOF) return
          else if (t.currentToken == Token.RCURLY) t.lookAhead()
          else if (t.currentToken == Token.TAG) parsePseudoXML(true)
        }
        nextToken()
    }
  }


  @throws[XPathException]
  override  def parseAnnotationsList: AnnotationList = {
    val annotations = new util.ArrayList[Annotation]
    val options: Int = 0
    while (true) {
      t.setState(Tokenizer.BARE_NAME_STATE)
      nextToken()
      expect(Token.NAME)
      t.setState(Tokenizer.DEFAULT_STATE)
      var qName: StructuredQName = null
      var uri: String = null
      if (t.currentTokenValue.indexOf(':') < 0) {
        uri = NamespaceConstant.XQUERY
        qName = new StructuredQName("", uri, t.currentTokenValue)
      }
      else {
        qName = makeStructuredQName(t.currentTokenValue, "")
        assert(qName != null)
        uri = qName.getURI
      }
      val annotation = new Annotation(qName)
      if (uri == NamespaceConstant.XQUERY) {
        if (!(qName == Annotation.PRIVATE) && !(qName == Annotation.PUBLIC) &&
          !(qName == Annotation.UPDATING) && !(qName == Annotation.SIMPLE))
          grumble("Unrecognized variable or function annotation " + qName.getDisplayName, "XQST0045")
        annotation.addAnnotationParameter(new Int64Value(options))
      }
      else if (isReservedInQuery(uri)) grumble("The annotation " + t.currentTokenValue + " is in a reserved namespace", "XQST0045")
      else if (uri == "") grumble("The annotation " + t.currentTokenValue + " is in no namespace", "XQST0045")
      else {

      }
      nextToken()
      if (t.currentToken == Token.LPAR) {
        nextToken()
        if (t.currentToken == Token.RPAR) grumble("Annotation parameter list cannot be empty")
        breakable {
          while ( {
            true
          }) {
            var arg: Literal = null
            t.currentToken match {
              case Token.STRING_LITERAL =>
                arg = parseStringLiteral(false).asInstanceOf[Literal]
              case Token.NUMBER =>
                arg = parseNumericLiteral(false).asInstanceOf[Literal]
              case _ =>
                grumble("Annotation parameter must be a literal")
                return null
            }
            val `val` = arg.getValue
            if (`val`.isInstanceOf[StringValue] || `val`.isInstanceOf[NumericValue]) annotation.addAnnotationParameter(`val`.asInstanceOf[AtomicValue])
            else grumble("Annotation parameter must be a string or number")
            if (t.currentToken == Token.RPAR) {
              nextToken()
              break()
            }
            expect(Token.COMMA)
            nextToken()
          }
        }
      }
      annotations.add(annotation)
      if (t.currentToken != Token.PERCENT) return new AnnotationList(annotations)
    }
    null
  }

  private def sealNamespaces(namespacesToBeSealed: util.List[String], config: Configuration): Unit = {

    for (aNamespacesToBeSealed <- namespacesToBeSealed.asScala) {
      val ns = aNamespacesToBeSealed.asInstanceOf[String]
      config.sealNamespace(ns)
    }
  }


  @throws[XPathException]
  private def processPreamble(): Unit = {
    if (preambleProcessed) return
    preambleProcessed = true
    if (foundDefaultCollation) {
      var collationName: String = env.getDefaultCollationName
      var collationURI: URI = null
      try {
        collationURI = new URI(collationName)
        if (!collationURI.isAbsolute) {
          val base = new URI(env.getStaticBaseURI)
          collationURI = base.resolve(collationURI)
          collationName = collationURI.toString
        }
      } catch {
        case err: URISyntaxException =>
          grumble("Default collation name '" + collationName + "' is not a valid URI", "XQST0046")
          collationName = NamespaceConstant.CODEPOINT_COLLATION_URI
      }
      if (env.getConfiguration.getCollation(collationName) == null) {
        grumble("Default collation name '" + collationName + "' is not a recognized collation", "XQST0038")
        collationName = NamespaceConstant.CODEPOINT_COLLATION_URI
      }
      env.asInstanceOf[QueryModule].setDefaultCollationName(collationName)
    }

    for (imp <- schemaImports.asScala) {
      try applySchemaImport(imp)
      catch {
        case err: XPathException =>
          if (!err.hasBeenReported) {
            err.maybeSetLocation(makeLocation(imp.offset))
            throw err
          }
      }
    }

    for (imp <- moduleImports.asScala) {
      try applyModuleImport(imp)
      catch {
        case err: XPathException =>
          if (!err.hasBeenReported) {
            err.maybeSetLocation(makeLocation(imp.offset))
            throw err
          }
      }
    }
  }

  @throws[XPathException]
  private def parseDefaultCollation(): Unit = {
    if (foundDefaultCollation) grumble("default collation appears more than once", "XQST0038")
    foundDefaultCollation = true
    nextToken()
    expect(Token.STRING_LITERAL)
    val uri = uriLiteral(t.currentTokenValue)
    env.asInstanceOf[QueryModule].setDefaultCollationName(uri)
    nextToken()
  }


  @throws[XPathException]
  private def parseDefaultOrder(): Unit = {
    if (foundEmptyOrderingDeclaration) grumble("empty ordering declaration appears more than once", "XQST0069")
    foundEmptyOrderingDeclaration = true
    nextToken()
    if (!isKeyword("empty")) grumble("After 'declare default order', expected keyword 'empty'")
    nextToken()
    if (isKeyword("least")) env.asInstanceOf[QueryModule].setEmptyLeast(true)
    else if (isKeyword("greatest")) env.asInstanceOf[QueryModule].setEmptyLeast(false)
    else grumble("After 'declare default order empty', expected keyword 'least' or 'greatest'")
    nextToken()
  }


  @throws[XPathException]
  private def parseBoundarySpaceDeclaration(): Unit = {
    if (foundBoundarySpaceDeclaration) grumble("'declare boundary-space' appears more than once", "XQST0068")
    foundBoundarySpaceDeclaration = true
    nextToken()
    expect(Token.NAME)
    if ("preserve" == t.currentTokenValue) env.asInstanceOf[QueryModule].setPreserveBoundarySpace(true)
    else if ("strip" == t.currentTokenValue) env.asInstanceOf[QueryModule].setPreserveBoundarySpace(false)
    else grumble("boundary-space must be 'preserve' or 'strip'")
    nextToken()
  }


  @throws[XPathException]
  private def parseOrderingDeclaration(): Unit = {
    if (foundOrderingDeclaration) grumble("ordering mode declaration appears more than once", "XQST0065")
    foundOrderingDeclaration = true
    nextToken()
    expect(Token.NAME)
    if ("ordered" == t.currentTokenValue) {

    }
    else if ("unordered" == t.currentTokenValue) {
    }
    else grumble("ordering mode must be 'ordered' or 'unordered'")
    nextToken()
  }


  @throws[XPathException]
  private def parseCopyNamespacesDeclaration(): Unit = {
    if (foundCopyNamespaces) grumble("declare copy-namespaces appears more than once", "XQST0055")
    foundCopyNamespaces = true
    nextToken()
    expect(Token.NAME)
    if ("preserve" == t.currentTokenValue) env.asInstanceOf[QueryModule].setPreserveNamespaces(true)
    else if ("no-preserve" == t.currentTokenValue) env.asInstanceOf[QueryModule].setPreserveNamespaces(false)
    else grumble("copy-namespaces must be followed by 'preserve' or 'no-preserve'")
    nextToken()
    expect(Token.COMMA)
    nextToken()
    expect(Token.NAME)
    if ("inherit" == t.currentTokenValue) env.asInstanceOf[QueryModule].setInheritNamespaces(true)
    else if ("no-inherit" == t.currentTokenValue) env.asInstanceOf[QueryModule].setInheritNamespaces(false)
    else grumble("After the comma in the copy-namespaces declaration, expected 'inherit' or 'no-inherit'")
    nextToken()
  }


  @throws[XPathException]
  private def parseConstructionDeclaration(): Unit = {
    if (foundConstructionDeclaration) grumble("declare construction appears more than once", "XQST0067")
    foundConstructionDeclaration = true
    nextToken()
    expect(Token.NAME)
    var `val` = 0
    if ("preserve" == t.currentTokenValue) {
      `val` = Validation.PRESERVE


    }
    else if ("strip" == t.currentTokenValue) `val` = Validation.STRIP
    else {
      grumble("construction mode must be 'preserve' or 'strip'")
      `val` = Validation.STRIP
    }
    env.asInstanceOf[QueryModule].setConstructionMode(`val`)
    nextToken()
  }


  @throws[XPathException]
   def parseRevalidationDeclaration(): Unit = grumble("declare revalidation is allowed only in XQuery Update")


  @throws[XPathException]
  private def parseSchemaImport(): Boolean = {
    ensureSchemaAware("import schema")
    var sImport: Import = new XQueryParser.Import
    var prefix: String = null
    sImport.namespaceURI = null
    sImport.locationURIs = new util.ArrayList[String](5)
    sImport.offset = t.currentTokenStartOffset
    nextToken()
    if (isKeyword("namespace")) prefix = readNamespaceBinding
    else if (isKeyword("default")) {
      nextToken()
      if (!isKeyword("element")) grumble("In 'import schema', expected 'element namespace'")
      nextToken()
      if (!isKeyword("namespace")) grumble("In 'import schema', expected keyword 'namespace'")
      nextToken()
      prefix = ""
    }
    if (t.currentToken == Token.STRING_LITERAL) {
      val uri = uriLiteral(t.currentTokenValue)
      checkProhibitedPrefixes(prefix, uri)
      sImport.namespaceURI = uri
      nextToken()
      if (isKeyword("at")) {
        nextToken()
        expect(Token.STRING_LITERAL)
        sImport.locationURIs.add(uriLiteral(t.currentTokenValue))
        nextToken()
        while ( {
          t.currentToken == Token.COMMA
        }) {
          nextToken()
          expect(Token.STRING_LITERAL)
          sImport.locationURIs.add(uriLiteral(t.currentTokenValue))
          nextToken()
        }
      }
      else if (t.currentToken != Token.SEMICOLON) grumble("After the target namespace URI, expected 'at' or ';'")
    }
    else grumble("After 'import schema', expected 'namespace', 'default', or a string-literal")
    if (prefix != null) try if (prefix.isEmpty) env.asInstanceOf[QueryModule].setDefaultElementNamespace(sImport.namespaceURI)
    else {
      if (sImport.namespaceURI == null || "" == sImport.namespaceURI) grumble("A prefix cannot be bound to the null namespace", "XQST0057")
      env.asInstanceOf[QueryModule].declarePrologNamespace(prefix, sImport.namespaceURI)
    }
    catch {
      case err: XPathException =>
        err.setLocator(makeLocation)
        reportError(err)
    }

    breakable {
      for (schemaImport <- schemaImports.asScala) {
        val imp = schemaImport.asInstanceOf[XQueryParser.Import]
        if (imp.namespaceURI == sImport.namespaceURI) {
          grumble("Schema namespace '" + sImport.namespaceURI + "' is imported more than once", "XQST0058")
          break()
        }
      }
    }
    schemaImports.add(sImport)
  }

  @throws[XPathException]
  private def readNamespaceBinding: String = {
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    expect(Token.NAME)
    val prefix = t.currentTokenValue
    nextToken()
    expect(Token.EQUALS)
    nextToken()
    prefix
  }

  @throws[XPathException]
   def ensureSchemaAware(featureName: String): Unit = {
    if (!env.getConfiguration.isLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XQUERY)) throw new XPathException("This Saxon version and license does not allow use of '" + featureName + "'", "XQST0009")
    env.getConfiguration.checkLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XQUERY, featureName, -1)
    getExecutable.setSchemaAware(true)
    getStaticContext.getPackageData.setSchemaAware(true)
  }

  @throws[XPathException]
  private def applySchemaImport(sImport: XQueryParser.Import): Unit = {
    val config = env.getConfiguration

    config synchronized {
      if (!config.isSchemaAvailable(sImport.namespaceURI)) if (!sImport.locationURIs.isEmpty) try {
        val pipe = config.makePipelineConfiguration
        config.readMultipleSchemas(pipe, env.getStaticBaseURI, sImport.locationURIs, sImport.namespaceURI)
        namespacesToBeSealed.add(sImport.namespaceURI)
      } catch {
        case err: SchemaException =>
          grumble("Error in schema " + sImport.namespaceURI + ": " + err.getMessage, "XQST0059", sImport.offset)
      }
      else if (sImport.namespaceURI == NamespaceConstant.XML || sImport.namespaceURI == NamespaceConstant.FN || sImport.namespaceURI == NamespaceConstant.SCHEMA_INSTANCE) config.addSchemaForBuiltInNamespace(sImport.namespaceURI)
      else grumble("Unable to locate requested schema " + sImport.namespaceURI, "XQST0059", sImport.offset)
      env.asInstanceOf[QueryModule].addImportedSchema(sImport.namespaceURI, env.getStaticBaseURI, sImport.locationURIs)
    }
  }


  @throws[XPathException]
  private def parseModuleImport(): Unit = {
    val thisModule = env.asInstanceOf[QueryModule]
    val mImport = new XQueryParser.Import
    var prefix: String = null
    mImport.namespaceURI = null
    mImport.locationURIs = new util.ArrayList[String](5)
    mImport.offset = t.currentTokenStartOffset
    nextToken()
    if (t.currentToken == Token.NAME && t.currentTokenValue == "namespace") prefix = readNamespaceBinding
    if (t.currentToken == Token.STRING_LITERAL) {
      val uri = uriLiteral(t.currentTokenValue)
      checkProhibitedPrefixes(prefix, uri)
      mImport.namespaceURI = uri
      if (mImport.namespaceURI.isEmpty) {
        grumble("Imported module namespace cannot be \"\"", "XQST0088")
        mImport.namespaceURI = "http:"
      }
      if (importedModules.contains(mImport.namespaceURI)) grumble("Two 'import module' declarations specify the same module namespace", "XQST0047")
      importedModules.add(mImport.namespaceURI)
      env.asInstanceOf[QueryModule].addImportedNamespace(mImport.namespaceURI)
      nextToken()
      if (isKeyword("at")) do {
        nextToken()
        expect(Token.STRING_LITERAL)
        mImport.locationURIs.add(uriLiteral(t.currentTokenValue))
        nextToken()
      } while ( {
        t.currentToken == Token.COMMA
      })
    }
    else grumble("After 'import module', expected 'namespace' or a string-literal")
    if (prefix != null) try if (mImport.namespaceURI == thisModule.getModuleNamespace && mImport.namespaceURI == thisModule.checkURIForPrefix(prefix)) {

    }
    else thisModule.declarePrologNamespace(prefix, mImport.namespaceURI)
    catch {
      case err: XPathException =>
        err.setLocator(makeLocation)
        reportError(err)
    }


    moduleImports.add(mImport)
  }

  @throws[XPathException]
  def applyModuleImport(mImport: XQueryParser.Import): Unit = {


    var existingModules: util.List[QueryModule] = null

    for (i <- 0 until mImport.locationURIs.size) {
      try {
        val uri = mImport.locationURIs.get(i)
        val abs = ResolveURI.makeAbsolute(uri, env.getStaticBaseURI)
        mImport.locationURIs.set(i, abs.toString)
      } catch {
        case e: URISyntaxException =>
          grumble("Invalid URI " + mImport.locationURIs.get(i) + ": " + e.getMessage, "XQST0046", mImport.offset)
      }
    }

    val lib = env.asInstanceOf[QueryModule].getUserQueryContext.getCompiledLibrary(mImport.namespaceURI)
    if (lib != null) {
      executable.addQueryLibraryModule(lib)
      existingModules = new util.ArrayList[QueryModule]
      existingModules.add(lib)
      lib.link(env.asInstanceOf[QueryModule])
    }
    else if (!env.getConfiguration.getBooleanProperty(Feature.XQUERY_MULTIPLE_MODULE_IMPORTS)) {

      val list = executable.getQueryLibraryModules(mImport.namespaceURI)
      if (list != null && !list.isEmpty) return
    }
    else for (h <- mImport.locationURIs.size - 1 to 0 by -1) {
      if (executable.isQueryLocationHintProcessed(mImport.locationURIs.get(h))) mImport.locationURIs.remove(h)
    }

    if (mImport.locationURIs.isEmpty) {
      val list = executable.getQueryLibraryModules(mImport.namespaceURI)
      if (list != null && !list.isEmpty) return
    }

    var resolver = env.asInstanceOf[QueryModule].getUserQueryContext.getModuleURIResolver
    val hints = new Array[String](mImport.locationURIs.size)
    for (h <- 0 until hints.length) {
      hints(h) = mImport.locationURIs.get(h)
    }
    var sources: Array[StreamSource] = null
    if (resolver != null) try sources = resolver.resolve(mImport.namespaceURI, env.getStaticBaseURI, hints)
    catch {
      case err: XPathException =>
        grumble("Failed to resolve URI of imported module: " + err.getMessage, "XQST0059", mImport.offset)
    }
    if (sources == null) {
      if (hints.length == 0) grumble("Cannot locate module for namespace " + mImport.namespaceURI, "XQST0059", mImport.offset)
      resolver = env.getConfiguration.getStandardModuleURIResolver
      sources = resolver.resolve(mImport.namespaceURI, env.getStaticBaseURI, hints)
    }

    for (hint <- mImport.locationURIs.asScala) {
      executable.addQueryLocationHintProcessed(hint)
    }
    breakable {
      for (m <- 0 until sources.length) {
        val ss = sources(m)
        var baseURI = ss.getSystemId
        if (baseURI == null) if (m < hints.length) {
          baseURI = hints(m)
          ss.setSystemId(hints(m))
        }
        else grumble("No base URI available for imported module", "XQST0059", mImport.offset)


        existingModules = executable.getQueryLibraryModules(mImport.namespaceURI)
        var loaded = false
        if (existingModules != null && m < hints.length) {

          for (existingModule <- existingModules.asScala) {
            val uri = existingModule.location_URI
            if (uri != null && uri.toString == mImport.locationURIs.get(m)) {
              loaded = true
              break()
            }
          }
        }
        if (loaded) break()
        try {
          val queryText = QueryReader.readSourceQuery(ss, charChecker)
          try if (ss.getInputStream != null) ss.getInputStream.close()
          else if (ss.getReader != null) ss.getReader.close()
          catch {
            case e: IOException =>
              throw new XPathException("Failure while closing file for imported query module")
          }
          QueryModule.makeQueryModule(baseURI, executable, env.asInstanceOf[QueryModule], queryText, mImport.namespaceURI)
        } catch {
          case err: XPathException =>
            err.maybeSetLocation(makeLocation)
            reportError(err)
        }
      }
    }
  }


  @throws[XPathException]
  private def parseBaseURIDeclaration(): Unit = {
    if (foundBaseURIDeclaration) grumble("Base URI Declaration may only appear once", "XQST0032")
    foundBaseURIDeclaration = true
    nextToken()
    expect(Token.STRING_LITERAL)
    var uri = uriLiteral(t.currentTokenValue)
    try {
      val baseURI = new URI(uri)
      if (!baseURI.isAbsolute) {
        val oldBase = env.getStaticBaseURI
        uri = ResolveURI.makeAbsolute(uri, oldBase).toString
      }
      env.asInstanceOf[QueryModule].setBaseURI(uri)
    } catch {
      case err: URISyntaxException =>

        env.asInstanceOf[QueryModule].setBaseURI(uri)
    }
    nextToken()
  }


  @throws[XPathException]
  private def parseDecimalFormatDeclaration(): Unit = {
    nextToken()
    expect(Token.NAME)
    val formatName = makeStructuredQName(t.currentTokenValue, "")
    if (env.getDecimalFormatManager.getNamedDecimalFormat(formatName) != null) grumble("Duplicate declaration of decimal-format " + formatName.getDisplayName, "XQST0111")
    nextToken()
    parseDecimalFormatProperties(formatName)
  }


  @throws[XPathException]
  private def parseDefaultDecimalFormat(): Unit = {
    if (foundDefaultDecimalFormat) grumble("Duplicate declaration of default decimal-format", "XQST0111")
    foundDefaultDecimalFormat = true
    parseDecimalFormatProperties(null)
  }

  @throws[XPathException]
  private def parseDecimalFormatProperties(formatName: StructuredQName): Unit = {
    val outerOffset = t.currentTokenStartOffset
    val dfm = env.getDecimalFormatManager
    val dfs = if (formatName == null) dfm.getDefaultDecimalFormat
    else dfm.obtainNamedDecimalFormat(formatName)
    dfs.setHostLanguage(HostLanguage.XQUERY, 31)
    val propertyNames = new util.HashSet[String](10)
    while ( {
      t.currentToken != Token.SEMICOLON
    }) {
      val offset = t.currentTokenStartOffset
      val propertyName = t.currentTokenValue
      if (propertyNames.contains(propertyName)) grumble("Property name " + propertyName + " is defined more than once", "XQST0114", offset)
      nextToken()
      expect(Token.EQUALS)
      nextToken()
      expect(Token.STRING_LITERAL)
      val propertyValue = unescape(t.currentTokenValue).toString
      nextToken()
      propertyNames.add(propertyName)
      propertyName match {
        case "decimal-separator" =>
          dfs.setDecimalSeparator(propertyValue)
        case "grouping-separator" =>
          dfs.setGroupingSeparator(propertyValue)
        case "infinity" =>
          dfs.setInfinity(propertyValue)
        case "minus-sign" =>
          dfs.setMinusSign(propertyValue)
        case "NaN" =>
          dfs.setNaN(propertyValue)
        case "percent" =>
          dfs.setPercent(propertyValue)
        case "per-mille" =>
          dfs.setPerMille(propertyValue)
        case "zero-digit" =>
          try dfs.setZeroDigit(propertyValue)
          catch {
            case err: XPathException =>
              err.setErrorCode("XQST0097")
              throw err
          }
        case "digit" =>
          dfs.setDigit(propertyValue)
        case "pattern-separator" =>
          dfs.setPatternSeparator(propertyValue)
        case "exponent-separator" =>
          dfs.setExponentSeparator(propertyValue)
        case _ =>
          grumble("Unknown decimal-format property: " + propertyName, "XPST0003", offset)
      }
    }
    try dfs.checkConsistency(formatName)
    catch {
      case err: XPathException =>
        grumble(err.getMessage, "XQST0098", outerOffset)
    }
  }


  @throws[XPathException]
  private def parseDefaultFunctionNamespace(): Unit = {
    if (foundDefaultFunctionNamespace) grumble("default function namespace appears more than once", "XQST0066")
    foundDefaultFunctionNamespace = true
    nextToken()
    expect(Token.NAME)
    if (!("namespace" == t.currentTokenValue)) grumble("After 'declare default function', expected 'namespace'")
    nextToken()
    expect(Token.STRING_LITERAL)
    val uri = uriLiteral(t.currentTokenValue)
    if (uri == NamespaceConstant.XML || uri == NamespaceConstant.XMLNS) grumble("Reserved namespace used as default element/type namespace", "XQST0070")
    env.asInstanceOf[QueryModule].setDefaultFunctionNamespace(uri)
    nextToken()
  }


  @throws[XPathException]
  private def parseDefaultElementNamespace(): Unit = {
    if (foundDefaultElementNamespace)
      grumble("default element namespace appears more than once", "XQST0066")
    foundDefaultElementNamespace = true
    nextToken()
    expect(Token.NAME)
    if (!("namespace" == t.currentTokenValue)) grumble("After 'declare default element', expected 'namespace'")
    nextToken()
    expect(Token.STRING_LITERAL)
    val uri = uriLiteral(t.currentTokenValue)
    if (uri == NamespaceConstant.XML || uri == NamespaceConstant.XMLNS)
      grumble("Reserved namespace used as default element/type namespace", "XQST0070")
    env.asInstanceOf[QueryModule].setDefaultElementNamespace(uri)
    nextToken()
  }


  @throws[XPathException]
  private def parseNamespaceDeclaration(): Unit = {
    nextToken()
    expect(Token.NAME)
    val prefix = t.currentTokenValue
    if (!NameChecker.isValidNCName(prefix)) grumble("Invalid namespace prefix " + Err.wrap(prefix))
    nextToken()
    expect(Token.EQUALS)
    nextToken()
    expect(Token.STRING_LITERAL)
    val uri = uriLiteral(t.currentTokenValue)
    checkProhibitedPrefixes(prefix, uri)
    if ("xml" == prefix) {
      grumble("Namespace prefix 'xml' cannot be declared", "XQST0070")
    }
    try env.asInstanceOf[QueryModule].declarePrologNamespace(prefix, uri)
    catch {
      case err: XPathException =>
        err.setLocator(makeLocation)
        reportError(err)
    }
    nextToken()
  }


  @throws[XPathException]
  private def checkProhibitedPrefixes(prefix: String, uri: String): Unit = {
    var prefixStr = prefix
    var uriStr = uri
    if (prefixStr != null && !prefixStr.isEmpty && !NameChecker.isValidNCName(prefixStr)) grumble("The namespace prefix " + Err.wrap(prefixStr) + " is not a valid NCName")
    if (prefixStr == null) prefixStr = ""
    if (uriStr == null) uriStr = ""
    if ("xmlns" == prefixStr) grumble("The namespace prefix 'xmlns' cannot be redeclared", "XQST0070")
    if (uriStr == NamespaceConstant.XMLNS) grumble("The xmlns namespace URI is reserved", "XQST0070")
    if (uriStr == NamespaceConstant.XML && !(prefixStr == "xml"))
      grumble("The XML namespace cannot be bound to any prefix other than 'xml'", "XQST0070")
    if (prefixStr == "xml" && !(uriStr == NamespaceConstant.XML))
      grumble("The prefix 'xml' cannot be bound to any namespace other than " + NamespaceConstant.XML, "XQST0070")
  }


  @throws[XPathException]
  private def parseVariableDeclaration(annotations: AnnotationList): Unit = {
    val offset = t.currentTokenStartOffset
    var `var` = new GlobalVariable
    `var`.setPackageData(env.getPackageData)
    `var`.setLineNumber(t.getLineNumber + 1)
    `var`.setSystemId(env.getSystemId)
    if (annotations != null) `var`.setPrivate(annotations.includes(Annotation.PRIVATE))
    nextToken()
    expect(Token.DOLLAR)
    t.setState(Tokenizer.BARE_NAME_STATE)
    nextToken()
    expect(Token.NAME)
    val varName = t.currentTokenValue
    val varQName = makeStructuredQName(t.currentTokenValue, "")
    assert(varQName != null)
    `var`.setVariableQName(varQName)
    val uri = varQName.getURI
    val moduleURI = env.asInstanceOf[QueryModule].getModuleNamespace
    if (moduleURI != null && !(moduleURI == uri)) grumble("A variable declared in a library module must be in the module namespace", "XQST0048", offset)
    nextToken()
    var requiredType = SequenceType.ANY_SEQUENCE
    if (t.currentToken == Token.AS) {
      t.setState(Tokenizer.SEQUENCE_TYPE_STATE)
      nextToken()
      requiredType = parseSequenceType
    }
    `var`.setRequiredType(requiredType)
    if (t.currentToken == Token.ASSIGN) {
      t.setState(Tokenizer.DEFAULT_STATE)
      nextToken()
      val exp = parseExprSingle
      `var`.setBody(makeTracer(exp, varQName))
    }
    else if (t.currentToken == Token.NAME) if ("external" == t.currentTokenValue) {
      val par = new GlobalParam
      par.setPackageData(env.getPackageData)

      par.setLineNumber(`var`.getLineNumber)
      par.setSystemId(`var`.getSystemId)
      par.setVariableQName(`var`.getVariableQName)
      par.setRequiredType(`var`.getRequiredType)
      `var` = par
      nextToken()
      if (t.currentToken == Token.ASSIGN) {
        t.setState(Tokenizer.DEFAULT_STATE)
        nextToken()
        val exp = parseExprSingle
        `var`.setBody(makeTracer(exp, varQName))
      }
    }
    else grumble("Variable must either be initialized or be declared as external")
    else grumble("Expected ':=' or 'external' in variable declaration")
    val qenv = env.asInstanceOf[QueryModule]
    val rsc = env.makeRetainedStaticContext
    `var`.setRetainedStaticContext(rsc)
    if (`var`.getBody != null) ExpressionTool.setDeepRetainedStaticContext(`var`.getBody, rsc)
    if (qenv.getModuleNamespace != null && !(uri == qenv.getModuleNamespace)) grumble("Variable " + Err.wrap(varName, Err.VARIABLE) + " is not defined in the module namespace")
    try qenv.declareVariable(`var`)
    catch {
      case e: XPathException =>
        grumble(e.getMessage, e.getErrorCodeQName, -1)
    }
  }


  @throws[XPathException]
  private def parseContextItemDeclaration(): Unit = {
    val offset = t.currentTokenStartOffset
    nextToken()
    if (!isKeyword("item")) grumble("After 'declare context', expected 'item'")
    if (foundContextItemDeclaration) grumble("More than one context item declaration found", "XQST0099", offset)
    foundContextItemDeclaration = true
    val req = new GlobalContextRequirement
    req.setAbsentFocus(false)
    t.setState(Tokenizer.BARE_NAME_STATE)
    nextToken()
    var requiredType: ItemType = AnyItemType
    if (t.currentToken == Token.AS) {
      t.setState(Tokenizer.SEQUENCE_TYPE_STATE)
      nextToken()
      requiredType = parseItemType
    }
    req.addRequiredItemType(requiredType)
    if (t.currentToken == Token.ASSIGN) {
      if (!(env.asInstanceOf[QueryModule]).isMainModule) grumble("The context item must not be initialized in a library module", "XQST0113")
      t.setState(Tokenizer.DEFAULT_STATE)
      nextToken()
      var exp = parseExprSingle
      exp.setRetainedStaticContext(env.makeRetainedStaticContext)
      val role = new RoleDiagnostic(RoleDiagnostic.CONTEXT_ITEM, "context item declaration", 0)
      exp = CardinalityChecker.makeCardinalityChecker(exp, StaticProperty.EXACTLY_ONE, role)
      val visitor = ExpressionVisitor.make(env)
      exp = exp.simplify
      val info = env.getConfiguration.makeContextItemStaticInfo(AnyItemType, maybeUndefined = true)
      exp.setRetainedStaticContext(env.makeRetainedStaticContext)
      exp = exp.typeCheck(visitor, info)
      req.setDefaultValue(exp)
      req.setExternal(false)
    }
    else if (t.currentToken == Token.NAME && "external" == t.currentTokenValue) {
      req.setAbsentFocus(false)
      req.setExternal(true)
      nextToken()
      if (t.currentToken == Token.ASSIGN) {
        if (!(env.asInstanceOf[QueryModule]).isMainModule) grumble("The context item must not be initialized in a library module", "XQST0113")
        t.setState(Tokenizer.DEFAULT_STATE)
        nextToken()
        var exp = parseExprSingle
        val role = new RoleDiagnostic(RoleDiagnostic.CONTEXT_ITEM, "context item declaration", 0)
        exp = CardinalityChecker.makeCardinalityChecker(exp, StaticProperty.EXACTLY_ONE, role)
        exp.setRetainedStaticContext(env.makeRetainedStaticContext)
        req.setDefaultValue(exp)
      }
    }
    else grumble("Expected ':=' or 'external' in context item declaration")
    val exec = getExecutable
    if (exec.getGlobalContextRequirement != null) {
      val gcr = exec.getGlobalContextRequirement
      if (gcr.getDefaultValue == null && req.getDefaultValue != null) gcr.setDefaultValue(req.getDefaultValue)

      for (otherType <- gcr.getRequiredItemTypes.asScala) {
        if (otherType ne AnyItemType) {
          val th = env.getConfiguration.getTypeHierarchy
          val rel = th.relationship(requiredType, otherType)
          if (rel eq Affinity.DISJOINT) {
            grumble("Different modules specify incompatible requirements for the type of the initial context item", "XPTY0004")
          }
        }
      }
      gcr.addRequiredItemType(requiredType)
    }
    else exec.setGlobalContextRequirement(req)
  }


  @throws[XPathException]
  def parseFunctionDeclaration(annotations: AnnotationList): Unit = {

    if (annotations.includes(XQueryParser.SAXON_MEMO_FUNCTION)) if (env.getConfiguration.getEditionCode == "HE")
      warning("saxon:memo-function option is ignored under Saxon-HE")
    else memoFunction = true

    val offset = t.currentTokenStartOffset
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    expect(Token.FUNCTION)
    var uri: String = null
    var qName: StructuredQName = null
    if (t.currentTokenValue.indexOf(':') < 0) {
      uri = env.getDefaultFunctionNamespace
      qName = new StructuredQName("", uri, t.currentTokenValue)
    }
    else {
      qName = makeStructuredQName(t.currentTokenValue, "")
      uri = qName.getURI
    }
    if (uri.isEmpty) grumble("The function must be in a namespace", "XQST0060")
    val moduleURI = env.asInstanceOf[QueryModule].getModuleNamespace
    if (moduleURI != null && !(moduleURI == uri)) grumble("A function in a library module must be in the module namespace", "XQST0048")
    if (isReservedInQuery(uri)) grumble("The function name " + t.currentTokenValue + " is in a reserved namespace", "XQST0045")
    val func = new XQueryFunction
    func.setFunctionName(qName)
    func.setResultType(SequenceType.ANY_SEQUENCE)
    func.setBody(null)
    val loc = makeNestedLocation(env.getContainingLocation, t.getLineNumber(offset), t.getColumnNumber(offset), null)
    func.setLocation(loc)
    func.setStaticContext(env.asInstanceOf[QueryModule])
    func.setMemoFunction(memoFunction)
    func.setUpdating(annotations.includes(Annotation.UPDATING))
    func.setAnnotations(annotations)
    nextToken()
    val paramNames = new util.HashSet[StructuredQName](8)
    var external = false
    if (t.currentToken != Token.RPAR)
      breakable {
        while ( {
          true
        }) {

          expect(Token.DOLLAR)
          nextToken()
          expect(Token.NAME)
          val argQName = makeStructuredQName(t.currentTokenValue, "")
          if (paramNames.contains(argQName)) grumble("Duplicate parameter name " + Err.wrap(t.currentTokenValue, Err.VARIABLE), "XQST0039")
          paramNames.add(argQName)
          var paramType = SequenceType.ANY_SEQUENCE
          nextToken()
          if (t.currentToken == Token.AS) {
            nextToken()
            paramType = parseSequenceType
          }
          val arg = new UserFunctionParameter
          arg.setRequiredType(paramType)
          arg.setVariableQName(argQName)
          func.addArgument(arg)
          declareRangeVariable(arg)
          if (t.currentToken == Token.RPAR) break()
          else if (t.currentToken == Token.COMMA) nextToken()
          else grumble("Expected ',' or ')' after function argument, found '" + Token.tokens(t.currentToken) + '\'')
        }
      }
    t.setState(Tokenizer.BARE_NAME_STATE)
    nextToken()
    if (t.currentToken == Token.AS) {
      if (func.isUpdating) grumble("Cannot specify a return type for an updating function", "XUST0028")
      t.setState(Tokenizer.SEQUENCE_TYPE_STATE)
      nextToken()
      func.setResultType(parseSequenceType)
    }
    if (isKeyword("external")) external = true
    else {
      expect(Token.LCURLY)
      t.setState(Tokenizer.DEFAULT_STATE)
      nextToken()
      if (t.currentToken == Token.RCURLY) {
        val body = Literal.makeEmptySequence
        body.setRetainedStaticContext(env.makeRetainedStaticContext)
        setLocation(body)
        func.setBody(body)
      }
      else {
        val body = parseExpression
        func.setBody(body)
        ExpressionTool.setDeepRetainedStaticContext(body, env.makeRetainedStaticContext)
      }
      expect(Token.RCURLY)
      lookAhead()
    }
    val params = func.getParameterDefinitions

    for (param <- params) {
      undeclareRangeVariable()
    }
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    val qenv = env.asInstanceOf[QueryModule]
    if (external) parserExtension.handleExternalFunctionDeclaration(this, func)
    else try qenv.declareFunction(func)
    catch {
      case e: XPathException =>
        grumble(e.getMessage, e.getErrorCodeQName, -1)
    }
    memoFunction = false
  }


  @throws[XPathException]
   def parseTypeAliasDeclaration(): Unit = parserExtension.parseTypeAliasDeclaration(this)


  @throws[XPathException]
  private def parseOptionDeclaration(): Unit = {
    nextToken()
    expect(Token.NAME)
    val defaultUri = NamespaceConstant.XQUERY
    val varName = makeStructuredQName(t.currentTokenValue, defaultUri)
    assert(varName != null)
    val uri = varName.getURI
    if (uri.isEmpty) {
      grumble("The QName identifying an option declaration must be prefixed", "XPST0081")
      return
    }
    nextToken()
    expect(Token.STRING_LITERAL)

    var value = unescape(t.currentTokenValue).toString
    if (uri == NamespaceConstant.OUTPUT) parseOutputDeclaration(varName, value)
    else if (uri == NamespaceConstant.SAXON) {
      val localName = varName.getLocalPart
      localName match {
        case "output" =>
          setOutputProperty(value)
        case "memo-function" =>
          value = value.trim
          value match {
            case "true" =>
              memoFunction = true
              if (env.getConfiguration.getEditionCode == "HE") warning("saxon:memo-function option is ignored under Saxon-HE")
            case "false" =>
              memoFunction = false
            case _ =>
              warning("Value of saxon:memo-function must be 'true' or 'false'")
          }
        case "allow-cycles" =>
          warning("Value of saxon:allow-cycles is ignored")
        case _ =>
          warning("Unknown Saxon option declaration: " + varName.getDisplayName)
      }
    }
    nextToken()
  }

  @throws[XPathException]
   def parseOutputDeclaration(varName: StructuredQName, value: String) = {
    if (!(env.asInstanceOf[QueryModule]).isMainModule) grumble("Output declarations must not appear in a library module", "XQST0108")
    val localName = varName.getLocalPart
    if (outputPropertiesSeen.contains(varName)) grumble("Duplicate output declaration (" + varName + ")", "XQST0110")
    outputPropertiesSeen.add(varName)
    localName match {
      case "parameter-document" =>
        var source: Source = null
        try source = env.getConfiguration.getURIResolver.resolve(value, env.getStaticBaseURI)
        catch {
          case e: TransformerException =>
            throw XPathException.makeXPathException(e)
        }
        val options = new ParseOptions
        options.setSchemaValidationMode(Validation.LAX)
        options.setDTDValidationMode(Validation.SKIP)
        val doc = env.getConfiguration.buildDocumentTree(source)
        val ph = new SerializationParamsHandler(parameterDocProperties)
        ph.setSerializationParams(doc.getRootNode)
        val characterMap = ph.getCharacterMap
        if (characterMap != null) {
          val index = new CharacterMapIndex
          index.putCharacterMap(characterMap.getName, characterMap)
          getExecutable.setCharacterMapIndex(index)
          parameterDocProperties.setProperty(SaxonOutputKeys.USE_CHARACTER_MAPS, characterMap.getName.getClarkName)
        }
      case "use-character-maps" =>
        grumble("Output declaration use-character-maps cannot appear except in a parameter file", "XQST0109")
      case _ =>
        val props = getExecutable.getPrimarySerializationProperties.getProperties
        ResultDocument.setSerializationProperty(props, "", localName, value, env.getNamespaceResolver, prevalidated = false, env.getConfiguration)
    }
  }


  private def setOutputProperty(property: String) = {
    val equals = property.indexOf("=")
    if (equals < 0) badOutputProperty("no equals sign")
    else if (equals == 0) badOutputProperty("starts with '=")
    val keyword = Whitespace.trim(property.substring(0, equals))
    val value = if (equals == property.length - 1) ""
    else Whitespace.trim(property.substring(equals + 1))
    val props = getExecutable.getPrimarySerializationProperties.getProperties
    try {
      val name = makeStructuredQName(keyword, "")
      val lname = name.getLocalPart
      val uri = name.getURI
      ResultDocument.setSerializationProperty(props, uri, lname, value, env.getNamespaceResolver, prevalidated = false, env.getConfiguration)
    } catch {
      case e: XPathException =>
        badOutputProperty(e.getMessage)
    }
  }

  private def badOutputProperty(s: String) = warning("Invalid serialization property (" + s + ")")


  @throws[XPathException]
  override  def parseFLWORExpression: Expression = {

    val flwor = new FLWORExpression
    val exprOffset = t.currentTokenStartOffset
    val clauseList = new util.ArrayList[Clause](4)
    breakable {
      while (true) {
        val offset = t.currentTokenStartOffset
        if (t.currentToken == Token.FOR) parseForClause(flwor, clauseList)
        else if (t.currentToken == Token.LET) parseLetClause(flwor, clauseList)
        else if (t.currentToken == Token.COUNT) parseCountClause(clauseList)
        else if (t.currentToken == Token.GROUP_BY) parseGroupByClause(flwor, clauseList)
        else if (t.currentToken == Token.FOR_TUMBLING || t.currentToken == Token.FOR_SLIDING) parseWindowClause(flwor, clauseList)
        else if (t.currentToken == Token.WHERE || isKeyword("where")) {
          nextToken()
          val condition = parseExprSingle
          val clause = new WhereClause(flwor, condition)
          clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
          clauseList.add(clause)
        }
        else if (isKeyword("stable") || isKeyword("order")) {
          if (isKeyword("stable")) {
            nextToken()
            if (!isKeyword("order")) grumble("'stable' must be followed by 'order by'")
          }
          val tupleExpression = new TupleExpression
          val vars = new util.ArrayList[LocalVariableReference]

          for (c <- clauseList.asScala) {
            for (b <- c.getRangeVariables) {
              vars.add(new LocalVariableReference(b))
            }
          }
          tupleExpression.setVariables(vars)
          var sortSpecList: util.List[SortSpec] = null
          t.setState(Tokenizer.BARE_NAME_STATE)
          nextToken()
          if (!isKeyword("by")) grumble("'order' must be followed by 'by'")
          t.setState(Tokenizer.DEFAULT_STATE)
          nextToken()
          sortSpecList = parseSortDefinition
          val keys = new Array[SortKeyDefinition](sortSpecList.size)
          for (i <- 0 until keys.length) {
            val spec = sortSpecList.get(i)
            val key = new SortKeyDefinition
            key.setSortKey(sortSpecList.get(i).sortKey, setContext = false)
            key.setOrder(new StringLiteral(if (spec.ascending) "ascending"
            else "descending"))
            key.setEmptyLeast(spec.emptyLeast)
            if (spec.collation != null) {
              val comparator = env.getConfiguration.getCollation(spec.collation)
              if (comparator == null) grumble("Unknown collation '" + spec.collation + '\'', "XQST0076")
              key.setCollation(comparator)
            }
            keys(i) = key
          }
          val clause = new OrderByClause(flwor, keys, tupleExpression)
          clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
          clauseList.add(clause)
        }
        else break()
        setLocation(clauseList.get(clauseList.size - 1), offset)
      }
    }
    val returnOffset = t.currentTokenStartOffset
    expect(Token.RETURN)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    var returnExpression = parseExprSingle
    returnExpression = makeTracer(returnExpression, null)

    for (i <- clauseList.size - 1 to 0 by -1) {
      val clause = clauseList.get(i)
      for (n <- 0 until clause.getRangeVariables.length) {
        undeclareRangeVariable()
      }
    }


    flwor.init(clauseList, returnExpression)
    setLocation(flwor, exprOffset)
    flwor
  }


   def makeLetExpression: LetExpression = if (env.asInstanceOf[QueryModule].getUserQueryContext.isCompileWithTracing) new EagerLetExpression
  else new LetExpression


  @throws[XPathException]
  private def parseForClause(flwor: FLWORExpression, clauseList: util.List[Clause]): Unit = {
    var first = true
    do {
      val clause = new ForClause
      clause.setRepeated(!first || XQueryParser.containsLoopingClause(clauseList))
      setLocation(clause, t.currentTokenStartOffset)
      if (first) {

      }
      clauseList.add(clause)
      nextToken()
      if (first) first = false
      else {
      }
      expect(Token.DOLLAR)
      nextToken()
      expect(Token.NAME)
      val varQName = makeStructuredQName(t.currentTokenValue, "")
      var `type` = SequenceType.SINGLE_ITEM
      nextToken()
      var explicitType = false
      if (t.currentToken == Token.AS) {
        explicitType = true
        nextToken()
        `type` = parseSequenceType
      }
      var allowingEmpty = false
      if (isKeyword("allowing")) {
        allowingEmpty = true
        clause.setAllowingEmpty(true)
        if (!explicitType) `type` = SequenceType.OPTIONAL_ITEM
        nextToken()
        if (!isKeyword("empty")) grumble("After 'allowing', expected 'empty'")
        nextToken()
      }
      if (explicitType && !allowingEmpty && `type`.getCardinality != StaticProperty.EXACTLY_ONE) {
        warning("Occurrence indicator on singleton range variable has no effect")
        `type` = SequenceType.makeSequenceType(`type`.getPrimaryType, StaticProperty.EXACTLY_ONE)
      }
      val binding = new LocalVariableBinding(varQName, `type`)
      clause.setRangeVariable(binding)
      if (isKeyword("at")) {
        nextToken()
        expect(Token.DOLLAR)
        nextToken()
        expect(Token.NAME)
        val posQName = makeStructuredQName(t.currentTokenValue, "")
        if (!scanOnly && posQName == varQName) grumble("The two variables declared in a single 'for' clause must have different names", "XQST0089")
        val pos = new LocalVariableBinding(posQName, SequenceType.SINGLE_INTEGER)
        clause.setPositionVariable(pos)
        nextToken()
      }
      expect(Token.IN)
      nextToken()
      clause.initSequence(flwor, parseExprSingle)
      declareRangeVariable(clause.getRangeVariable)
      if (clause.getPositionVariable != null) declareRangeVariable(clause.getPositionVariable)
      if (allowingEmpty) checkForClauseAllowingEmpty(flwor, clause)
    } while ( {
      t.currentToken == Token.COMMA
    })
  }


  @throws[XPathException]
  private def checkForClauseAllowingEmpty(flwor: FLWORExpression, clause: ForClause): Unit = {
    if (!allowXPath30Syntax) grumble("The 'allowing empty' option requires XQuery 3.0")
    val `type` = clause.getRangeVariable.getRequiredType
    if (!Cardinality.allowsZero(`type`.getCardinality)) warning("When 'allowing empty' is specified, the occurrence indicator on the range variable type should be '?'")
  }


  @throws[XPathException]
  private def parseLetClause(flwor: FLWORExpression, clauseList: util.List[Clause]) = {
    var first: Boolean = true
    do {
      val clause = new LetClause
      setLocation(clause, t.currentTokenStartOffset)
      clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
      if (first) {
      }
      clauseList.add(clause)
      nextToken()
      if (first) first = false
      else {
      }
      expect(Token.DOLLAR)
      nextToken()
      expect(Token.NAME)
      val `var` = t.currentTokenValue
      val varQName = makeStructuredQName(`var`, "")
      var `type` = SequenceType.ANY_SEQUENCE
      nextToken()
      if (t.currentToken == Token.AS) {
        nextToken()
        `type` = parseSequenceType
      }
      val v = new LocalVariableBinding(varQName, `type`)
      expect(Token.ASSIGN)
      nextToken()
      clause.initSequence(flwor, parseExprSingle)
      clause.setRangeVariable(v)
      declareRangeVariable(v)
    } while ( {
      t.currentToken == Token.COMMA
    })
  }


  @throws[XPathException]
  private def parseCountClause(clauseList: util.List[Clause]) = do {
    val clause = new CountClause
    setLocation(clause, t.currentTokenStartOffset)
    clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
    clauseList.add(clause)
    nextToken()
    expect(Token.DOLLAR)
    nextToken()
    expect(Token.NAME)
    val `var` = t.currentTokenValue
    val varQName = makeStructuredQName(`var`, "")
    val `type` = SequenceType.ANY_SEQUENCE
    nextToken()
    val v = new LocalVariableBinding(varQName, `type`)
    clause.setRangeVariable(v)
    declareRangeVariable(v)
  } while ( {
    t.currentToken == Token.COMMA
  })


  @throws[XPathException]
  private def parseGroupByClause(flwor: FLWORExpression, clauseList: util.List[Clause]): Unit = {

    val clause = new GroupByClause(env.getConfiguration)
    setLocation(clause, t.currentTokenStartOffset)
    clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
    val variableNames = new util.ArrayList[StructuredQName]
    val collations = new util.ArrayList[String]
    nextToken()
    breakable {
      while (true) {
        var `type` = SequenceType.ANY_SEQUENCE
        val varQName = readVariableName
        if (t.currentToken == Token.AS) {
          nextToken()
          `type` = parseSequenceType
          if (t.currentToken != Token.ASSIGN) grumble("In group by, if the type is declared then it must be followed by ':= value'")
        }
        if (t.currentToken == Token.ASSIGN) {
          val letClause = new LetClause
          clauseList.add(letClause)
          nextToken()
          val v = new LocalVariableBinding(varQName, `type`)
          val value = parseExprSingle
          val role = new RoleDiagnostic(RoleDiagnostic.MISC, "grouping key", 0)
          val atomizedValue = Atomizer.makeAtomizer(value, role)
          letClause.initSequence(flwor, atomizedValue)
          letClause.setRangeVariable(v)
          declareRangeVariable(v)
        }
        variableNames.add(varQName)
        if (isKeyword("collation")) {
          nextToken()
          expect(Token.STRING_LITERAL)
          collations.add(t.currentTokenValue)
          nextToken()
        }
        else collations.add(env.getDefaultCollationName)
        if (t.currentToken == Token.COMMA) nextToken()
        else break()
      }
    }

    val groupingTupleExpr = new TupleExpression
    val retainedTupleExpr = new TupleExpression
    val groupingRefs = new util.ArrayList[LocalVariableReference]
    val retainedRefs = new util.ArrayList[LocalVariableReference]
    val groupedBindings = new util.ArrayList[LocalVariableBinding]

    for (q <- variableNames.asScala) {
      var found = false
      val search = new Breaks
      search.breakable {
        for (i <- clauseList.size - 1 to 0 by -1) {
          for (b <- clauseList.get(i).getRangeVariables) {
            if (q == b.getVariableQName) {
              groupedBindings.add(b)
              groupingRefs.add(new LocalVariableReference(b))
              found = true
              search.break()
            }
          }
        }
      }
      if (!found) grumble("The grouping variable " + q.getDisplayName + " must be the name of a variable bound earlier in the FLWOR expression", "XQST0094")
    }
    groupingTupleExpr.setVariables(groupingRefs)
    clause.initGroupingTupleExpression(flwor, groupingTupleExpr)
    val ungroupedBindings = new util.ArrayList[LocalVariableBinding]
    for (i <- clauseList.size - 1 to 0 by -1) {
      for (b <- clauseList.get(i).getRangeVariables) {
        if (!groupedBindings.contains(b)) {
          ungroupedBindings.add(b)
          retainedRefs.add(new LocalVariableReference(b))
        }
      }
    }
    retainedTupleExpr.setVariables(retainedRefs)
    clause.initRetainedTupleExpression(flwor, retainedTupleExpr)
    val bindings = new Array[LocalVariableBinding](groupedBindings.size + ungroupedBindings.size)
    var k = 0

    for (b <- groupedBindings.asScala) {
      bindings(k) = new LocalVariableBinding(b.getVariableQName, b.getRequiredType)

      k += 1
    }

    for (b <- ungroupedBindings.asScala) {
      val itemType = b.getRequiredType.getPrimaryType
      bindings(k) = new LocalVariableBinding(b.getVariableQName, SequenceType.makeSequenceType(itemType, StaticProperty.ALLOWS_ZERO_OR_MORE))
      k += 1
    }
    for (z <- groupedBindings.size until bindings.length) {
      declareRangeVariable(bindings(z))
    }
    for (z <- 0 until groupedBindings.size) {
      declareRangeVariable(bindings(z))
    }
    clause.setVariableBindings(bindings)
    val comparers = new Array[GenericAtomicComparer](collations.size)
    val context = env.makeEarlyEvaluationContext
    for (i <- 0 until comparers.length) {
      val coll = env.getConfiguration.getCollation(collations.get(i))
      comparers(i) = GenericAtomicComparer.makeAtomicComparer(BuiltInAtomicType.ANY_ATOMIC, BuiltInAtomicType.ANY_ATOMIC, coll, context).asInstanceOf[GenericAtomicComparer]
    }
    clause.setComparers(comparers)
    clauseList.add(clause)
  }

  @throws[XPathException]
  private def readVariableName: StructuredQName = {
    expect(Token.DOLLAR)
    nextToken()
    expect(Token.NAME)
    val name = t.currentTokenValue
    nextToken()
    makeStructuredQName(name, "")
  }


  @throws[XPathException]
  private def parseWindowClause(flwor: FLWORExpression, clauseList: util.List[Clause]): Unit = {
    val clause = new WindowClause
    setLocation(clause, t.currentTokenStartOffset)
    clause.setRepeated(XQueryParser.containsLoopingClause(clauseList))
    clause.setIsSlidingWindow(t.currentToken == Token.FOR_SLIDING)
    nextToken()
    if (!isKeyword("window")) grumble("after 'sliding' or 'tumbling', expected 'window', but found " + currentTokenDisplay)
    nextToken()
    val windowVarName = readVariableName
    var windowType = SequenceType.ANY_SEQUENCE
    if (t.currentToken == Token.AS) {
      nextToken()
      windowType = parseSequenceType
    }
    val windowVar = new LocalVariableBinding(windowVarName, windowType)
    clause.setVariableBinding(WindowClause.WINDOW_VAR, windowVar)

    val windowItemTypeMandatory = SequenceType.SINGLE_ITEM
    val windowItemTypeOptional = SequenceType.OPTIONAL_ITEM
    expect(Token.IN)
    nextToken()
    clause.initSequence(flwor, parseExprSingle)
    if (!isKeyword("start")) grumble("in window clause, expected 'start', but found " + currentTokenDisplay)
    t.setState(Tokenizer.BARE_NAME_STATE)
    nextToken()
    if (t.currentToken == Token.DOLLAR) {
      val startItemVar = new LocalVariableBinding(readVariableName, windowItemTypeMandatory)
      clause.setVariableBinding(WindowClause.START_ITEM, startItemVar)
      declareRangeVariable(startItemVar)
    }
    if (isKeyword("at")) {
      nextToken()
      val startPositionVar = new LocalVariableBinding(readVariableName, SequenceType.SINGLE_INTEGER)
      clause.setVariableBinding(WindowClause.START_ITEM_POSITION, startPositionVar)
      declareRangeVariable(startPositionVar)
    }
    if (isKeyword("previous")) {
      nextToken()
      val startPreviousItemVar = new LocalVariableBinding(readVariableName, windowItemTypeOptional)
      clause.setVariableBinding(WindowClause.START_PREVIOUS_ITEM, startPreviousItemVar)
      declareRangeVariable(startPreviousItemVar)
    }
    if (isKeyword("next")) {
      nextToken()
      val startNextItemVar = new LocalVariableBinding(readVariableName, windowItemTypeOptional)
      clause.setVariableBinding(WindowClause.START_NEXT_ITEM, startNextItemVar)
      declareRangeVariable(startNextItemVar)
    }
    if (!isKeyword("when")) grumble("Expected 'when' condition for window start, but found " + currentTokenDisplay)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    clause.initStartCondition(flwor, parseExprSingle)
    if (isKeyword("only")) {
      clause.setIncludeUnclosedWindows(false)
      nextToken()
    }
    if (isKeyword("end")) {
      t.setState(Tokenizer.BARE_NAME_STATE)
      nextToken()
      if (t.currentToken == Token.DOLLAR) {
        val endItemVar = new LocalVariableBinding(readVariableName, windowItemTypeMandatory)
        clause.setVariableBinding(WindowClause.END_ITEM, endItemVar)
        declareRangeVariable(endItemVar)
      }
      if (isKeyword("at")) {
        nextToken()
        val endPositionVar = new LocalVariableBinding(readVariableName, SequenceType.SINGLE_INTEGER)
        clause.setVariableBinding(WindowClause.END_ITEM_POSITION, endPositionVar)
        declareRangeVariable(endPositionVar)
      }
      if (isKeyword("previous")) {
        nextToken()
        val endPreviousItemVar = new LocalVariableBinding(readVariableName, windowItemTypeOptional)
        clause.setVariableBinding(WindowClause.END_PREVIOUS_ITEM, endPreviousItemVar)
        declareRangeVariable(endPreviousItemVar)
      }
      if (isKeyword("next")) {
        nextToken()
        val endNextItemVar = new LocalVariableBinding(readVariableName, windowItemTypeOptional)
        clause.setVariableBinding(WindowClause.END_NEXT_ITEM, endNextItemVar)
        declareRangeVariable(endNextItemVar)
      }
      if (!isKeyword("when")) grumble("Expected 'when' condition for window end, but found " + currentTokenDisplay)
      t.setState(Tokenizer.DEFAULT_STATE)
      nextToken()
      clause.initEndCondition(flwor, parseExprSingle)
    }
    else {
      if (clause.isSlidingWindow) grumble("A sliding window requires an end condition")
    }
    declareRangeVariable(windowVar)
    clauseList.add(clause)
  }


  @throws[XPathException]
  private def parseSortDefinition: util.List[SortSpec] = {

    val sortSpecList = new util.ArrayList[XQueryParser.SortSpec](5)
    breakable {
      while (true) {
        val sortSpec = new XQueryParser.SortSpec
        sortSpec.sortKey = parseExprSingle
        sortSpec.ascending = true
        sortSpec.emptyLeast = env.asInstanceOf[QueryModule].isEmptyLeast
        sortSpec.collation = env.getDefaultCollationName

        if (isKeyword("ascending")) nextToken()
        else if (isKeyword("descending")) {
          sortSpec.ascending = false
          nextToken()
        }
        if (isKeyword("empty")) {
          nextToken()
          if (isKeyword("greatest")) {
            sortSpec.emptyLeast = false
            nextToken()
          }
          else if (isKeyword("least")) {
            sortSpec.emptyLeast = true
            nextToken()
          }
          else grumble("'empty' must be followed by 'greatest' or 'least'")
        }
        if (isKeyword("collation")) sortSpec.collation = readCollationName
        sortSpecList.add(sortSpec)
        if (t.currentToken == Token.COMMA) nextToken()
        else break()
      }
    }
    sortSpecList
  }

  @throws[XPathException]
   def readCollationName: String = {
    nextToken()
    expect(Token.STRING_LITERAL)
    var collationName = uriLiteral(t.currentTokenValue)
    var collationURI: URI = null
    try {
      collationURI = new URI(collationName)
      if (!collationURI.isAbsolute) {
        val base = new URI(env.getStaticBaseURI)
        collationURI = base.resolve(collationURI)
        collationName = collationURI.toString
      }
    } catch {
      case _: URISyntaxException =>
        grumble("Collation name '" + collationName + "' is not a valid URI", "XQST0046")
        collationName = NamespaceConstant.CODEPOINT_COLLATION_URI
    }
    nextToken()
    collationName
  }


  @throws[XPathException]
  override def parseTypeswitchExpression: Expression = {
    val offset = t.currentTokenStartOffset
    nextToken()
    val operand = parseExpression
    val types = new util.ArrayList[util.List[SequenceType]](10)
    val actions = new util.ArrayList[Expression](10)
    expect(Token.RPAR)
    nextToken()


    val outerLet = makeLetExpression
    outerLet.setRequiredType(SequenceType.ANY_SEQUENCE)
    outerLet.setVariableQName(new StructuredQName("zz", NamespaceConstant.SAXON, "zz_typeswitchVar"))
    outerLet.setSequence(operand)
    while ( {
      t.currentToken == Token.CASE
    }) {
      var typeList: util.List[SequenceType] = null
      var action: Expression = null
      nextToken()
      if (t.currentToken == Token.DOLLAR) {
        nextToken()
        expect(Token.NAME)
        val `var` = t.currentTokenValue
        val varQName = makeStructuredQName(`var`, "")
        nextToken()
        expect(Token.AS)
        nextToken()
        typeList = parseSequenceTypeList
        action = makeTracer(parseTypeswitchReturnClause(varQName, outerLet), varQName)
        if (action.isInstanceOf[TraceExpression]) action.asInstanceOf[TraceExpression].setProperty("type", typeList.get(0).toString)
      }
      else {
        typeList = parseSequenceTypeList
        action = makeTracer(parseExprSingle, null)
        if (action.isInstanceOf[TraceExpression]) action.asInstanceOf[TraceExpression].setProperty("type", typeList.get(0).toString)
      }
      types.add(typeList)
      actions.add(action)
    }
    if (types.isEmpty) grumble("At least one case clause is required in a typeswitch")
    expect(Token.DEFAULT)
    val defaultOffset: Int = t.currentTokenStartOffset
    nextToken()
    var defaultAction: Expression = null
    if (t.currentToken == Token.DOLLAR) {
      nextToken()
      expect(Token.NAME)
      val `var` = t.currentTokenValue
      val varQName = makeStructuredQName(`var`, "")
      nextToken()
      expect(Token.RETURN)
      nextToken()
      defaultAction = makeTracer(parseTypeswitchReturnClause(varQName, outerLet), varQName)
    }
    else {
      t.treatCurrentAsOperator()
      expect(Token.RETURN)
      nextToken()
      defaultAction = makeTracer(parseExprSingle, null)
    }
    var lastAction: Expression = defaultAction

    for (i <- types.size - 1 to 0 by -1) {
      val `var` = new LocalVariableReference(outerLet)
      setLocation(`var`)
      var ioe: Expression = new InstanceOfExpression(`var`, types.get(i).get(0))
      for (j <- 1 until types.get(i).size) {
        ioe = new OrExpression(ioe, new InstanceOfExpression(`var`.copy(new RebindingMap), types.get(i).get(j)))
      }
      setLocation(ioe)
      val ife = Choose.makeConditional(ioe, actions.get(i), lastAction)
      setLocation(ife)
      lastAction = ife
    }
    outerLet.setAction(lastAction)
    makeTracer(outerLet, null)
  }

  @throws[XPathException]
  private def parseSequenceTypeList: util.List[SequenceType] = {

    val typeList = new util.ArrayList[SequenceType]
    breakable {
      while (true) {
        val `type` = parseSequenceType
        typeList.add(`type`)
        t.treatCurrentAsOperator()
        if (t.currentToken == Token.UNION) nextToken()
        else break()
      }
    }
    expect(Token.RETURN)
    nextToken()
    typeList
  }

  @throws[XPathException]
  private def parseTypeswitchReturnClause(varQName: StructuredQName, outerLet: LetExpression): Expression = {
    var action: Expression = null
    val innerLet: LetExpression = makeLetExpression
    innerLet.setRequiredType(SequenceType.ANY_SEQUENCE)
    innerLet.setVariableQName(varQName)
    innerLet.setSequence(new LocalVariableReference(outerLet))
    declareRangeVariable(innerLet)
    action = parseExprSingle
    undeclareRangeVariable()
    innerLet.setAction(action)
    innerLet
  }


  @throws[XPathException]
  override  def parseSwitchExpression: Expression = {
    nextToken()
    val operand: Expression = parseExpression
    expect(Token.RPAR)
    nextToken()
    val conditions: util.List[Expression] = new util.ArrayList[Expression](10)
    val actions: util.List[Expression] = new util.ArrayList[Expression](10)


    val outerLet = makeLetExpression
    outerLet.setRequiredType(SequenceType.OPTIONAL_ATOMIC)
    outerLet.setVariableQName(new StructuredQName("zz", NamespaceConstant.SAXON, "zz_switchVar"))
    outerLet.setSequence(Atomizer.makeAtomizer(operand, null))
    do {
      val caseExpressions = new util.ArrayList[Expression](4)
      expect(Token.CASE)
      do {
        nextToken()
        val c = parseExprSingle
        caseExpressions.add(c)
      } while ( {
        t.currentToken == Token.CASE
      })
      expect(Token.RETURN)
      nextToken()
      val action = parseExprSingle
      for (i <- 0 until caseExpressions.size) {
        val vc = new EquivalenceComparison(new LocalVariableReference(outerLet), Token.FEQ, caseExpressions.get(i))
        if (i == 0) {
          conditions.add(vc)
          actions.add(action)
        }
        else {
          val orExpr = new OrExpression(conditions.remove(conditions.size - 1), vc)
          conditions.add(orExpr)
        }

      }
    } while ( {
      t.currentToken == Token.CASE
    })
    expect(Token.DEFAULT)
    nextToken()
    expect(Token.RETURN)
    nextToken()
    val defaultExpr = parseExprSingle
    conditions.add(Literal.makeLiteral(BooleanValue.TRUE))
    actions.add(defaultExpr)
    val choice = new Choose(conditions.toArray(new Array[Expression](0)), actions.toArray(new Array[Expression](conditions.size)))
    outerLet.setAction(choice)
    makeTracer(outerLet, null)
  }


  @throws[XPathException]
  override  def parseValidateExpression: Expression = {
    val offset: Int = t.currentTokenStartOffset
    var mode: Int = Validation.STRICT
    var foundCurly: Boolean = false
    var requiredType: SchemaType = null
    ensureSchemaAware("validate expression")
    t.currentToken match {
      case Token.VALIDATE_STRICT =>
        mode = Validation.STRICT
        nextToken()
      case Token.VALIDATE_LAX =>
        mode = Validation.LAX
        nextToken()
      case Token.VALIDATE_TYPE =>


        mode = Validation.BY_TYPE
        nextToken()
        expect(Token.KEYWORD_CURLY)
        if (!NameChecker.isQName(t.currentTokenValue)) grumble("Schema type name expected after 'validate type")
        requiredType = env.getConfiguration.getSchemaType(makeStructuredQName(t.currentTokenValue, env.getDefaultElementNamespace))
        if (requiredType == null) grumble("Unknown schema type " + t.currentTokenValue, "XQST0104")
        foundCurly = true
      case Token.KEYWORD_CURLY =>
        if (t.currentTokenValue == "validate") mode = Validation.STRICT
        else throw new AssertionError("shouldn't be parsing a validate expression")
        foundCurly = true
    }
    if (!foundCurly) expect(Token.LCURLY)
    nextToken()
    var exp = parseExpression
    if (exp.isInstanceOf[ParentNodeConstructor]) exp.asInstanceOf[ParentNodeConstructor].setValidationAction(mode, if (mode == Validation.BY_TYPE) requiredType
    else null)
    else {


      exp = new CopyOf(exp, true, mode, requiredType, true)
      setLocation(exp)
      exp.asInstanceOf[CopyOf].setRequireDocumentOrElement(true)
    }
    expect(Token.RCURLY)
    t.lookAhead()
    nextToken()
    makeTracer(exp, null)
  }


  @throws[XPathException]
  override  def parseExtensionExpression: Expression = {
    var requiredType: SchemaType = null
    val trimmed = Whitespace.removeLeadingWhitespace(t.currentTokenValue)
    var c = 0
    val len = trimmed.length
    while ( {
      c < len && " \t\r\n".indexOf(trimmed.charAt(c)) < 0
    }) c += 1
    val qname = trimmed.subSequence(0, c).toString
    var pragmaContents = ""
    while ( {
      c < len && " \t\r\n".indexOf(trimmed.charAt(c)) >= 0
    }) c += 1
    if (c < len) pragmaContents = trimmed.subSequence(c, len).toString
    var validateType = false
    val streaming = false
    val pragmaName = makeStructuredQName(qname, "")
    assert(pragmaName != null)
    val uri = pragmaName.getURI
    val localName = pragmaName.getLocalPart
    if (uri == NamespaceConstant.SAXON) if ("validate-type" == localName) if (!env.getConfiguration.isLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XQUERY))
      warning("Ignoring saxon:validate-type. To use this feature " + "you need the Saxon-EE processor from http:")
    else {
      val typeName = Whitespace.trim(pragmaContents)
      if (!NameChecker.isQName(typeName)) grumble("Schema type name expected in saxon:validate-type pragma: found " + Err.wrap(typeName))
      requiredType = env.getConfiguration.getSchemaType(makeStructuredQName(typeName, env.getDefaultElementNamespace))
      if (requiredType == null) grumble("Unknown schema type " + typeName)
      validateType = true
    }
    else warning("Ignored pragma " + qname + " (unrecognized Saxon pragma)")
    nextToken()
    var expr: Expression = null
    if (t.currentToken == Token.PRAGMA) expr = parseExtensionExpression
    else {
      expect(Token.LCURLY)
      nextToken()
      if (t.currentToken == Token.RCURLY) {
        t.lookAhead()
        nextToken()
        grumble("Unrecognized pragma, with no fallback expression", "XQST0079")
      }
      expr = parseExpression
      expect(Token.RCURLY)
      t.lookAhead()
      nextToken()
    }
    if (validateType) if (expr.isInstanceOf[ParentNodeConstructor]) {
      expr.asInstanceOf[ParentNodeConstructor].setValidationAction(Validation.BY_TYPE, requiredType)
      expr
    }
    else if (expr.isInstanceOf[AttributeCreator]) {
      if (!requiredType.isInstanceOf[SimpleType]) grumble("The type used for validating an attribute must be a simple type")

      expr.asInstanceOf[AttributeCreator].setSchemaType(requiredType.asInstanceOf[SimpleType])
      expr.asInstanceOf[AttributeCreator].setValidationAction(Validation.BY_TYPE)
      expr
    }
    else {
      val copy = new CopyOf(expr, true, Validation.BY_TYPE, requiredType, true)
      copy.setLocation(makeLocation)
      copy
    }
    else expr
  }


  @throws[XPathException]
  override  def parseConstructor: Expression = {
    val offset = t.currentTokenStartOffset
    t.currentToken match {
      case Token.TAG =>
        val tag = parsePseudoXML(false)
        lookAhead()
        t.setState(Tokenizer.OPERATOR_STATE)
        nextToken()
        return tag
      case Token.KEYWORD_CURLY =>
        val nodeKind = t.currentTokenValue
        nodeKind match {
          case "validate" =>
            grumble("A validate expression is not allowed within a path expression")


          case "ordered" =>
          case "unordered" =>

            nextToken()
            var content: Expression = null
            if (t.currentToken == Token.RCURLY && allowXPath31Syntax) content = Literal.makeEmptySequence
            else content = parseExpression
            expect(Token.RCURLY)
            lookAhead()
            nextToken()
            return content
          case "document" =>
            return parseDocumentConstructor(offset)
          case "element" =>
            return parseComputedElementConstructor(offset)
          case "attribute" =>
            return parseComputedAttributeConstructor(offset)
          case "text" =>
            return parseTextNodeConstructor(offset)
          case "comment" =>
            return parseCommentConstructor(offset)
          case "processing-instruction" =>
            return parseProcessingInstructionConstructor(offset)
          case "namespace" =>
            return parseNamespaceConstructor(offset)
          case _ =>
            grumble("Unrecognized node constructor " + t.currentTokenValue + "{}")
        }
      case Token.ELEMENT_QNAME =>
        return parseNamedElementConstructor(offset)
      case Token.ATTRIBUTE_QNAME =>
        return parseNamedAttributeConstructor(offset)
      case Token.NAMESPACE_QNAME =>
        return parseNamedNamespaceConstructor(offset)
      case Token.PI_QNAME =>
        return parseNamedProcessingInstructionConstructor(offset)
    }
    new ErrorExpression
  }


  @throws[XPathException]
  private def parseDocumentConstructor(offset: Int): Expression = {
    nextToken()
    var content: Expression = null
    if (t.currentToken == Token.RCURLY && allowXPath31Syntax) content = Literal.makeEmptySequence
    else content = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    val doc = new DocumentInstr(false, null)
    if (!(env.asInstanceOf[QueryModule]).isPreserveNamespaces) content = new CopyOf(content, false, Validation.PRESERVE, null, true)
    doc.setValidationAction(env.asInstanceOf[QueryModule].getConstructionMode, null)
    doc.setContentExpression(content)
    setLocation(doc, offset)
    doc
  }


  @throws[XPathException]
  private def parseComputedElementConstructor(offset: Int): Expression = {
    nextToken()

    val name = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    expect(Token.LCURLY)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    var content: Expression = null
    if (t.currentToken != Token.RCURLY) {
      content = parseExpression


      if (content.isInstanceOf[ElementCreator] && content.asInstanceOf[ElementCreator].getSchemaType == null) content.asInstanceOf[ElementCreator].setValidationAction(Validation.PRESERVE, null)
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    var inst: Instruction = null
    if (name.isInstanceOf[Literal]) {
      val vName = name.asInstanceOf[Literal].getValue

      var elemName: NodeName = null
      if (vName.isInstanceOf[StringValue] && !vName.isInstanceOf[AnyURIValue]) {
        val lex = vName.asInstanceOf[StringValue].getStringValue
        try {
          elemName = makeNodeName(lex, useDefault = true)
          elemName.obtainFingerprint(env.getConfiguration.getNamePool)
        } catch {
          case staticError: XPathException =>
            val code = staticError.getErrorCodeLocalPart
            if ("XPST0008" == code || "XPST0081" == code) staticError.setErrorCode("XQDY0074")
            else if ("XPST0003" == code) {
              grumble("Invalid QName in element constructor: " + lex, "XQDY0074", offset)
              return new ErrorExpression
            }
            staticError.setLocator(makeLocation)
            staticError.setIsStaticError(false)
            return new ErrorExpression(new XmlProcessingException(staticError))
        }
      }
      else if (vName.isInstanceOf[QualifiedNameValue]) {
        val uri = vName.asInstanceOf[QualifiedNameValue].getNamespaceURI
        elemName = new FingerprintedQName("", uri, vName.asInstanceOf[QualifiedNameValue].getLocalName)
        elemName.obtainFingerprint(env.getConfiguration.getNamePool)
      }
      else {
        grumble("Element name must be either a string or a QName", "XPTY0004", offset)
        return new ErrorExpression
      }
      inst = new FixedElement(elemName, env.asInstanceOf[QueryModule].getActiveNamespaceBindings, env.asInstanceOf[QueryModule].isInheritNamespaces, true, null, env.asInstanceOf[QueryModule].getConstructionMode)
      if (content == null) content = Literal.makeEmptySequence
      if (!(env.asInstanceOf[QueryModule]).isPreserveNamespaces) content = new CopyOf(content, false, Validation.PRESERVE, null, true)
      inst.asInstanceOf[FixedElement].setContentExpression(content)
      setLocation(inst, offset)

      makeTracer(inst, elemName.getStructuredQName)
    }
    else {
      val ns = new NamespaceResolverWithDefault(env.getNamespaceResolver, env.getDefaultElementNamespace)
      inst = new ComputedElement(name, null, null, env.asInstanceOf[QueryModule].getConstructionMode, env.asInstanceOf[QueryModule].isInheritNamespaces, true)
      setLocation(inst)
      if (content == null) content = Literal.makeEmptySequence
      if (!(env.asInstanceOf[QueryModule]).isPreserveNamespaces) content = new CopyOf(content, false, Validation.PRESERVE, null, true)
      inst.asInstanceOf[ComputedElement].setContentExpression(content)
      setLocation(inst, offset)
      makeTracer(inst, null)
    }
  }


  @throws[XPathException]
  private def parseNamedElementConstructor(offset: Int): Expression = {
    val nodeName: NodeName = makeNodeName(t.currentTokenValue, useDefault = true)
    var content: Expression = null
    nextToken()
    if (t.currentToken != Token.RCURLY) {
      content = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    val el2 = new FixedElement(nodeName, env.asInstanceOf[QueryModule].getActiveNamespaceBindings,
      env.asInstanceOf[QueryModule].isInheritNamespaces, true, null, env.asInstanceOf[QueryModule].getConstructionMode)
    setLocation(el2, offset)
    if (content == null) content = Literal.makeEmptySequence
    if (!(env.asInstanceOf[QueryModule]).isPreserveNamespaces)
      content = new CopyOf(content, false, Validation.PRESERVE, null, true)
    el2.setContentExpression(content)
    makeTracer(el2, nodeName.getStructuredQName)
  }


  @throws[XPathException]
  private def parseComputedAttributeConstructor(offset: Int): Expression = {
    nextToken()
    val name: Expression = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    expect(Token.LCURLY)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    var content: Expression = null
    if (t.currentToken != Token.RCURLY) {
      content = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    if (name.isInstanceOf[Literal]) {
      val vName = name.asInstanceOf[Literal].getValue
      if (vName.isInstanceOf[StringValue] && !vName.isInstanceOf[AnyURIValue]) {
        val lex = vName.asInstanceOf[StringValue].getStringValue
        if (lex == "xmlns" || lex.startsWith("xmlns:")) grumble("Cannot create a namespace using an attribute constructor", "XQDY0044", offset)
        var attributeName: NodeName = null
        try attributeName = makeNodeName(lex, useDefault = false)
        catch {
          case staticError: XPathException =>
            val code = staticError.getErrorCodeLocalPart
            staticError.setLocator(makeLocation)
            if ("XPST0008" == code || "XPST0081" == code) staticError.setErrorCode("XQDY0074")
            else if ("XPST0003" == code) {
              grumble("Invalid QName in attribute constructor: " + lex, "XQDY0074", offset)
              return new ErrorExpression
            }
            throw staticError
        }
        val fatt = new FixedAttribute(attributeName, Validation.STRIP, null)
        fatt.setRejectDuplicates()
        makeSimpleContent(content, fatt, offset)
        return makeTracer(fatt, null)
      }
      else if (vName.isInstanceOf[QNameValue]) {
        val qnv = vName.asInstanceOf[QNameValue]
        val attributeName = new FingerprintedQName(qnv.getPrefix, qnv.getNamespaceURI, qnv.getLocalName)
        attributeName.obtainFingerprint(env.getConfiguration.getNamePool)
        val fatt = new FixedAttribute(attributeName, Validation.STRIP, null)
        fatt.setRejectDuplicates()
        makeSimpleContent(content, fatt, offset)
        return makeTracer(fatt, null)
      }
    }
    val att = new ComputedAttribute(name, null, env.getNamespaceResolver, Validation.STRIP, null, true)
    att.setRejectDuplicates()
    makeSimpleContent(content, att, offset)
    makeTracer(att, null)
  }


  @throws[XPathException]
  private def parseNamedAttributeConstructor(offset: Int): Expression = {
    var warningStr: String = null
    if (t.currentTokenValue == "xmlns" || t.currentTokenValue.startsWith("xmlns:")) warningStr = "Cannot create a namespace declaration using an attribute constructor"
    var attributeName = makeNodeName(t.currentTokenValue, useDefault = false)
    if (!(attributeName.getURI == "") && attributeName.getPrefix == "") {
      attributeName = new FingerprintedQName("_", attributeName.getURI, attributeName.getLocalPart)
    }
    var attContent: Expression = null
    nextToken()
    if (t.currentToken != Token.RCURLY) {
      attContent = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    if (warningStr == null) {
      val att2 = new FixedAttribute(attributeName, Validation.STRIP, null)
      att2.setRejectDuplicates()
      att2.setRetainedStaticContext(env.makeRetainedStaticContext)
      makeSimpleContent(attContent, att2, offset)
      makeTracer(att2, attributeName.getStructuredQName)
    }
    else {
      warning(warningStr)
      new ErrorExpression(warningStr, "XQDY0044", false)
    }
  }

  @throws[XPathException]
  private def parseTextNodeConstructor(offset: Int): Expression = {
    nextToken()
    var value: Expression = null
    if (t.currentToken == Token.RCURLY && allowXPath31Syntax) value = Literal.makeEmptySequence
    else value = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    val select = XQueryParser.stringify(value, noNodeIfEmpty = true, env)
    val vof = new ValueOf(select, false, true)
    setLocation(vof, offset)
    makeTracer(vof, null)
  }

  @throws[XPathException]
  private def parseCommentConstructor(offset: Int): Expression = {
    nextToken()
    var value: Expression = null
    if (t.currentToken == Token.RCURLY && allowXPath31Syntax) value = Literal.makeEmptySequence
    else value = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    val com = new Comment
    makeSimpleContent(value, com, offset)
    makeTracer(com, null)
  }


  @throws[XPathException]
  private def parseProcessingInstructionConstructor(offset: Int): Expression = {
    nextToken()
    val name = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    expect(Token.LCURLY)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    var content: Expression = null
    if (t.currentToken != Token.RCURLY) {
      content = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    val pi: ProcessingInstruction = new ProcessingInstruction(name)
    makeSimpleContent(content, pi, offset)
    makeTracer(pi, null)
  }


  @throws[XPathException]
  private def parseNamedProcessingInstructionConstructor(offset: Int): Expression = {
    val target = t.currentTokenValue
    var warningStr: String = null
    if (target.equalsIgnoreCase("xml")) warningStr = "A processing instruction must not be named 'xml' in any combination of upper and lower case"
    if (!NameChecker.isValidNCName(target)) grumble("Invalid processing instruction name " + Err.wrap(target))
    val piName = new StringLiteral(target)
    var piContent: Expression = null
    nextToken()
    if (t.currentToken != Token.RCURLY) {
      piContent = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    if (warningStr == null) {
      val pi2 = new ProcessingInstruction(piName)
      makeSimpleContent(piContent, pi2, offset)
      makeTracer(pi2, null)
    }
    else {
      warning(warningStr)
      new ErrorExpression(warningStr, "XQDY0064", false)
    }
  }


  @throws[XPathException]
  override  def parseTryCatchExpression: Expression = {
    if (!allowXPath30Syntax) grumble("try/catch requires XQuery 3.0")
    val offset = t.currentTokenStartOffset
    nextToken()
    var tryExpr: Expression = null
    if (t.currentToken == Token.RCURLY && allowXPath31Syntax) tryExpr = Literal.makeEmptySequence
    else tryExpr = parseExpression
    val tryCatch = new TryCatch(tryExpr)
    setLocation(tryCatch, offset)
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    var foundOneCatch = false
    val tests = new util.ArrayList[QNameTest]
    while ( {
      isKeyword("catch")
    }) {
      tests.clear()
      foundOneCatch = true
      var seenCurly = false
      do {
        nextToken()
        var tokv = t.currentTokenValue
        t.currentToken match {
          case Token.NAME =>
            nextToken()
            tests.add(makeQNameTest(Type.ELEMENT, tokv))
          case Token.KEYWORD_CURLY =>
            nextToken()
            tests.add(makeQNameTest(Type.ELEMENT, tokv))
            seenCurly = true
          case Token.PREFIX =>
            nextToken()
            tests.add(makeNamespaceTest(Type.ELEMENT, tokv))
          case Token.SUFFIX =>
            nextToken()
            tokv = t.currentTokenValue
            if (t.currentToken == Token.NAME) {

            }
            else if (t.currentToken == Token.KEYWORD_CURLY) seenCurly = true
            else grumble("Expected name after '*:'")
            nextToken()
            tests.add(makeLocalNameTest(Type.ELEMENT, tokv))
          case Token.STAR =>
          case Token.MULT =>
            nextToken()
            tests.add(AnyNodeTest.getInstance)
          case _ =>
            grumble("Unrecognized name test")
            return null
        }
      } while ( {
        t.currentToken == Token.UNION && !(t.currentTokenValue == "union")
      })
      if (!seenCurly) {
        expect(Token.LCURLY)
        nextToken()
      }
      var test: QNameTest = null
      if (tests.size == 1) test = tests.get(0)
      else test = new UnionQNameTest(tests)
      catchDepth += 1
      var catchExpr: Expression = null
      if (t.currentToken == Token.RCURLY && allowXPath31Syntax) catchExpr = Literal.makeEmptySequence
      else catchExpr = parseExpression
      tryCatch.addCatchExpression(test, catchExpr)
      expect(Token.RCURLY)
      lookAhead()
      nextToken()
      catchDepth -= 1
    }
    if (!foundOneCatch) grumble("After try{}, expected 'catch'")
    tryCatch
  }


  @throws[XPathException]
  private def parseNamespaceConstructor(offset: Int): Expression = {
    if (!allowXPath30Syntax) grumble("Namespace node constructors require XQuery 3.0")
    nextToken()
    val nameExpr = parseExpression
    expect(Token.RCURLY)
    lookAhead()
    nextToken()
    expect(Token.LCURLY)
    t.setState(Tokenizer.DEFAULT_STATE)
    nextToken()
    var content: Expression = null
    if (t.currentToken != Token.RCURLY) {
      content = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    val instr = new NamespaceConstructor(nameExpr)
    setLocation(instr)
    makeSimpleContent(content, instr, offset)
    makeTracer(instr, null)
  }


  @throws[XPathException]
  private def parseNamedNamespaceConstructor(offset: Int): Expression = {
    if (!allowXPath30Syntax) grumble("Namespace node constructors require XQuery 3.0")
    val target = t.currentTokenValue
    if (!NameChecker.isValidNCName(target)) grumble("Invalid namespace prefix " + Err.wrap(target))
    val nsName = new StringLiteral(target)
    var nsContent: Expression = null
    nextToken()
    if (t.currentToken != Token.RCURLY) {
      nsContent = parseExpression
      expect(Token.RCURLY)
    }
    lookAhead()
    nextToken()
    val instr = new NamespaceConstructor(nsName)
    makeSimpleContent(nsContent, instr, offset)
    makeTracer(instr, null)
  }


   def makeSimpleContent(content: Expression, inst: SimpleNodeConstructor, offset: Int): Unit = {
    if (content == null) inst.setSelect(new StringLiteral(StringValue.EMPTY_STRING))
    else inst.setSelect(XQueryParser.stringify(content, noNodeIfEmpty = false, env))
    setLocation(inst, offset)
  }


  @throws[XPathException]
  private def parsePseudoXML(allowEndTag: Boolean): Expression = try {

    var exp: Expression = null
    val offset = t.inputOffset

    var c = t.nextChar
    c match {
      case '!' =>
        c = t.nextChar
        if (c == '-') exp = parseCommentConstructor
        else if (c == '[') {
          grumble("A CDATA section is allowed only in element content")
          return null

        }
        else {
          grumble("Expected '--' or '[CDATA[' after '<!'")
          return null
        }
      case '?' =>
        exp = parsePIConstructor
      case '/' =>
        if (allowEndTag) {
          val sb = new FastStringBuffer(FastStringBuffer.C16)
          breakable {
            while (true) {
              c = t.nextChar
              if (c == '>') break()
              sb.cat(c)
            }
          }
          return new StringLiteral(sb.toString)
        }
        grumble("Unmatched XML end tag")
        return new ErrorExpression
      case _ =>
        t.unreadChar()
        exp = parseDirectElementConstructor(allowEndTag)
    }
    setLocation(exp, offset)
    exp
  } catch {
    case e: StringIndexOutOfBoundsException =>
      grumble("End of input encountered while parsing direct constructor")
      new ErrorExpression
  }


  @throws[XPathException]
  @throws[StringIndexOutOfBoundsException]
  private def parseDirectElementConstructor(isNested: Boolean): Expression = {

    val pool = env.getConfiguration.getNamePool
    var changesContext = false
    val offset = t.inputOffset - 1
    var c: Char = 0
    val buff = new FastStringBuffer(FastStringBuffer.C64)
    var namespaceCount = 0
    breakable {
      while ( {
        true
      }) {
        c = t.nextChar
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '/' || c == '>') break()
        buff.cat(c)
      }
    }
    val elname = buff.toString
    if (elname.isEmpty) grumble("Expected element name after '<'")

    val attributes = new util.LinkedHashMap[String, XQueryParser.AttributeDetails](10)
    while ( {
      true
    }) {


      c = skipSpaces(c)
      if (c == '/' || c == '>') break()
      val attOffset = t.inputOffset - 1
      buff.setLength(0)

      do {
        buff.cat(c)
        c = t.nextChar
      } while ( {
        c != ' ' && c != '\n' && c != '\r' && c != '\t' && c != '='
      })
      val attName = buff.toString
      if (!NameChecker.isQName(attName)) grumble("Invalid attribute name " + Err.wrap(attName, Err.ATTRIBUTE))
      c = skipSpaces(c)
      expectChar(c, '=')
      c = t.nextChar
      c = skipSpaces(c)
      val delim = c
      val isNamespace = "xmlns" == attName || attName.startsWith("xmlns:")
      var end = 0
      if (isNamespace) {
        end = makeNamespaceContent(t.input, t.inputOffset, delim)
        changesContext = true
      }
      else {
        var avt: Expression = null
        try avt = makeAttributeContent(t.input, t.inputOffset, delim, scanOnly = true)
        catch {
          case err: XPathException =>
            if (!err.hasBeenReported) grumble(err.getMessage)
            throw err
        }

        end = (avt.asInstanceOf[Literal]).getValue.asInstanceOf[Int64Value].longValue.toInt
      }

      val `val` = t.input.substring(t.inputOffset - 1, end + 1)

      var rval = t.input.substring(t.inputOffset, end)


      var tail = `val`
      var pos = 0
      while ( {
        {
          (pos = tail.indexOf('\n'))
          pos
        } >= 0
      }) {
        t.incrementLineNumber(t.inputOffset - 1 + pos)
        tail = tail.substring(pos + 1)
      }
      t.inputOffset = end + 1
      breakable {
        if (isNamespace) {

          val sb = new FastStringBuffer(rval.length)
          var prevDelim = false
          var prevOpenCurly = false
          var prevCloseCurly = false
          for (i <- 0 until rval.length) {
            val n = rval.charAt(i)
            if (n == delim) {
              prevDelim = !prevDelim
              if (prevDelim) {}
            }
            if (n == '{') {
              prevOpenCurly = !prevOpenCurly
              if (prevOpenCurly) {}
            }
            else if (prevOpenCurly) grumble("Namespace must not contain an unescaped opening brace", "XQST0022")
            if (n == '}') {
              prevCloseCurly = !prevCloseCurly
              if (prevCloseCurly) {}
            }
            else if (prevCloseCurly) grumble("Namespace must not contain an unescaped closing brace", "XPST0003")
            sb.cat(n)
          }
          if (prevOpenCurly) grumble("Namespace must not contain an unescaped opening brace", "XQST0022")
          if (prevCloseCurly) grumble("Namespace must not contain an unescaped closing brace", "XPST0003")
          rval = sb.toString
          val uri = uriLiteral(rval)
          if (!StandardURIChecker.getInstance.isValidURI(uri)) grumble("Namespace must be a valid URI value", "XQST0046")
          var prefix: String = null
          if ("xmlns" == attName) {
            prefix = ""
            if (uri == NamespaceConstant.XML) grumble("Cannot have the XML namespace as the default namespace", "XQST0070")
          }
          else {
            prefix = attName.substring(6)
            if (prefix == "xml" && !(uri == NamespaceConstant.XML)) grumble("Cannot bind the prefix 'xml' to a namespace other than the XML namespace", "XQST0070")
            else if (uri == NamespaceConstant.XML && !(prefix == "xml")) grumble("Cannot bind a prefix other than 'xml' to the XML namespace", "XQST0070")
            else if (prefix == "xmlns") grumble("Cannot use xmlns as a namespace prefix", "XQST0070")
            if (uri.isEmpty) if (env.getConfiguration.getXMLVersion == Configuration.XML10) grumble("Namespace URI must not be empty", "XQST0085")
          }
          namespaceCount += 1
          env.asInstanceOf[QueryModule].declareActiveNamespace(prefix, uri)
        }
      }
      if (attributes.get(attName) != null) if (isNamespace) grumble("Duplicate namespace declaration " + attName, "XQST0071", attOffset)
      else grumble("Duplicate attribute name " + attName, "XQST0040", attOffset)


      val a = new XQueryParser.AttributeDetails
      a.value = `val`
      a.startOffset = attOffset
      attributes.put(attName, a)

      c = t.nextChar
      if (!(c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '/' || c == '>')) grumble("There must be whitespace after every attribute except the last")
    }
    var qName: StructuredQName = null
    if (scanOnly) qName = StandardNames.getStructuredQName(StandardNames.XSL_ELEMENT)
    else try {
      val parts = NameChecker.getQNameParts(elname)
      val namespace = env.asInstanceOf[QueryModule].checkURIForPrefix(parts(0))
      if (namespace == null) grumble("Undeclared prefix in element name " + Err.wrap(elname, Err.ELEMENT), "XPST0081", offset)
      qName = new StructuredQName(parts(0), namespace, parts(1))
    } catch {
      case e: QNameException =>
        grumble("Invalid element name " + Err.wrap(elname, Err.ELEMENT), "XPST0003", offset)
        qName = StandardNames.getStructuredQName(StandardNames.XSL_ELEMENT)
    }
    val validationMode = env.asInstanceOf[QueryModule].getConstructionMode
    val fqn = new FingerprintedQName(qName.getPrefix, qName.getURI, qName.getLocalPart, pool.allocateFingerprint(qName.getURI, qName.getLocalPart))
    val elInst = new FixedElement(fqn, env.asInstanceOf[QueryModule].getActiveNamespaceBindings, env.asInstanceOf[QueryModule].isInheritNamespaces, !isNested, null, validationMode)
    setLocation(elInst, offset)
    val contents = new util.ArrayList[Expression](10)
    val attFingerprints = new IntHashSet(attributes.size)


    for (entry <- attributes.entrySet.asScala) {
      val attName = entry.getKey
      val a = entry.getValue
      val attValue = a.value
      val attOffset = a.startOffset
      if ("xmlns" == attName || attName.startsWith("xmlns:")) {

      }
      else if (scanOnly) {
      }
      else {
        var attributeName: NodeName = null
        var attNamespace: String = null
        try {
          val parts = NameChecker.getQNameParts(attName)
          if (parts(0).isEmpty) {
            attNamespace = ""
          }
          else attNamespace = env.asInstanceOf[QueryModule].checkURIForPrefix(parts(0))
          if (attNamespace == null) grumble("Undeclared prefix in attribute name " + Err.wrap(attName, Err.ATTRIBUTE), "XPST0081", attOffset)
          attributeName = new FingerprintedQName(parts(0), attNamespace, parts(1))
          val key = attributeName.obtainFingerprint(pool)
          if (attFingerprints.contains(key)) grumble("Duplicate expanded attribute name " + attName, "XQST0040", attOffset)
          attFingerprints.add(key)
        } catch {
          case e: QNameException =>
            grumble("Invalid attribute name " + Err.wrap(attName, Err.ATTRIBUTE), "XPST0003", attOffset)
        }
        assert(attributeName != null)
        val attInst = new FixedAttribute(attributeName, Validation.STRIP, null)
        setLocation(attInst)
        var select: Expression = null
        try select = makeAttributeContent(attValue, 1, attValue.charAt(0), scanOnly = false)
        catch {
          case err: XPathException =>
            err.setIsStaticError(true)
            throw err
        }
        attInst.setRetainedStaticContext(env.makeRetainedStaticContext)
        attInst.setSelect(select)
        attInst.setRejectDuplicates()
        setLocation(attInst)
        contents.add(makeTracer(attInst, attributeName.getStructuredQName))
      }
    }
    if (c == '/') {
      expectChar(t.nextChar, '>')
    }
    else readElementContent(elname, contents)
    val elk = new Array[Expression](contents.size)
    for (i <- 0 until contents.size) {
      if (validationMode != Validation.STRIP) contents.get(i).suppressValidation(validationMode)
      elk(i) = contents.get(i)
    }
    val block = new Block(elk)
    if (changesContext) block.setRetainedStaticContext(env.makeRetainedStaticContext)
    elInst.setContentExpression(block)

    for (n <- 0 until namespaceCount) {
      env.asInstanceOf[QueryModule].undeclareNamespace()
    }
    makeTracer(elInst, qName)
  }


  @throws[XPathException]
  private def makeAttributeContent(avt: String, start: Int, terminator: Char, scanOnly: Boolean): Expression = {

    val loc = makeLocation
    val components = new util.ArrayList[Expression](10)
    var i0 = 0
    var i1 = 0
    var i2 = 0
    var i8 = 0
    var i9 = 0
    var len = 0
    var last = 0
    last = start
    len = avt.length
    breakable {
      while ( {
        last < len
      }) {
        i2 = avt.indexOf(terminator, last)
        if (i2 < 0) {
          val e = new XPathException("Attribute constructor is not properly terminated")
          e.setIsStaticError(true)
          throw e
        }
        i0 = avt.indexOf("{", last)
        i1 = avt.indexOf("{{", last)
        i8 = avt.indexOf("}", last)
        i9 = avt.indexOf("}}", last)
        if ((i0 < 0 || i2 < i0) && (i8 < 0 || i2 < i8)) {
          addStringComponent(components, avt, last, i2)

          if (i2 + 1 < avt.length && avt.charAt(i2 + 1) == terminator) {
            components.add(new StringLiteral(terminator.toString))
            last = i2 + 2

          }
          else {
            last = i2
            break()
          }
        }
        else if (i8 >= 0 && (i0 < 0 || i8 < i0)) {
          if (i8 != i9) {
            val e = new XPathException("Closing curly brace in attribute value template \"" + avt + "\" must be doubled")
            e.setIsStaticError(true)
            throw e
          }
          addStringComponent(components, avt, last, i8 + 1)
          last = i8 + 2
        }
        else if (i1 >= 0 && i1 == i0) {
          addStringComponent(components, avt, last, i1 + 1)
          last = i1 + 2
        }
        else if (i0 >= 0) {
          if (i0 > last) addStringComponent(components, avt, last, i0)
          var exp: Expression = null
          val parser = newParser
          parser.asInstanceOf[XQueryParser].executable = executable
          parser.setAllowAbsentExpression(allowXPath31Syntax)
          parser.setScanOnly(scanOnly)
          parser.setRangeVariableStack(rangeVariables)
          parser.setCatchDepth(catchDepth)
          exp = parser.parse(avt, i0 + 1, Token.RCURLY, env)
          if (!scanOnly) exp = exp.simplify
          last = parser.getTokenizer.currentTokenStartOffset + 1
          components.add(XQueryParser.makeStringJoin(exp, env))
        }
        else throw new IllegalStateException("Internal error parsing direct attribute constructor")
      }
    }

    if (scanOnly) return Literal.makeLiteral(Int64Value.makeIntegerValue(last))

    if (components.isEmpty) return new StringLiteral(StringValue.EMPTY_STRING)

    if (components.size == 1) return components.get(0)

    val args = new Array[Expression](components.size)
    components.toArray(args)
    val rsc = new RetainedStaticContext(env)
    val fn = SystemFunction.makeCall("concat", rsc, args.toIndexedSeq: _*)
    assert(fn != null)
    fn.setLocation(loc)
    fn

  }

  @throws[XPathException]
  private def addStringComponent(components: util.List[Expression], avt: String, start: Int, end: Int): Unit = {
    if (start < end) {
      val sb = new FastStringBuffer(end - start)
      var i = start
      while ( {
        i < end
      }) {
        val c = avt.charAt(i)
        c match {
          case '&' =>
            val semic = avt.indexOf(';', i)
            if (semic < 0) grumble("No closing ';' found for entity or character reference")
            else {
              val entity = avt.substring(i + 1, semic)
              sb.append(new XQueryParser.Unescaper(env.getConfiguration.getValidCharacterChecker).analyzeEntityReference(entity))
              i = semic
            }
          case '<' =>
            grumble("The < character must not appear in attribute content")
          case '\n' =>
          case '\t' =>
            sb.cat(' ')
          case '\r' =>
            sb.cat(' ')
            if (i + 1 < end && avt.charAt(i + 1) == '\n') i += 1
          case _ =>
            sb.cat(c)
        }
        i += 1
      }
      components.add(new StringLiteral(sb.toString))
    }
  }


  @throws[XPathException]
  private def makeNamespaceContent(avt: String, start: Int, terminator: Char): Int = {

    var i2 = 0
    var len = 0
    var last = 0
    last = start
    len = avt.length
    breakable {
      while ( {
        last < len
      }) {
        i2 = avt.indexOf(terminator, last)
        if (i2 < 0) {
          val e = new XPathException("Namespace declaration is not properly terminated")
          e.setIsStaticError(true)
          throw e
        }
        if (i2 + 1 < avt.length && avt.charAt(i2 + 1) == terminator) last = i2 + 2
        else {
          last = i2
          break()
        }
      }
    }
    last
  }


  @throws[XPathException]
  private def readElementContent(startTag: String, components: util.List[Expression]): Unit = try {

    var afterEnclosedExpr = false
    while (true) {
      val text: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
      var c: Char = 0
      var containsEntities = false
      while (true) {
        c = t.nextChar
        breakable {
          if (c == '<') {
            if (t.nextChar == '!') if (t.nextChar == '[') {
              readCDATASection(text)
              containsEntities = true
            }
            else {
              t.unreadChar()
              t.unreadChar()
            }
            else t.unreadChar()
            break()
          }
          else if (c == '&') {
            text.append(readEntityReference)
            containsEntities = true
          }
          else if (c == '}') {
            c = t.nextChar
            if (c != '}') grumble("'}' must be written as '}}' within element content")
            text.cat(c)
          }
          else if (c == '{') {
            c = t.nextChar
            if (c != '{') {
              c = '{'
              break()
            }
            text.cat(c)
          }
          else {
            if (!charChecker.test(c) && !UTF16CharacterSet.isSurrogate(c)) grumble("Character code " + c + " is not a valid XML character")
            text.cat(c)
          }
        }
      }
      if (!text.isEmpty && (containsEntities | env.asInstanceOf[QueryModule].isPreserveBoundarySpace || !Whitespace.isWhite(text))) {
        val inst = new ValueOf(new StringLiteral(new StringValue(text.condense)), false, false)
        setLocation(inst)
        components.add(inst)
        afterEnclosedExpr = false
      }
      if (c == '<') {
        val exp = parsePseudoXML(true)

        if (exp.isInstanceOf[StringLiteral]) {
          var endTag = exp.asInstanceOf[StringLiteral].getStringValue
          if (Whitespace.isWhitespace(endTag.charAt(0)).asInstanceOf[Boolean]) grumble("End tag contains whitespace before the name")
          endTag = Whitespace.trim(endTag)
          if (endTag == startTag) return
          else {
            grumble("End tag </" + endTag + "> does not match start tag <" + startTag + '>', "XQST0118")

          }
        }
        else components.add(exp)
      }
      else {
        if (afterEnclosedExpr) {
          val previousComponent = components.get(components.size - 1)
          var previousComponentIsNodeTest = true
          val previousItemType = previousComponent.getStaticUType(UType.ANY)
          previousComponentIsNodeTest = UType.ANY_NODE.subsumes(previousItemType)
          if (!previousComponentIsNodeTest) {

            val inst = new ValueOf(new StringLiteral(StringValue.EMPTY_STRING), false, false)
            setLocation(inst)
            components.add(inst)
          }
        }
        t.unreadChar()
        t.setState(Tokenizer.DEFAULT_STATE)
        lookAhead()
        nextToken()
        if (t.currentToken == Token.RCURLY && allowXPath31Syntax) components.add(Literal.makeEmptySequence)
        else {
          var exp = parseExpression
          if (!(env.asInstanceOf[QueryModule]).isPreserveNamespaces) exp =
            new CopyOf(exp, false, Validation.PRESERVE, null, true)
          components.add(exp)
          expect(Token.RCURLY)
        }
        afterEnclosedExpr = true
      }
    }
  } catch {
    case err: StringIndexOutOfBoundsException =>
      grumble("No closing end tag found for direct element constructor")
  }

  @throws[XPathException]
  private def parsePIConstructor: Expression = try {
    val pi = new FastStringBuffer(FastStringBuffer.C64)
    var firstSpace = -1
    while ( {
      !pi.toString.endsWith("?>")
    }) {
      val c = t.nextChar
      if (firstSpace < 0 && " \t\r\n".indexOf(c) >= 0) firstSpace = pi.length
      pi.cat(c)
    }
    pi.setLength(pi.length - 2)
    var target: String = null
    var data: String = ""
    if (firstSpace < 0) {
      target = pi.toString
    }
    else {
      target = pi.toString.substring(0, firstSpace)
      firstSpace += 1
      while ( {
        firstSpace < pi.length && " \t\r\n".indexOf(pi.charAt(firstSpace)) >= 0
      }) firstSpace += 1
      data = pi.toString.substring(firstSpace)
    }
    if (!NameChecker.isValidNCName(target)) grumble("Invalid processing instruction name " + Err.wrap(target))
    if (target.equalsIgnoreCase("xml")) grumble("A processing instruction must not be named 'xml' in any combination of upper and lower case")
    val instruction = new ProcessingInstruction(new StringLiteral(target))
    instruction.setSelect(new StringLiteral(data))
    setLocation(instruction)
    instruction
  } catch {
    case err: StringIndexOutOfBoundsException =>
      grumble("No closing '?>' found for processing instruction")
      null
  }

  @throws[XPathException]
  private def readCDATASection(cdata: FastStringBuffer): Unit = try {
    var c: Char = 0

    c = t.nextChar
    expectChar(c, 'C')
    c = t.nextChar
    expectChar(c, 'D')
    c = t.nextChar
    expectChar(c, 'A')
    c = t.nextChar
    expectChar(c, 'T')
    c = t.nextChar
    expectChar(c, 'A')
    c = t.nextChar
    expectChar(c, '[')
    while ( {
      !cdata.toString.endsWith("]]>")
    }) cdata.cat(t.nextChar)
    cdata.setLength(cdata.length - 3)
  } catch {
    case err: StringIndexOutOfBoundsException =>
      grumble("No closing ']]>' found for CDATA section")
  }

  @throws[XPathException]
  private def parseCommentConstructor: Expression = try {
    val c = t.nextChar

    expectChar(c, '-')
    val comment = new FastStringBuffer(FastStringBuffer.C256)
    while ( {
      !comment.toString.endsWith("--")
    }) comment.cat(t.nextChar)
    if (t.nextChar != '>') grumble("'--' is not permitted in an XML comment")
    val commentText = comment.subSequence(0, comment.length - 2)
    val instruction = new Comment
    instruction.setSelect(new StringLiteral(new StringValue(commentText)))
    setLocation(instruction)
    instruction
  } catch {
    case err: StringIndexOutOfBoundsException =>
      grumble("No closing '-->' found for comment constructor")
      null
  }


  @throws[XPathException]
   override def makeStringLiteral(token: String): Literal = {
    var lit: StringLiteral = null
    if (token.indexOf('&') == -1) lit = new StringLiteral(token)
    else {
      val sb = unescape(token)
      lit = new StringLiteral(StringValue.makeStringValue(sb))
    }
    setLocation(lit)
    lit
  }


  @throws[XPathException]
  override  def unescape(token: String): CharSequence = new XQueryParser.Unescaper(env.getConfiguration.getValidCharacterChecker).unescape(token)


  @throws[XPathException]
  private def readEntityReference: String = try {

    val sb = new FastStringBuffer(FastStringBuffer.C64)
    breakable {
      while (true) {
        val c = t.nextChar
        if (c == ';') break()
        sb.cat(c)
      }
    }
    val entity = sb.toString
    new XQueryParser.Unescaper(env.getConfiguration.getValidCharacterChecker).analyzeEntityReference(entity)
  } catch {
    case err: StringIndexOutOfBoundsException =>
      grumble("No closing ';' found for entity or character reference")
      ""
  }


  @throws[XPathException]
  override def parseStringConstructor: Expression = {
    val offset = t.currentTokenStartOffset
    if (!allowXPath31Syntax) throw new XPathException("String constructor expressions require XQuery 3.1")
    val components = new util.ArrayList[Expression]
    components.add(new StringLiteral(t.currentTokenValue))
    t.next()
    val outer = new Breaks
    val inner = new Breaks
    outer.breakable {
      while (true) {
        val emptyExpression = t.currentToken == Token.RCURLY
        if (emptyExpression) components.add(new StringLiteral(StringValue.EMPTY_STRING))
        else {
          val enclosed = parseExpression
          val stringJoin = SystemFunction.makeCall("string-join", env.makeRetainedStaticContext, enclosed, new StringLiteral(" "))
          components.add(stringJoin)
        }
        if (t.currentToken != Token.RCURLY) grumble("Expected '}' after enclosed expression in string constructor")
        val sb = new FastStringBuffer(256)
        var c = t.nextChar
        if (c != '`') grumble("Expected '}`' after enclosed expression in string constructor")
        var prior = 0.toChar
        var penult = 0.toChar
        try
          inner.breakable {
            while (true) {
              c = t.nextChar
              if (prior == '`' && c == '{') {
                sb.setLength(sb.length - 1)
                components.add(new StringLiteral(sb))
                t.lookAhead()
                t.next()
                if (t.currentToken == Token.RCURLY) {
                  components.add(Literal.makeEmptySequence)
                  sb.setLength(0)
                } else {
                  inner.break()
                }
              } else if (penult == ']' && prior == '`' && c == '`') {
                sb.setLength(sb.length - 2)
                components.add(new StringLiteral(sb))
                t.lookAhead()
                t.next()
                outer.break()
              } else {
                sb.cat(c)
                penult = prior
                prior = c
              }
            }
          }
        catch {
          case e: StringIndexOutOfBoundsException =>
            grumble("String constructor is missing ]`` terminator ")
        }
      }
    }
    val args = components.toArray(new Array[Expression](0))
    val result = SystemFunction.makeCall("concat", env.makeRetainedStaticContext, args.toIndexedSeq: _*)
    setLocation(result, offset)
    result
  }


  @throws[XPathException]
  def uriLiteral(in: String): String = Whitespace.applyWhitespaceNormalization(Whitespace.COLLAPSE, unescape(in)).toString


  @throws[XPathException]
   def lookAhead(): Unit = try t.lookAhead()
  catch {
    case err: XPathException =>
      grumble(err.getMessage)
  }

  override  def atStartOfRelativePath: Boolean = {
    t.currentToken == Token.TAG || super.atStartOfRelativePath

  }

  @throws[XPathException]
  override  def testPermittedAxis(axis: Int, errorCode: String): Unit = {
    super.testPermittedAxis(axis, errorCode)
    if (axis == AxisInfo.NAMESPACE && (language eq ParsedLanguage.XQUERY)) grumble("The namespace axis is not available in XQuery", errorCode)
  }


  @throws[StringIndexOutOfBoundsException]
  private def skipSpaces(c: Char): Char = {
    var ch = c
    while ( {
      ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t'
    }) ch = t.nextChar
    ch
  }


  @throws[XPathException]
  private def expectChar(actual: Char, expected: Char): Unit = if (actual != expected) grumble("Expected '" + expected + "', found '" + actual + '\'')


  override  def getLanguage: String = "XQuery"
}