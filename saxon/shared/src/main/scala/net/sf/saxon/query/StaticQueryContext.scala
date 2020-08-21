package net.sf.saxon.query

import net.sf.saxon.expr.Literal
import net.sf.saxon.expr.instruct.Executable
import net.sf.saxon.expr.instruct.GlobalParam
import net.sf.saxon.expr.instruct.GlobalVariable
import net.sf.saxon.expr.parser.CodeInjector
import net.sf.saxon.expr.parser.OptimizerOptions
import net.sf.saxon.functions.FunctionLibrary
import net.sf.saxon.lib._
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.om.NamePool
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.s9api.Location
import net.sf.saxon.trace.TraceCodeInjector
import net.sf.saxon.trans.UncheckedXPathException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.SequenceType
import javax.xml.transform.ErrorListener
import java.io.IOException
import java.io.InputStream
import java.io.Reader
import java.util._

import StaticQueryContext._
import net.sf.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

object StaticQueryContext {

  def makeDefaultStaticQueryContext(
                                     config: Configuration): StaticQueryContext = {
    val sqc: StaticQueryContext = new StaticQueryContext()
    sqc.config = config
    sqc.namePool = config.getNamePool
    sqc.reset()
    sqc
  }

}

class StaticQueryContext () {

  private var config: Configuration = _

  @BeanProperty
  var namePool: NamePool = _

  @BeanProperty
  var baseURI: String = _

  @BeanProperty
  var userDeclaredNamespaces: HashMap[String, String] = _

  private var userDeclaredVariables: Set[GlobalVariable] = _

  @BooleanBeanProperty
  var inheritNamespaces: Boolean = true

  @BooleanBeanProperty
  var preserveNamespaces: Boolean = true

  @BeanProperty
  var constructionMode: Int = Validation.PRESERVE

  @BeanProperty
  var defaultFunctionNamespace: String = NamespaceConstant.FN

  @BeanProperty
  var default_ElementNamespace: String = NamespaceConstant.NULL

  @BeanProperty
  var requiredContextItemType: ItemType = AnyItemType.getInstance

  private var preserveSpace: Boolean = false

  private var defaultEmptyLeast: Boolean = true

  @BeanProperty
  var moduleURIResolver: ModuleURIResolver = _

  @BeanProperty
  var errorReporter: ErrorReporter = new StandardErrorReporter()

  @BeanProperty
  var codeInjector: CodeInjector = _

  var isUpdating: Boolean = false

  @BeanProperty
  var defaultCollationName: String = _

  @BeanProperty
  var moduleLocation: Location = _

  @BeanProperty
  var optimizerOptions: OptimizerOptions = _

  def this(config: Configuration, initialize: Boolean) = {
    this()
    this.config = config
    this.namePool = config.getNamePool
    if (initialize) {
      copyFrom(config.getDefaultStaticQueryContext)
    } else {
      userDeclaredNamespaces = new HashMap()
      userDeclaredVariables = new HashSet()
      optimizerOptions = config.getOptimizerOptions
      clearNamespaces()
    }
  }

  def this(config: Configuration) = this(config, true)

  def this(c: StaticQueryContext) = {
    this()
    copyFrom(c)
  }

  def copyFrom(c: StaticQueryContext): Unit = {
    config = c.config
    namePool = c.namePool
    baseURI = c.baseURI
    moduleURIResolver = c.moduleURIResolver
    if (c.userDeclaredNamespaces != null) {
      userDeclaredNamespaces = new HashMap(c.userDeclaredNamespaces)
    }
    if (c.userDeclaredVariables != null) {
      userDeclaredVariables = new HashSet(c.userDeclaredVariables)
    }
    inheritNamespaces = c.inheritNamespaces
    preserveNamespaces = c.preserveNamespaces
    constructionMode = c.constructionMode
    defaultFunctionNamespace = c.defaultFunctionNamespace
    requiredContextItemType = c.requiredContextItemType
    preserveSpace = c.preserveSpace
    defaultEmptyLeast = c.defaultEmptyLeast
    moduleURIResolver = c.moduleURIResolver
    errorReporter = c.errorReporter
    codeInjector = c.codeInjector
    isUpdating = c.isUpdating
    optimizerOptions = c.optimizerOptions
  }

  def reset(): Unit = {
    userDeclaredNamespaces = new HashMap(10)
    errorReporter = new StandardErrorReporter()
    constructionMode =
      if (getConfiguration.isLicensedFeature(
        Configuration.LicenseFeature.ENTERPRISE_XQUERY))
        Validation.PRESERVE
      else Validation.STRIP
    preserveSpace = false
    defaultEmptyLeast = true
    requiredContextItemType = AnyItemType.getInstance
    defaultFunctionNamespace = NamespaceConstant.FN
    default_ElementNamespace = NamespaceConstant.NULL
    moduleURIResolver = null
    defaultCollationName = config.getDefaultCollationName
    clearNamespaces()
    isUpdating = false
    optimizerOptions = config.getOptimizerOptions
  }

  def setConfiguration(config: Configuration): Unit = {
    if (this.config != null && this.config != config) {
      throw new IllegalArgumentException(
        "Configuration cannot be changed dynamically")
    }
    this.config = config
    namePool = config.getNamePool
  }

  def getConfiguration(): Configuration = config

  def makeExecutable(): Executable = {
    val executable: Executable = new Executable(config)
    executable.setSchemaAware(isSchemaAware)
    executable.setHostLanguage(HostLanguage.XQUERY)
    executable
  }

  def setSchemaAware(aware: Boolean): Unit = {
    if (aware) {
      throw new UnsupportedOperationException(
        "Schema-awareness requires Saxon-EE")
    }
  }

  def isSchemaAware(): Boolean = false

  def setStreaming(option: Boolean): Unit = {
    if (option) {
      throw new UnsupportedOperationException("Streaming requires Saxon-EE")
    }
  }

  def isStreaming(): Boolean = false

  def setLanguageVersion(version: Int): Unit = {
    if (version == 10 || version == 30 || version == 31) {} else {
      throw new IllegalArgumentException("languageVersion = " + version)
    }
  }

  def getLanguageVersion(): Int = {
    val langVersion: java.lang.Integer = 31
    langVersion
  }

  def getExtensionFunctionLibrary(): FunctionLibrary = null

  def isCompileWithTracing(): Boolean =
    codeInjector.isInstanceOf[TraceCodeInjector]

  def setCompileWithTracing(trace: Boolean): Unit = {
    codeInjector = if (trace) new TraceCodeInjector() else null
  }

  def compileQuery(query: String): XQueryExpression = {
    val qp: XQueryParser = config
      .newExpressionParser("XQ", isUpdating, 31)
      .asInstanceOf[XQueryParser]
    if (codeInjector != null) {
      qp.setCodeInjector(codeInjector)
    } else if (config.isCompileWithTracing) {
      qp.setCodeInjector(new TraceCodeInjector())
    }
    qp.setStreaming(isStreaming)
    val mainModule: QueryModule = new QueryModule(this)
    qp.makeXQueryExpression(query, mainModule, config)
  }

  def compileQuery(source: Reader): XQueryExpression = synchronized {
    val buffer: Array[Char] = Array.ofDim[Char](4096)
    val sb: StringBuilder = new StringBuilder(4096)
    breakable { while (true) {
      val n: Int = source.read(buffer)
      if (n > 0) {
        sb.append(buffer, 0, n)
      } else {
        break()
      }
    }
  }
    compileQuery(sb.toString)
  }

  def compileQuery(source: InputStream, encoding: String): XQueryExpression =
    synchronized {
      val query: String =
        QueryReader.readInputStream(source,
          encoding,
          config.getValidCharacterChecker)
      compileQuery(query)
    }

  def compileLibrary(query: String): Unit = {
    throw new XPathException(
      "Separate compilation of query libraries requires Saxon-EE")
  }

  def compileLibrary(source: Reader): Unit = {
    throw new XPathException(
      "Separate compilation of query libraries requires Saxon-EE")
  }

  def compileLibrary(source: InputStream, encoding: String): Unit = {
    throw new UnsupportedOperationException(
      "Separate compilation of query libraries requires Saxon-EE")
  }

  def getCompiledLibrary(namespace: String): QueryLibrary = null

  def getCompiledLibraries(): Collection[QueryLibrary] = Collections.emptySet()

  def declareNamespace(prefix: String, uri: String): Unit = {
    if (prefix == null) {
      throw new NullPointerException(
        "Null prefix supplied to declareNamespace()")
    }
    if (uri == null) {
      throw new NullPointerException(
        "Null namespace URI supplied to declareNamespace()")
    }
    if (prefix.==("xml") != uri == NamespaceConstant.XML) {
      throw new IllegalArgumentException("Misdeclaration of XML namespace")
    }
    if (prefix.==("xmlns") || uri == NamespaceConstant.XMLNS) {
      throw new IllegalArgumentException("Misdeclaration of xmlns namespace")
    }
    if (prefix.isEmpty) {
      default_ElementNamespace = uri
    }
    if (uri.isEmpty) {
      userDeclaredNamespaces.remove(prefix)
    } else {
      userDeclaredNamespaces.put(prefix, uri)
    }
  }

  def clearNamespaces(): Unit = {
    userDeclaredNamespaces.clear()
    declareNamespace("xml", NamespaceConstant.XML)
    declareNamespace("xs", NamespaceConstant.SCHEMA)
    declareNamespace("xsi", NamespaceConstant.SCHEMA_INSTANCE)
    declareNamespace("fn", NamespaceConstant.FN)
    declareNamespace("local", NamespaceConstant.LOCAL)
    declareNamespace("err", NamespaceConstant.ERR)
    declareNamespace("saxon", NamespaceConstant.SAXON)
    declareNamespace("", "")
  }

  def iterateDeclaredPrefixes(): Iterator[String] =
    userDeclaredNamespaces.keySet.iterator()

  def getNamespaceForPrefix(prefix: String): String =
    userDeclaredNamespaces.get(prefix)

  def setDefaultElementNamespace(uri: String): Unit = {
    default_ElementNamespace = uri
    declareNamespace("", uri)
  }

  def declareGlobalVariable(qName: StructuredQName,
                            `type`: SequenceType,
                            value: Sequence,
                            external: Boolean): Unit = {
    if (value == null && !external) {
      throw new NullPointerException("No initial value for declared variable")
    }
    if (value != null &&
      !`type`.matches(value, getConfiguration.getTypeHierarchy)) {
      throw new XPathException(
        "Value of declared variable does not match its type")
    }
    val `var`: GlobalVariable =
      if (external) new GlobalParam() else new GlobalVariable()
    `var`.setVariableQName(qName)
    `var`.setRequiredType(`type`)
    if (value != null) {
      `var`.setBody(Literal.makeLiteral(value.materialize()))
    }
    if (userDeclaredVariables == null) {
      userDeclaredVariables = new HashSet()
    }
    userDeclaredVariables.add(`var`)
  }

  def iterateDeclaredGlobalVariables(): Iterator[GlobalVariable] =
    if (userDeclaredVariables == null) {
      val empty: List[GlobalVariable] = Collections.emptyList()
      empty.iterator()
    } else {
      userDeclaredVariables.iterator()
    }

  def clearDeclaredGlobalVariables(): Unit = {
    userDeclaredVariables = null
  }

  def declareDefaultCollation(name: String): Unit = {
    if (name == null) {
      throw new NullPointerException()
    }
    var c: StringCollator = null
    try c = getConfiguration.getCollation(name)
    catch {
      case e: XPathException => c = null

    }
    if (c == null) {
      throw new IllegalStateException("Unknown collation " + name)
    }
    this.defaultCollationName = name
  }

  def getSystemId(): String = baseURI

  def setPreserveBoundarySpace(preserve: Boolean): Unit = {
    preserveSpace = preserve
  }

  def isPreserveBoundarySpace(): Boolean = preserveSpace

  def setEmptyLeast(least: Boolean): Unit = {
    defaultEmptyLeast = least
  }

  def isEmptyLeast(): Boolean = defaultEmptyLeast

  def setErrorListener(listener: ErrorListener): Unit = {
    this.errorReporter = new ErrorReporterToListener(listener)
  }

  def getErrorListener(): ErrorListener =
    if (errorReporter.isInstanceOf[ErrorReporterToListener]) {
      errorReporter.asInstanceOf[ErrorReporterToListener].getErrorListener
    } else {
      null
    }

  def setUpdatingEnabled(updating: Boolean): Unit = {
    isUpdating = updating
  }

  def isUpdatingEnabled(): Boolean = isUpdating

}
