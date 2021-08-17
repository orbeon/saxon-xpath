package org.orbeon.saxon.utils

import java.io.{InputStream, PrintStream, UnsupportedEncodingException}
import java.net.{URI, URISyntaxException, URLDecoder}
import java.{util => ju}
import java.util
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.function.{IntPredicate, Predicate}
import java.util.{Collections, Comparator, Properties}

import javax.xml.transform._
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.event._
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.accum.AccumulatorRegistry
import org.orbeon.saxon.expr.compat.TypeChecker10
import org.orbeon.saxon.expr.instruct._
import org.orbeon.saxon.expr.number.Numberer_en
import org.orbeon.saxon.expr.parser.XPathParser.ParsedLanguage
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.{AlphanumericCollator, CodepointCollator, HTML5CaseBlindCollator}
import org.orbeon.saxon.functions._
import org.orbeon.saxon.functions.registry._
import org.orbeon.saxon.lib._
import org.orbeon.saxon.ma.arrays.ArrayFunctionSet
import org.orbeon.saxon.ma.map.{MapFunctionSet, MapItem}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om
import org.orbeon.saxon.om.{AllElementsSpaceStrippingRule, DocumentPool, FocusTrackingIterator, GroundedValue, IgnorableSpaceStrippingRule, NamePool, NoElementsSpaceStrippingRule, NodeInfo, NotationSet, Sequence, SequenceIterator, StructuredQName, TreeInfo, TreeModel}
import org.orbeon.saxon.pattern.PatternParser30
import org.orbeon.saxon.pull.PullSource
import org.orbeon.saxon.query.{QueryModule, StaticQueryContext, XQueryExpression, XQueryParser}
import org.orbeon.saxon.regex.RegularExpression
import org.orbeon.saxon.resource._
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.sapling.SaplingDocument
import org.orbeon.saxon.serialize.SerializationProperties
import org.orbeon.saxon.serialize.charcode.{CharacterSetFactory, XMLCharacterData}
import org.orbeon.saxon.sxpath.IndependentContext
import org.orbeon.saxon.trace.TraceCodeInjector
import org.orbeon.saxon.trans.FunctionStreamability.FunctionStreamability
import org.orbeon.saxon.trans._
import org.orbeon.saxon.tree.tiny.TreeStatistics
import org.orbeon.saxon.tree.util.DocumentNumberAllocator
import org.orbeon.saxon.utils.Configuration.ApiProvider
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.{IntHashSet, IntSet}
import org.xml.sax._

import scala.jdk.CollectionConverters._


object Configuration {

  val booleanFeatures = new util.HashSet[Feature[_]](40)
  /**
   * Constant indicating the XML Version 1.0
   */
  val XML10 = 10
  /**
   * Constant indicating the XML Version 1.1
   */
  val XML11 = 11
  /**
   * Language versions for XML Schema
   */
  val XSD10 = 10
  val XSD11 = 11

  /**
   * Factory method to create a Configuration, of the class defined using conditional
   * compilation tags when compiling this version of Saxon: that is,
   * the type of Configuration appropriate to the edition of the software
   * being used. This method does not check that the Configuration is licensed.
   *
   * @return a Configuration object of the class appropriate to the Saxon edition in use.
   * @since 9.2
   */
  def newConfiguration: Configuration =
    new Configuration

  /**
   * Read a resource file issued with the Saxon product
   *
   * @param filename the filename of the file to be read
   * @param messages List to be populated with messages in the event of failure
   * @return an InputStream for reading the file/resource
   */
  def locateResource(filename: String, messages: util.List[String]): InputStream = {

    // ORBEON: JVM only
    null
//
//    val fileName: String = "net/sf/saxon/data/" + filename
//    var loader: ClassLoader = null
//    try loader = Thread.currentThread.getContextClassLoader
//    catch {
//      case _: Exception =>
//        messages.add("Failed to getContextClassLoader() - continuing\n")
//    }
//    var in: InputStream = null
//    if (loader != null) {
//      in = loader.getResourceAsStream(fileName)
//      if (in == null)
//        messages.add("Cannot read " + fileName + " file located using ClassLoader " + loader + " - continuing\n")
//    }
//    if (in == null) {
//      loader = classOf[Configuration].getClassLoader
//      if (loader != null) {
//        in = loader.getResourceAsStream(fileName)
//        if (in == null)
//          messages.add("Cannot read " + fileName + " file located using ClassLoader " + loader + " - continuing\n")
//      }
//    }
//    if (in == null) { // Means we're in a very strange class-loading environment, things are getting desperate
//      val url = ClassLoader.getSystemResource(fileName)
//      if (url != null)
//        try in = url.openStream
//        catch {
//          case ioe: IOException =>
//            messages.add("IO error " + ioe.getMessage + " reading " + fileName + " located using getSystemResource(): using defaults")
//            in = null
//        }
//    }
//    in
  }

  // ORBEON: No callers.
//  /**
//   * Read a resource file issued with the Saxon product, returning a StreamSource with bound systemId
//   * This means it can be an XSLT stylesheet which uses inclusions etc.
//   *
//   * @param filename the filename of the file to be read
//   * @param messages List to be populated with messages in the event of failure
//   * @param loaders  List to be populated with the ClassLoader that succeeded in loading the resource
//   * @return a StreamSource for reading the file/resource, with URL set appropriately
//   */
//  def locateResourceSource(filename: String, messages: util.List[String], loaders: util.List[ClassLoader]): StreamSource = {
//    var loader: ClassLoader = null
//    try loader = Thread.currentThread.getContextClassLoader
//    catch {
//      case err: Exception =>
//        messages.add("Failed to getContextClassLoader() - continuing\n")
//    }
//    var in: InputStream = null
//    var url: java.net.URL = null
//    if (loader != null) {
//      url = loader.getResource(filename)
//      in = loader.getResourceAsStream(filename)
//      if (in == null) messages.add("Cannot read " + filename + " file located using ClassLoader " + loader + " - continuing\n")
//    }
//    if (in == null) {
//      loader = classOf[Configuration].getClassLoader
//      if (loader != null) {
//        in = loader.getResourceAsStream(filename)
//        if (in == null) messages.add("Cannot read " + filename + " file located using ClassLoader " + loader + " - continuing\n")
//      }
//    }
//    loaders.add(loader)
//    new StreamSource(in, url.toString)
//  }

//  /**
//   * Factory method to construct a Configuration object by reading a configuration file.
//   *
//   * @param source Source object containing the configuration file
//   * @return the resulting Configuration
//   */
//  @throws[XPathException]
//  def readConfiguration(source: Source) = {
//    val tempConfig = newConfiguration
//    tempConfig.readConfigurationFile(source)
//  }
//
//  /**
//   * Factory method to construct a Configuration object by reading a configuration file.
//   * This version of the method creates a configuration that is "compatible" with the
//   * supplied configuration, in that it shares the same NamePool and DocumentNumberAllocator.
//   * (This is used by fn:transform)
//   *
//   * @param source            Source object containing the configuration file
//   * @param baseConfiguration an existing configuration whose NamePool and DocumentNumberAllocator
//   *                          will be used in the new Configuration; the license details from
//   *                          the base configuration will also be shared
//   * @return the resulting Configuration
//   */
//  @throws[XPathException]
//  def readConfiguration(source: Source, baseConfiguration: Configuration): Configuration = {
//    val tempConfig = newConfiguration
//    tempConfig.readConfigurationFile(source, baseConfiguration)
//  }

//  /**
//   * Instantiate a Configuration object with a given class name
//   *
//   * @param className   the class name
//   * @param classLoader the class loader to be used
//   * @return a Configuration of the required class
//   * @throws ClassNotFoundException if the class is not found
//   * @throws InstantiationException if the class cannot be instantiated
//   * @throws IllegalAccessException if the class is not accessible
//   */
//  @throws[ClassNotFoundException]
//  @throws[InstantiationException]
//  @throws[IllegalAccessException]
//  def instantiateConfiguration(className: String, classLoader: ClassLoader): Configuration = {
//    var theClass: Class[_] = null
//    var loader: ClassLoader = classLoader
//    if (loader == null) {
//      try loader = Thread.currentThread().getContextClassLoader
//      catch {
//        case err: Exception =>
//          System.err.println("Failed to getContextClassLoader() - continuing")
//
//      }
//    }
//    if (loader != null) {
//      try theClass = loader.loadClass(className)
//      catch {
//        case ex: Exception => theClass = Class.forName(className)
//
//      }
//    } else {
//      theClass = Class.forName(className)
//    }
//    theClass.newInstance().asInstanceOf[Configuration]
//  }

  /**
   * Ask if Java is being run with assertions enabled (-ea option)
   *
   * @return true if the -ea option is set
   */
  def isAssertionsEnabled: Boolean = {
    // Highly devious logic here. If assertions are enabled, the assertion is false, and a deliberate side-effect
    // of evaluating the assertion is that assertsEnabled is set to true. If assertions are not enabled, the assert
    // statement is not executed, so assertsEnabled is left as false.
    var assertsEnabled = false
    //noinspection AssertWithSideEffects

    assert({
      assertsEnabled = true
      assertsEnabled
    })
    assertsEnabled
  }

  // ORBEON: No callers.
//  /**
//   * Static method to instantiate a professional or enterprise configuration.
//   * <p>This method fails if the specified configuration class cannot be loaded,
//   * but it does not check whether there is a license available.
//   *
//   * @param classLoader - the class loader to be used. If null, the context class loader for the current
//   *                    thread is used.
//   * @param className   - the name of the configuration class. Defaults to
//   *                    "com.saxonica.config.ProfessionalConfiguration" if null is supplied. This allows an assembly
//   *                    qualified name to be supplied under .NET. The class, once instantiated, must be an instance
//   *                    of Configuration.
//   * @return the new ProfessionalConfiguration or EnterpriseConfiguration
//   * @throws RuntimeException if the required Saxon edition cannot be loaded
//   * @since 9.2 (renamed from makeSchemaAwareConfiguration)
//   */
//  @throws[RuntimeException]
//  def makeLicensedConfiguration(classLoader: ClassLoader, className: String): Configuration = {
//    var classnameVar = className
//    if (classnameVar == null) classnameVar = "com.saxonica.config.ProfessionalConfiguration"
//    try instantiateConfiguration(classnameVar, classLoader)
//    catch {
//      case e@(_: ClassNotFoundException | _: InstantiationException | _: IllegalAccessException) =>
//        throw new RuntimeException(e)
//    }
//  }

  /**
   * Marker interface to represent an API that is provided as a layer on top of this
   * {@code Configuration}
   */
  trait ApiProvider

  /**
   * Get a parser by instantiating the SAXParserFactory
   *
   * @return the parser (XMLReader)
   */
  private def loadParser: XMLReader = Version.platform.loadParser()

  /**
   * Get the configuration, given the context. This is provided as a static method to make it accessible
   * as an extension function.
   *
   * @param context the XPath dynamic context
   * @return the Saxon Configuration for a given XPath dynamic context
   */
  def getConfiguration(context: XPathContext): Configuration = context.getConfiguration

  /**
   * Validate a property value where the required type is boolean
   *
   * @param propertyName the name of the property
   * @param value        the supplied value of the property. This may be either a java.lang.Boolean, or a string
   *                     taking one of the values on|off, true|false, yes|no, or 1|0 (suited to the conventions of different
   *                     configuration APIs that end up calling this method)
   * @return the value as a boolean
   * @throws IllegalArgumentException if the supplied value cannot be validated as a recognized boolean value
   */
  def requireBoolean(propertyName: String, value: Any): Boolean = if (value.isInstanceOf[Boolean]) value.asInstanceOf[Boolean]
  else if (value.isInstanceOf[String]) {
    val valueStr = value.asInstanceOf[String].trim
    if ("true" == valueStr || "on" == valueStr || "yes" == valueStr || "1" == valueStr) true
    else if ("false" == valueStr || "off" == valueStr || "no" == valueStr || "0" == valueStr) false
    else throw new IllegalArgumentException(propertyName + " must be 'true' or 'false' (or on|off, yes|no, 1|0)")
  }
  else throw new IllegalArgumentException(propertyName + " must be a boolean (or a string representing a boolean)")

  /**
   * This class contains constants representing features of the software that may or may
   * not be licensed. (Note, this list is at a finer-grained level than the actual
   * purchasing options.)
   */
  object LicenseFeature {
    val SCHEMA_VALIDATION = 1
    val ENTERPRISE_XSLT = 2
    val ENTERPRISE_XQUERY = 4
    val PROFESSIONAL_EDITION = 8
  }

  booleanFeatures.add(Feature.ALLOW_EXTERNAL_FUNCTIONS)
  booleanFeatures.add(Feature.ALLOW_MULTITHREADING)
  booleanFeatures.add(Feature.ALLOW_SYNTAX_EXTENSIONS)
  booleanFeatures.add(Feature.ASSERTIONS_CAN_SEE_COMMENTS)
  booleanFeatures.add(Feature.COMPILE_WITH_TRACING)
  booleanFeatures.add(Feature.DEBUG_BYTE_CODE)
  booleanFeatures.add(Feature.DISABLE_XSL_EVALUATE)
  booleanFeatures.add(Feature.DISPLAY_BYTE_CODE)
  booleanFeatures.add(Feature.DTD_VALIDATION)
  booleanFeatures.add(Feature.EAGER_EVALUATION)
  booleanFeatures.add(Feature.EXPAND_ATTRIBUTE_DEFAULTS)
  booleanFeatures.add(Feature.EXPATH_FILE_DELETE_TEMPORARY_FILES)
  booleanFeatures.add(Feature.GENERATE_BYTE_CODE)
  booleanFeatures.add(Feature.IGNORE_SAX_SOURCE_PARSER)
  booleanFeatures.add(Feature.IMPLICIT_SCHEMA_IMPORTS)
  booleanFeatures.add(Feature.MARK_DEFAULTED_ATTRIBUTES)
  booleanFeatures.add(Feature.MONITOR_HOT_SPOT_BYTE_CODE)
  booleanFeatures.add(Feature.MULTIPLE_SCHEMA_IMPORTS)
  booleanFeatures.add(Feature.PRE_EVALUATE_DOC_FUNCTION)
  booleanFeatures.add(Feature.PREFER_JAXP_PARSER)
  booleanFeatures.add(Feature.RETAIN_DTD_ATTRIBUTE_TYPES)
  booleanFeatures.add(Feature.STABLE_COLLECTION_URI)
  booleanFeatures.add(Feature.STABLE_UNPARSED_TEXT)
  booleanFeatures.add(Feature.STREAMING_FALLBACK)
  booleanFeatures.add(Feature.STRICT_STREAMABILITY)
  booleanFeatures.add(Feature.SUPPRESS_EVALUATION_EXPIRY_WARNING)
  booleanFeatures.add(Feature.SUPPRESS_XPATH_WARNINGS)
  booleanFeatures.add(Feature.SUPPRESS_XSLT_NAMESPACE_CHECK)
  booleanFeatures.add(Feature.TRACE_EXTERNAL_FUNCTIONS)
  booleanFeatures.add(Feature.TRACE_OPTIMIZER_DECISIONS)
  booleanFeatures.add(Feature.USE_PI_DISABLE_OUTPUT_ESCAPING)
  booleanFeatures.add(Feature.USE_TYPED_VALUE_CACHE)
  booleanFeatures.add(Feature.XQUERY_MULTIPLE_MODULE_IMPORTS)
  booleanFeatures.add(Feature.RETAIN_NODE_FOR_DIAGNOSTICS)
  booleanFeatures.add(Feature.ALLOW_UNRESOLVED_SCHEMA_COMPONENTS)
}


/**
 * Create a non-schema-aware configuration object with default settings for all options.
 *
 * @since 8.4
 */

class Configuration extends SourceResolver with NotationSet {
  @transient private var apiProcessor: ApiProvider = null
  @transient private var characterSetFactory: CharacterSetFactory = _
  private val collationMap: util.Map[String, StringCollator] = new util.HashMap(10)
  private var collationResolver: CollationURIResolver = new StandardCollationURIResolver
  private var defaultCollationName: String = NamespaceConstant.CODEPOINT_COLLATION_URI
  private var allowedUriTest: Predicate[java.net.URI] = (uri: java.net.URI) => true
  private val standardCollectionFinder: StandardCollectionFinder = new StandardCollectionFinder()
  private var collectionFinder: CollectionFinder = standardCollectionFinder
  private var environmentVariableResolver: EnvironmentVariableResolver = new StandardEnvironmentVariableResolver()
  private var defaultCollection: String = null
  private val defaultParseOptions: ParseOptions = new ParseOptions()
  @transient  var defaultStaticQueryContext: StaticQueryContext = _
  private var staticQueryContextFactory: StaticQueryContextFactory = new StaticQueryContextFactory()
  var optimizerOptions: OptimizerOptions = OptimizerOptions.FULL_HE_OPTIMIZATION

  //var defaultXsltCompilerInfo: CompilerInfo = makeCompilerInfo

  private var errorReporterFactory: Configuration => ErrorReporter =
    (config: Configuration) => {
      val reporter = new StandardErrorReporter
      reporter.setLogger(config.getLogger)
      reporter
    }

  private var label: String = null

  private var documentNumberAllocator: DocumentNumberAllocator =
    new DocumentNumberAllocator()

  /*@Nullable*/
  @transient private var debugger: Debugger = null
  private var defaultLanguage: String = "en" // Locale.getDefault.getLanguage
  private var defaultCountry: String = "us" // Locale.getDefault.getCountry
  private var defaultOutputProperties: Properties = new Properties()
  @transient private val dynamicLoader: DynamicLoader = new DynamicLoader
  private val enabledProperties: IntSet = new IntHashSet(64)
  private var externalObjectModels: util.List[ExternalObjectModel] = new util.ArrayList[ExternalObjectModel](4)
  private val globalDocumentPool: DocumentPool = new DocumentPool()
  private val integratedFunctionLibrary: IntegratedFunctionLibrary = new IntegratedFunctionLibrary()
  @transient private var localizerFactory: LocalizerFactory = _
  private var namePool: NamePool = new NamePool()
  var optimizer: Optimizer = null
  private var serializerFactory: SerializerFactory = new SerializerFactory(this)
  @volatile private var sourceParserPool: ConcurrentLinkedQueue[XMLReader] = new ConcurrentLinkedQueue()
  @volatile private var styleParserPool: ConcurrentLinkedQueue[XMLReader] = new ConcurrentLinkedQueue()
  private var sourceParserClass: String = _
  @transient private var sourceResolver: SourceResolver = this
  @transient private var traceOutput: Logger = new StandardLogger
  private val standardModuleURIResolver: ModuleURIResolver = Version.platform.makeStandardModuleURIResolver(this)
  private var styleParserClass: String = _
  private val systemURIResolver: StandardURIResolver = new StandardURIResolver(this)
  private var unparsedTextURIResolver: UnparsedTextURIResolver = new StandardUnparsedTextResolver()
  @transient private var theConversionContext: XPathContext = null
  private var theConversionRules: ConversionRules = null
  @transient private var traceListener: TraceListener = null
  private var traceListenerClass: String = null
  private var traceListenerOutput: String = null
  private var defaultRegexEngine: String = "S"
  @transient  var typeHierarchy: TypeHierarchy = _
  private val typeChecker: TypeChecker = new TypeChecker()
  private val typeChecker10: TypeChecker10 = new TypeChecker10()
  @transient private var uriResolver: URIResolver = _
  var builtInExtensionLibraryList: FunctionLibraryList = _
  var xsdVersion: Int = Configuration.XSD11
  private var xmlVersion: Int = Configuration.XML10
  private var xpathVersionForXsd: Int = 20
  private var xpathVersionForXslt: Int = 31
  // Plug-in to allow media queries in an xml-stylesheet processing instruction to be evaluated
  private var mediaQueryEvaluator: Comparator[String] = (o1: String, o2: String) => 0
  private val fileExtensions: util.Map[String, String] = new util.HashMap()
  private val resourceFactoryMapping: util.Map[String, ResourceFactory] = new util.HashMap()
  private val functionAnnotationHandlers: util.Map[String, FunctionAnnotationHandler] = new util.HashMap()
  val byteCodeThreshold: Int = 100
  private var regexBacktrackingLimit: Int = 10000000
  private val treeStatistics: TreeStatistics = new TreeStatistics()

//  /**
//   * Read the configuration file an construct a new Configuration (the real one)
//   *
//   * @param source the source of the configuration file
//   * @return the Configuration that will be used for real work
//   * @throws XPathException if the configuration file cannot be read or is invalid
//   */
//  @throws[XPathException]
//   def readConfigurationFile(source: Source): Configuration = new ConfigurationReader().makeConfiguration(source)
//
//  @throws[XPathException]
//   def readConfigurationFile(source: Source, baseConfiguration: Configuration): Configuration = {
//    val reader = makeConfigurationReader
//    reader.setBaseConfiguration(baseConfiguration)
//    reader.makeConfiguration(source)
//  }
//
//   def makeConfigurationReader = new ConfigurationReader

   locally {
    Version.platform.initialize(this)
    //defaultXsltCompilerInfo.setURIResolver(getSystemURIResolver)
    val resolver = new StandardEntityResolver(this)
    defaultParseOptions.setEntityResolver(resolver)
    internalSetBooleanProperty(Feature.PREFER_JAXP_PARSER, true)
    internalSetBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS, true)
    internalSetBooleanProperty(Feature.DISABLE_XSL_EVALUATE, false)
    //internalSetBooleanProperty(FeatureKeys.STABLE_COLLECTION_URI, true);

    registerFileExtension("xml", "application/xml")
    registerFileExtension("html", "application/html")
    registerFileExtension("atom", "application/atom")
    registerFileExtension("xsl", "application/xml+xslt")
    registerFileExtension("xslt", "application/xml+xslt")
    registerFileExtension("xsd", "application/xml+xsd")
    registerFileExtension("txt", "text/plain")
    registerFileExtension("MF", "text/plain")
    registerFileExtension("class", "application/java")
    registerFileExtension("json", "application/json")
    registerFileExtension("", "application/unknown")
    registerMediaType("application/xml", XmlResource.FACTORY)
    registerMediaType("text/xml", XmlResource.FACTORY)
    registerMediaType("application/html", XmlResource.FACTORY)
    registerMediaType("text/html", XmlResource.FACTORY)
    registerMediaType("application/atom", XmlResource.FACTORY)
    registerMediaType("application/xml+xslt", XmlResource.FACTORY)
    registerMediaType("application/xml+xsd", XmlResource.FACTORY)
    registerMediaType("application/rdf+xml", XmlResource.FACTORY)
    registerMediaType("text/plain", UnparsedTextResource.FACTORY)
    registerMediaType("application/java", BinaryResource.FACTORY)
    registerMediaType("application/binary", BinaryResource.FACTORY)
    registerMediaType("application/json", JSONResource.FACTORY)
    registerMediaType("application/unknown", UnknownResource.FACTORY)
    registerFunctionAnnotationHandler(new XQueryFunctionAnnotationHandler)
  }

  def importLicenseDetails(config: Configuration): Unit = {
    // No action for an HE configuration
  }

  /**
   * Get the edition code identifying this configuration: "HE", "PE" or "EE"
   *
   * @return the code identifying the Saxon edition associated with this configuration
   */
  def getEditionCode = "HE"

  /**
   * Save the ApiProvider object that owns this {@code Configuration} in the relevant API.
   * <p>Note: it is possible to use multiple APIs over the same {@code Configuration}. This mechanism
   * is only capable of holding one of these, and is therefore only really useful in cases where
   * the API in use is homogeneous.</p>
   *
   * @param processor This can be any ApiProvider, but it is typically one of the
   *                  following:
   *                  <ul>
   *                  <li>When using the Java s9api interface, the <code>org.orbeon.saxon.s9api.Processor</code></li>
   *                  <li>When using the .NET interface, the <code>Saxon.Api.Processor</code></li>
   *                  <li>When using the JAXP transformation interface, the JAXP <code>TransformerFactory</code></li>
   *                  <li>When using the JAXP XPath interface, the JAXP <code>XPathFactory</code></li>
   *                  <li>When using the JAXP Schema interface, the JAXP <code>SchemaFactory</code></li>
   *                  <li>When using XQJ, the <code>XQDataSource</code></li>
   *                  </ul>
   * @since 9.2. Changed in 9.9 to require an { @link ApiProvider} rather than any Object.
   */
  def setProcessor(processor: Configuration.ApiProvider): Unit = this.apiProcessor = processor

  /**
   * Get the Processor object that created this Configuration in the relevant API.
   * <p>The main purpose of this interface is to allow extension functions called from
   * a stylesheet or query to get access to the originating processor. This is particularly
   * useful when methods are static as there is then limited scope for passing data from the
   * calling application to the code of the extension function.</p>
   *
   * @return the processor that was supplied to the { @link #setProcessor(ApiProvider)} method, or null
   *         if this method has not been called. In practice this property is used to hold one of the
   *         following:
   *         <ul>
   *         <li>When using the Java s9api interface, the <code>org.orbeon.saxon.s9api.Processor</code></li>
   *         <li>When using the .NET interface, the <code>Saxon.Api.Processor</code></li>
   *         <li>When using the JAXP transformation interface, the JAXP <code>TransformerFactory</code></li>
   *         <li>When using the JAXP XPath interface, the JAXP <code>XPathFactory</code></li>
   *         <li>When using the JAXP Schema interface, the JAXP <code>SchemaFactory</code></li>
   *         <li>When using XQJ, the <code>XQDataSource</code></li>
   *         </ul>
   * @since 9.2. Changed in 9.9 to return an { @link ApiProvider} rather than any Object.
   */
  def getProcessor: ApiProvider = apiProcessor

  /**
   * Get a message used to identify this product when a transformation is run using the -t option
   *
   * @return A string containing both the product name and the product
   *         version
   * @since 8.4
   */
  def getProductTitle: String =
    "Saxon-" + getEditionCode + " " + Version.getProductVersion + Version.platform.getPlatformSuffix + " from Saxonica"

  @throws[LicenseException]
  def checkLicensedFeature(feature: Int, name: String, localLicenseId: Int): Unit = {
    val require = if (feature == Configuration.LicenseFeature.PROFESSIONAL_EDITION) "PE"
    else "EE"
    val message = "Requested feature (" + name + ") requires Saxon-" + require
    if (!Version.softwareEdition.equals("HE")) message + ". You are using Saxon-" + Version.softwareEdition +
      " software, but the Configuration is an instance of " + getClass.getName + "; to use this feature you need to create an instance of " +
      (if (feature == Configuration.LicenseFeature.PROFESSIONAL_EDITION) "com.saxonica.config.ProfessionalConfiguration"
      else "com.saxonica.config.EnterpriseConfiguration")
    throw new LicenseException(message, LicenseException.WRONG_CONFIGURATION)
  }

  /**
   * Instruct Saxon to run as if no license is available. This method is provided for testing purposes,
   * so that tests with and without a license can be run without making changes to the classpath.
   */
  def disableLicensing(): Unit = {
  }

  /**
   * Ask whether a particular feature is enabled by an embedded license (embedded in a SEF file)
   *
   * @param localLicenseId the identifier of the embedded license
   * @param feature        the feature in question, identified by a constant in class { @link org.orbeon.saxon.Configuration.LicenseFeature}
   * @return true if the embedded license exists and enables the requested feature
   */
  def isFeatureAllowedBySecondaryLicense(localLicenseId: Int, feature: Int): Boolean = false

  /**
   * Determine if a particular feature is licensed.
   *
   * @param feature the feature in question, identified by a constant in class { @link org.orbeon.saxon.Configuration.LicenseFeature}
   * @return true if the feature is licensed, false if it is not.
   */
  def isLicensedFeature(feature: Int): Boolean = false

  /**
   * Get the value of a named license feature
   *
   * @param name the name of the feature
   * @return the value of the feature if present, or null otherwise
   */
  def getLicenseFeature(name: String): String = null

  /**
   * Display a message about the license status
   */
  def displayLicenseMessage(): Unit = {
  }

  /**
   * Register a local license file (for use within a single transformation (etc))
   *
   * @param dmk the license in encoded form
   * @return an integer identifying this license uniquely within the configuration, or -1 if not accepted
   */
  def registerLocalLicense(dmk: String): Int = -1

//  /**
//   * Set the DynamicLoader to be used. By default an instance of {@link DynamicLoader} is used
//   * for all dynamic loading of Java classes. This method allows the actions of the standard
//   * DynamicLoader to be overridden
//   *
//   * @param dynamicLoader the DynamicLoader to be used by this Configuration
//   */
//  def setDynamicLoader(dynamicLoader: DynamicLoader): Unit = this.dynamicLoader = dynamicLoader

//  /**
//   * Get the DynamicLoader used by this Configuration. By default the standard system-supplied
//   * dynamic loader is returned.
//   *
//   * @return the DynamicLoader in use - either a user-supplied DynamicLoader, or the standard one
//   *         supplied by the system.
//   */
//  def getDynamicLoader: DynamicLoader = dynamicLoader

  // ORBEON
  def getResourceAsStream(name: String): InputStream =
    dynamicLoader.getResourceAsStream(name)

  /**
   * Load a class using the class name provided.
   * Note that the method does not check that the object is of the right class.
   * <p>This method is intended for internal use only. The call is delegated to the
   * <code>DynamicLoader</code>, which may be overridden by a user-defined <code>DynamicLoader</code>.</p>
   *
   * @param className   A string containing the name of the
   *                    class, for example "com.microstar.sax.LarkDriver"
   * @param tracing     true if diagnostic tracing is required
   * @return an instance of the class named, or null if it is not
   *         loadable.
   * @throws XPathException if the class cannot be loaded.
   */
  @throws[XPathException]
  def getConfClass(className: String, tracing: Boolean): Class[_] =
    dynamicLoader.getClass(className, if (tracing) traceOutput else null)

  /**
   * Instantiate a class using the class name provided.
   * Note that the method does not check that the object is of the right class.
   * <p>This method is intended for internal use only. The call is delegated to the
   * <code>DynamicLoader</code>, which may be overridden by a user-defined <code>DynamicLoader</code>.</p>
   * <p>Diagnostic output is produced if the option "isTiming" is set (corresponding to the -t option on
   * the command line).</p>
   *
   * @param className   A string containing the name of the
   *                    class, for example "com.microstar.sax.LarkDriver"
   * @return an instance of the class named, or null if it is not
   *         loadable.
   * @throws XPathException if the class cannot be loaded.
   */
  @throws[XPathException]
  def getInstance(className: String): Any = dynamicLoader.getInstance(className, if (isTiming) traceOutput else null)

  /**
   * Set a Predicate that is applied to a URI to determine whether the standard resource resolvers
   * ({@link URIResolver}, {@link UnparsedTextURIResolver}, {@link SchemaURIResolver},
   * {@link CollationURIResolver}, {@link ModuleURIResolver}) should accept it.
   *
   * <p>It is possible to set a predicate by means of the configuration property
   * {@link Feature#ALLOWED_PROTOCOLS}. This method, however, allows an arbitrary predicate to
   * be supplied.</p>
   *
   * <p>The predicate is only applicable to resolvers that choose to use it. This includes
   * all the standard Saxon-supplied resolvers, but user-supplied resolvers can bypass this
   * check.</p>
   *
   * @param test a condition that a URI must satisfy if access to a resource with this URI
   *             is to be permitted
   */
  def setAllowedUriTest(test: Predicate[URI]): Unit = this.allowedUriTest = test

  /**
   * Get the Predicate that is applied to a URI to determine whether the standard resource resolvers
   * ({@link URIResolver}, {@link UnparsedTextURIResolver}, {@link SchemaURIResolver},
   * {@link CollationURIResolver}, {@link ModuleURIResolver}) should accept it.
   *
   * <p>It is possible to set a predicate by means of the configuration property
   * {@link Feature#ALLOWED_PROTOCOLS}.</p>
   *
   * <p>The predicate is only applicable to resolvers that choose to use it. This includes
   * all the standard Saxon-supplied resolvers, but user-supplied resolvers can bypass this
   * check.</p>
   *
   * @return a condition that a URI must satisfy if access to a resource with this URI
   *         is to be permitted
   */
  def getAllowedUriTest: java.util.function.Predicate[URI] = this.allowedUriTest

  /**
   * Get the URIResolver used in this configuration
   *
   * @return the URIResolver. If no URIResolver has been set explicitly, the
   *         default URIResolver is used.
   * @since 8.4
   */
  def getURIResolver: URIResolver = {
    if (uriResolver == null) return systemURIResolver
    uriResolver
  }

  /**
   * Set the URIResolver to be used in this configuration. This will be used to
   * resolve the URIs used statically (e.g. by xsl:include) and also the URIs used
   * dynamically by functions such as document() and doc(). Note that the URIResolver
   * does not resolve the URI in the sense of RFC 2396 (which is also the sense in which
   * the resolve-uri() function uses the term): rather it dereferences an absolute URI
   * to obtain an actual resource, which is returned as a Source object.
   *
   * @param resolver The URIResolver to be used.
   * @since 8.4
   */
  def setURIResolver(resolver: URIResolver): Unit = {
    uriResolver = resolver
    if (resolver.isInstanceOf[StandardURIResolver]) resolver.asInstanceOf[StandardURIResolver].setConfiguration(this)
    //defaultXsltCompilerInfo.setURIResolver(resolver)
  }

  /**
   * Set the URIResolver to a URI resolver that allows query parameters after the URI,
   * and in the case of Saxon-EE, that inteprets the file extension .ptree
   */
  def setParameterizedURIResolver(): Unit = getSystemURIResolver.setRecognizeQueryParameters(true)

  /**
   * Get the system-defined URI Resolver. This is used when the user-defined URI resolver
   * returns null as the result of the resolve() method
   *
   * @return the system-defined URI resolver
   */
  def getSystemURIResolver: StandardURIResolver = systemURIResolver

  /**
   * Create an instance of a URIResolver with a specified class name.
   * Note that this method does not register the URIResolver with this Configuration.
   *
   * @param className The fully-qualified name of the URIResolver class
   * @return The newly created URIResolver
   * @throws TransformerException if the requested class does not
   *                              implement the javax.xml.transform.URIResolver interface
   */
  @throws[TransformerException]
  def makeURIResolver(className: String): URIResolver = {
    val obj = dynamicLoader.getInstance(className, null)
    if (obj.isInstanceOf[StandardURIResolver]) obj.asInstanceOf[StandardURIResolver].setConfiguration(this)
    if (obj.isInstanceOf[URIResolver]) return obj.asInstanceOf[URIResolver]
    throw new XPathException("Class " + className + " is not a URIResolver")
  }

  def setErrorReporterFactory(factory: Configuration => ErrorReporter): Unit =
    errorReporterFactory = factory

  def makeErrorReporter: ErrorReporter = errorReporterFactory(this)

  def getLogger: Logger = traceOutput

  /**
   * Report a fatal error
   *
   * @param err the exception to be reported
   */
  def reportFatalError(err: XPathException): Unit = if (!err.hasBeenReported) {
    makeErrorReporter.report(new XmlProcessingException(err))
    err.setHasBeenReported(true)
  }

  /**
   * Set the standard error output to be used in all cases where no more specific destination
   * is defined. This defaults to System.err.
   *
   * @param out the stream to be used for error output where no more specific destination
   *            has been supplied. The caller is responsible for closing this stream after use
   *            (if necessary).
   * @since 9.3
   */
  def setStandardErrorOutput(out: PrintStream): Unit = {
    if (!traceOutput.isInstanceOf[StandardLogger]) traceOutput = new StandardLogger
    traceOutput.asInstanceOf[StandardLogger].setPrintStream(out)
  }

  /**
   * Register a new logger to be used in the Saxon event logging mechanism
   *
   * @param logger the Logger to be used as default. The caller is responsible for
   *               ensuring that this is closed after use (if necessary), which can
   *               be achieved by calling either { @link Logger#close} or
   *               { @link Configuration#close};
   * @since 9.6
   */
  def setLogger(logger: Logger): Unit = traceOutput = logger

  /**
   * Get the standard error output to be used in all cases where no more specific destination
   * is defined. This defaults to System.err.
   *
   * @return the stream to be used for error output where no more specific destination
   *         has been supplied
   * @since 9.3
   */
  /*@NotNull*/
  def getStandardErrorOutput: PrintStream =
    traceOutput match {
      case standardLogger: StandardLogger => standardLogger.getPrintStream
      case _ => null
    }

  /**
   * Set the XML version to be used by default for validating characters and names.
   * Note that source documents specifying xml version="1.0" or "1.1" are accepted
   * regardless of this setting. The effect of this switch is to change the validation
   * rules for types such as Name and NCName, to change the meaning of \i and \c in
   * regular expressions, and to determine whether the serializer allows XML 1.1 documents
   * to be constructed.
   *
   * @param version one of the constants XML10 or XML11
   * @since 8.6
   */
  def setXMLVersion(version: Int): Unit = {
    xmlVersion = version
    theConversionRules = null
  }

  /**
   * Get the XML version to be used by default for validating characters and names
   *
   * @return one of the constants { @link #XML10} or { @link #XML11}
   * @since 8.6
   */
  def getXMLVersion: Int = xmlVersion

  /**
   * Get the parsing and document building options defined in this configuration
   *
   * @return the parsing and document building options. Note that any changes to this
   *         ParseOptions object will be reflected back in the Configuration; if changes are to be made
   *         locally, the caller should create a copy.
   * @since 9.2
   */
  def getParseOptions: ParseOptions = defaultParseOptions

  /**
   * Set a comparator which can be used to assess whether the media pseudo-attribute
   * in an xml-stylesheet processing instruction matches the media requested in the API
   * for the transformation
   *
   * @param comparator a comparator which returns 0 (equals) when the first argument of the compare
   *                   method is the value of the media attribute in the xml-stylesheet processing
   *                   instruction, and the second argument is the value of the required media given
   *                   in the calling API. The default implementation always returns 0, indicating that
   *                   the media pseudo-attribute is ignored. An alternative implementation, consistent
   *                   with previous Saxon releases, would be compare the strings for equality. A fully
   *                   conformant implementation could implement the syntax and semantics of media queries
   *                   as defined in CSS 3.
   */
  def setMediaQueryEvaluator(comparator: Comparator[String]): Unit = this.mediaQueryEvaluator = comparator

  /**
   * Get a comparator which can be used to assess whether the media pseudo-attribute
   * in an xml-stylesheet processing instruction matches the media requested in the API
   * for the transformation
   *
   * @return a comparator which returns 0 (equals) when the first argument of the compare
   *         method is the value of the media attribute in the xml-stylesheet processing
   *         instruction, and the second argument is the value of the required media given
   *         in the calling API. The default implementation always returns 0, indicating that
   *         the media pseudo-attribute is ignored. An alternative implementation, consistent
   *         with previous Saxon releases, would be compare the strings for equality. A fully
   *         conformant implementation could implement the syntax and semantics of media queries
   *         as defined in CSS 3.
   */
  def getMediaQueryEvaluator: Comparator[String] = mediaQueryEvaluator

  /**
   * Set the conversion rules to be used to convert between atomic types. By default,
   * The rules depend on the versions of XML and XSD in use by the configuration.
   *
   * @param rules the conversion rules to be used
   * @since 9.3
   */
  def setConversionRules(rules: ConversionRules): Unit =
    this.theConversionRules = rules

  /**
   * Get the conversion rules used to convert between atomic types. By default, the rules depend on the versions
   * of XML and XSD in use by the configuration
   *
   * @return the appropriate conversion rules
   * @since 9.3
   */
  def getConversionRules: ConversionRules =
    if (theConversionRules == null) {
      this.synchronized {
        val cv = new ConversionRules
        cv.setTypeHierarchy(getTypeHierarchy)
        cv.setNotationSet(this)
        if (xsdVersion == Configuration.XSD10) {
          cv.setStringToDoubleConverter(StringToDouble.getInstance)
          cv.setURIChecker(StandardURIChecker.getInstance)
          // In XSD 1.1, there is no checking
        } else {
          cv.setStringToDoubleConverter(StringToDouble11.getInstance)
        }
        cv.setAllowYearZero(xsdVersion != Configuration.XSD10)
        theConversionRules = cv
        return cv
      }
    } else
      theConversionRules

  /**
   * Get the version of XML Schema to be used
   *
   * @return { @link #XSD10} or { @link #XSD11}
   * @since 9.2
   */
  def getXsdVersion: Int = xsdVersion

  /**
   * Get an XPathContext object with sufficient capability to perform comparisons and conversions
   *
   * @return a dynamic context for performing conversions
   */
  def getConversionContext: XPathContext = {
    if (theConversionContext == null) theConversionContext = new EarlyEvaluationContext(this)
    theConversionContext
  }

  /**
   * Get an IntPredicate that tests whether a given character is valid in the selected
   * version of XML
   *
   * @return an IntPredicate whose matches() method tests a character for validity against
   *         the version of XML (1.0 or 1.1) selected by this configuration
   */

  def getValidCharacterChecker: IntPredicate = if (xmlVersion == Configuration.XML10) (x: Int) => XMLCharacterData.isValid10(x)
  else (x: Int) => XMLCharacterData.isValid11(x)

  /**
   * Get the Tree Model used by this Configuration. This is typically
   * {@link Builder#LINKED_TREE}, {@link Builder#TINY_TREE}, or {@link Builder#TINY_TREE_CONDENSED}.
   * The default is <code>Builder.TINY_TREE</code>.
   *
   * @return the selected Tree Model
   * @since 8.4 (Condensed tinytree added in 9.2)
   */
  def getTreeModel: Int = defaultParseOptions.getModel.getSymbolicValue

  /**
   * Set the Tree Model used by this Configuration. This is typically
   * {@link Builder#LINKED_TREE} or {@link Builder#TINY_TREE}, or {@link Builder#TINY_TREE_CONDENSED}.
   * The default is <code>Builder.TINY_TREE</code>.
   *
   * @param treeModel the integer constant representing the selected Tree Model
   * @since 8.4 (Condensed tinytree added in 9.2)
   */
  def setTreeModel(treeModel: Int): Unit = defaultParseOptions.setModel(TreeModel.getTreeModel(treeModel))

  /**
   * Determine whether source documents will maintain line numbers, for the
   * benefit of the saxon:line-number() extension function as well as run-time
   * tracing.
   *
   * @return true if line numbers are maintained in source documents
   * @since 8.4
   */
  def isLineNumbering: Boolean = defaultParseOptions.isLineNumbering

  /**
   * Determine whether source documents will maintain line numbers, for the
   * benefit of the saxon:line-number() extension function as well as run-time
   * tracing.
   *
   * @param lineNumbering true if line numbers are maintained in source documents
   * @since 8.4
   */
  def setLineNumbering(lineNumbering: Boolean): Unit = defaultParseOptions.setLineNumbering(lineNumbering)

  /**
   * Set whether or not source documents (including stylesheets and schemas) are have
   * XInclude processing applied to them, or not. Default is false.
   *
   * @param state true if XInclude elements are to be expanded, false if not
   * @since 8.9
   */
  def setXIncludeAware(state: Boolean): Unit = defaultParseOptions.setXIncludeAware(state)

  /**
   * Test whether or not source documents (including stylesheets and schemas) are to have
   * XInclude processing applied to them, or not
   *
   * @return true if XInclude elements are to be expanded, false if not
   * @since 8.9
   */
  def isXIncludeAware: Boolean = defaultParseOptions.isXIncludeAware

  /**
   * Get the TraceListener used for run-time tracing of instruction execution.
   *
   * @return the TraceListener that was set using { @link #setTraceListener} if set.
   *         Otherwise, returns null.
   * @since 8.4. Modified in 9.1.
   */
  def getTraceListener: TraceListener = traceListener

  /**
   * Get or create the TraceListener used for run-time tracing of instruction execution.
   *
   * @return If a TraceListener has been set using { @link #setTraceListener(org.orbeon.saxon.lib.TraceListener)},
   *         returns that TraceListener. Otherwise, if a TraceListener class has been set using
   *         { @link #setTraceListenerClass(String)}, returns a newly created instance of that class.
   *         Otherwise, returns null.
   * @throws XPathException if the supplied TraceListenerClass cannot be instantiated as an instance
   *                        of TraceListener
   * @since 9.1.
   */
  @throws[XPathException]
  def makeTraceListener: TraceListener =
    if (traceListener != null)
      traceListener
    else if (traceListenerClass != null)
      try makeTraceListener(traceListenerClass)
      catch {
        case e: ClassCastException =>
          throw new XPathException(e)
      }
    else
      null

  /**
   * Set the TraceListener to be used for run-time tracing of instruction execution.
   * <p>Note: this method should not be used if the Configuration is multithreading. In that situation,
   * use {@link #setCompileWithTracing(boolean)} to force stylesheets and queries to be compiled
   * with trace code enabled, and use {@link Controller#addTraceListener(org.orbeon.saxon.lib.TraceListener)} to
   * supply a TraceListener at run time.</p>
   *
   * @param traceListener The TraceListener to be used. If null is supplied, any existing TraceListener is removed
   * @since 8.4
   */
  def setTraceListener(traceListener: TraceListener): Boolean = {
    this.traceListener = traceListener
    setCompileWithTracing(traceListener != null)
    internalSetBooleanProperty(Feature.ALLOW_MULTITHREADING, false)
  }

  /**
   * Set the name of the trace listener class to be used for run-time tracing of instruction
   * execution. A new instance of this class will be created for each query or transformation
   * that requires tracing. The class must be an instance of {@link TraceListener}.
   *
   * @param className the name of the trace listener class. If null, any existing trace listener is
   *                  removed from the configuration.
   * @throws IllegalArgumentException if the class cannot be instantiated or does not implement
   *                                  TraceListener
   * @since 9.1. Changed in 9.4 to allow null to be supplied.
   */
  def setTraceListenerClass(className: String): Unit = if (className == null) {
    traceListenerClass = null
    setCompileWithTracing(false)
  }
  else {
    try makeTraceListener(className)
    catch {
      case err: XPathException =>
        throw new IllegalArgumentException(className + ": " + err.getMessage)
    }
    this.traceListenerClass = className
    setCompileWithTracing(true)
  }

  /**
   * Get the name of the trace listener class to be used for run-time tracing of instruction
   * execution. A new instance of this class will be created for each query or transformation
   * that requires tracing. The class must be an instance of {@link org.orbeon.saxon.lib.TraceListener}.
   *
   * @return the name of the trace listener class, or null if no trace listener class
   *         has been nominated.
   * @since 9.1
   */
  def getTraceListenerClass: String = traceListenerClass

  /**
   * Set the name of the file to be used for TraceListener output. (By default,
   * it is sent to System.err)
   *
   * @param filename the filename for TraceListener output
   * @since 9.8
   */
  def setTraceListenerOutputFile(filename: String): Unit = traceListenerOutput = filename

  /**
   * Get the name of the file to be used for TraceListener output. Returns
   * null if no value has been set
   *
   * @return the filename to be used for TraceListener output, or null
   *         if none has been set
   * @since 9.8
   */
  def getTraceListenerOutputFile: String = traceListenerOutput

  /**
   * Determine whether compile-time generation of trace code was requested
   *
   * @return true if compile-time generation of code was requested at the Configuration level.
   *         Note that tracing can also be requested at the XQueryCompiler or XsltCompiler level.
   * @since 8.8
   */
  def isCompileWithTracing: Boolean = getBooleanProperty(Feature.COMPILE_WITH_TRACING)

  /**
   * Request compile-time generation of trace code (or not)
   *
   * @param trace true if compile-time generation of trace code is required
   * @since 8.8
   */
  def setCompileWithTracing(trace: Boolean): Unit = {
    internalSetBooleanProperty(Feature.COMPILE_WITH_TRACING, trace)
    if (defaultStaticQueryContext != null) if (trace) defaultStaticQueryContext.setCodeInjector(new TraceCodeInjector)
    else defaultStaticQueryContext.setCodeInjector(null)
  }

  @throws[XPathException]
  def makeTraceListener(className: String): TraceListener = {
    ???
    // ORBEON: maybe TODO.
//    dynamicLoader.getInstance(className, null) match {
//      case listener: TraceListener =>
//        val destination = getTraceListenerOutputFile
//        if (destination != null)
//          try listener.setOutputDestination(new StandardLogger(new PrintStream(destination)))
//          catch {
//            case e: Exception =>
//              throw new XPathException(e)
//          }
//        return listener
//      case _ =>
//    }
//    throw new XPathException("Class " + className + " is not a TraceListener")
  }

//   def getXSLT30FunctionSet = XSLT30FunctionSet.getInstance

  def getUseWhenFunctionSet: UseWhen30FunctionSet = UseWhen30FunctionSet.getInstance

  def getXPath30FunctionSet: XPath30FunctionSet = XPath30FunctionSet.getInstance

  def getXPath31FunctionSet: XPath31FunctionSet = XPath31FunctionSet.getInstance

  def getXQueryUpdateFunctionSet: BuiltInFunctionSet = null

  /**
   * Make a function in the "fn" namespace
   *
   * @return the function
   */
  def makeSystemFunction(localName: String, arity: Int): SystemFunction =
    try
      getXPath31FunctionSet.makeFunction(localName, arity)
    catch {
      case _: XPathException =>
        null
    }

  /**
   * Register an extension function that is to be made available within any stylesheet, query,
   * or XPath expression compiled under the control of this processor. This method
   * registers an extension function implemented as an instance of
   * {@link org.orbeon.saxon.lib.ExtensionFunctionDefinition}, using an arbitrary name and namespace.
   * This supplements the ability to call arbitrary Java methods using a namespace and local name
   * that are related to the Java class and method name.
   *
   * @param function the object that implements the extension function.
   * @since 9.2
   */
  def registerExtensionFunction(function: ExtensionFunctionDefinition): Unit = integratedFunctionLibrary.registerFunction(function)

  /**
   * Get the IntegratedFunction library containing integrated extension functions
   *
   * @return the IntegratedFunctionLibrary
   * @since 9.2
   */
  def getIntegratedFunctionLibrary: IntegratedFunctionLibrary = integratedFunctionLibrary

  def getBuiltInExtensionLibraryList: FunctionLibraryList = {
    if (builtInExtensionLibraryList == null) {
      builtInExtensionLibraryList = new FunctionLibraryList
      builtInExtensionLibraryList.addFunctionLibrary(VendorFunctionSetHE.getInstance)
      builtInExtensionLibraryList.addFunctionLibrary(MathFunctionSet.getInstance)
      builtInExtensionLibraryList.addFunctionLibrary(MapFunctionSet.getInstance)
      builtInExtensionLibraryList.addFunctionLibrary(ArrayFunctionSet.getInstance)
    }
    builtInExtensionLibraryList
  }

  /**
   * Instantiate a Saxon extension function. This generally requires Saxon-PE or higher,
   * so it will fail with this Configuration
   *
   * @param localName the local name of the function in the Saxon namespace
   * @param arity     the function arity
   * @return the system function with this name and arity
   */
  @throws[XPathException]
  def bindSaxonExtensionFunction(localName: String, arity: Int) = throw new UnsupportedOperationException("The extension function saxon:" + localName + "#" + arity + " requires Saxon-PE or higher")

  /**
   * Add the registered extension binders to a function library.
   * This method is intended primarily for internal use
   *
   * @param list the function library list
   */
  def addExtensionBinders(list: FunctionLibraryList): Unit = {
    // no action in this class
  }

  /**
   * Get a system function. This can be any function defined in XPath 3.1 functions and operators,
   * including functions in the math, map, and array namespaces. It can also be a Saxon extension
   * function, provided a licensed Processor is used.
   *
   * @param name  the name of the required function
   * @param arity the arity of the required function
   * @return the requested function, or null if there is no such function. Note that some functions
   *         (those with particular context dependencies) may be unsuitable for dynamic calling.
   * @throws XPathException if dynamic function calls are not permitted by this Saxon Configuration
   */
  @throws[XPathException]
  def getSystemFunction(name: StructuredQName, arity: Int) = throw new XPathException("Dynamic functions require Saxon-PE or higher")

  /**
   * Make a UserFunction object.
   * This method is for internal use.
   *
   * @param memoFunction  true if the function is to be a memo function.
   * @param streamability the declared streamability of the function
   * @return the newly created user-defined function
   */
  def newUserFunction(memoFunction: Boolean, streamability: FunctionStreamability): UserFunction = if (memoFunction) new MemoFunction else new UserFunction

  /**
   * Register a collation. Collation URIs registered using this interface will be
   * picked up before calling the CollationURIResolver.
   *
   * @param collationURI the URI of the collation. This should be an absolute URI,
   *                     though it is not checked
   * @param collator     the implementation of the collation
   * @since 9.6
   */
  def registerCollation(collationURI: String, collator: StringCollator): StringCollator = collationMap.put(collationURI, collator)

  /**
   * Set a CollationURIResolver to be used to resolve collation URIs (that is,
   * to take a URI identifying a collation, and return the corresponding collation).
   * Note that Saxon attempts first to resolve a collation URI using the resolver
   * registered with the Controller; if that returns null, it tries again using the
   * resolver registered with the Configuration.
   * <p>Note that it is undefined whether collation URIs are resolved at compile time
   * or at run-time. It is therefore inadvisable to change the CollationURIResolver after
   * compiling a query or stylesheet and before running it.</p>
   *
   * @param resolver the collation URI resolver to be used. This replaces any collation
   *                 URI resolver previously registered.
   * @since 8.5
   */
  def setCollationURIResolver(resolver: CollationURIResolver): Unit = collationResolver = resolver

  /**
   * Get the collation URI resolver associated with this configuration. This will
   * return the CollationURIResolver previously set using the {@link #setCollationURIResolver}
   * method; if this has not been called, it returns the system-defined collation URI resolver
   *
   * @return the registered CollationURIResolver
   * @since 8.5
   */
  def getCollationURIResolver: CollationURIResolver = collationResolver

  /**
   * Get the collation with a given collation name. If the collation name has
   * not been registered in this CollationMap, the CollationURIResolver registered
   * with the Configuration is called. If this cannot resolve the collation name,
   * it should return null.
   *
   * @param collationName the collation name as an absolute URI
   * @return the StringCollator with this name if known, or null if not known
   * @throws XPathException if the collation URI is recognized but is invalid; for example,
   *                        if it is a URI that takes parameters, and the parameters are invalid. If a user-supplied
   *                        collation URI resolver is in use, then any exception thrown by that resolver is passed
   *                        on to the caller.
   */
  @throws[XPathException]
  def getCollation(collationName: String): StringCollator = {
    if (collationName == null || collationName == NamespaceConstant.CODEPOINT_COLLATION_URI)
      CodepointCollator.getInstance
    else if (collationName == NamespaceConstant.HTML5_CASE_BLIND_COLLATION_URI)
      HTML5CaseBlindCollator.getInstance
    else if (collationName.startsWith(AlphanumericCollator.PREFIX))
      new AlphanumericCollator(getCollation(collationName.substring(AlphanumericCollator.PREFIX.length)))
    else {
      var collator = collationMap.get(collationName)
      if (collator == null)
        collator = getCollationURIResolver.resolve(collationName, this)
      collator
    }
  }

  /**
   * Get the collation with a given collation name, supplying a relative URI and base
   * URI separately. If the collation name has
   * not been registered in this CollationMap, the CollationURIResolver registered
   * with the Configuration is called. If this cannot resolve the collation name,
   * it should return null.
   *
   * @param collationURI the collation name as a relative or absolute URI
   * @param baseURI      the base URI to be used to resolve the collationURI if it is relative
   * @return the StringCollator with this name if known, or null if not known
   * @throws XPathException if a failure occurs during URI resolution
   */
  @throws[XPathException]
  def getCollation(collationURI: String, baseURI: String): StringCollator = {
    if (collationURI == NamespaceConstant.CODEPOINT_COLLATION_URI) return CodepointCollator.getInstance
    try {
      val absoluteURI = ResolveURI.makeAbsolute(collationURI, baseURI).toString
      getCollation(absoluteURI)
    } catch {
      case e: URISyntaxException =>
        throw new XPathException("Collation name is not a valid URI: " + collationURI + " (base = " + baseURI + ")", "FOCH0002")
    }
  }

  /**
   * Get the collation with a given collation name, supplying a relative URI and base
   * URI separately, and throwing an error if it cannot be found. If the collation name has
   * not been registered in this CollationMap, the CollationURIResolver registered
   * with the Configuration is called. If this cannot resolve the collation name,
   * it should return null.
   *
   * @param collationURI the collation name as a relative or absolute URI
   * @param baseURI      the base URI to be used to resolve the collationURI if it is relative;
   *                     may be null if the supplied collationURI is known to be absolute
   * @param errorCode    the error to raise if the collation is not known
   * @return the StringCollator with this name if known, or null if not known
   * @throws XPathException if a failure occurs during URI resolution
   */
  @throws[XPathException]
  def getCollation(collationURI: String, baseURI: String, errorCode: String): StringCollator = {
    if (collationURI == NamespaceConstant.CODEPOINT_COLLATION_URI) return CodepointCollator.getInstance
    try {
      var absoluteURI = collationURI
      if (baseURI != null) absoluteURI = ResolveURI.makeAbsolute(collationURI, baseURI).toString
      val collator = getCollation(absoluteURI)
      if (collator == null) throw new XPathException("Unknown collation " + absoluteURI, errorCode)
      collator
    } catch {
      case e: URISyntaxException =>
        throw new XPathException("Collation name is not a valid URI: " + collationURI + " (base = " + baseURI + ")", errorCode)
    }
  }

  /**
   * Get the name of the global default collation
   *
   * @return the name of the default collation. If none has been set, returns
   *         the URI of the Unicode codepoint collation
   */
  def getDefaultCollationName: String = defaultCollationName

  /**
   * Set the default collection.
   * <p>If no default collection URI is specified, then a request for the default collection
   * is handled by calling the registered collection URI resolver with an argument of null.</p>
   *
   * @param uri the URI of the default collection. Calling the collection() function
   *            with no arguments is equivalent to calling collection() with this URI as an argument.
   *            The URI will be dereferenced by passing it to the registered CollectionFinder.
   *            If null is supplied, any existing default collection is removed.
   * @since 9.2
   */
  def setDefaultCollection(uri: String): Unit = defaultCollection = uri

  /**
   * Get the URI of the default collection. Returns null if no default collection URI has
   * been registered.
   *
   * @return the default collection URI. This is dereferenced in the same way as a normal
   *         collection URI (via the CollectionFinder) to return a sequence of nodes
   * @since 9.2
   */
  def getDefaultCollection: String = defaultCollection

  /**
   * Set the collection finder associated with this configuration. This is used to dereference
   * collection URIs used in the fn:collection and fn:uri-collection functions
   *
   * @param cf the CollectionFinder to be used
   * @since 9.7
   */
  def setCollectionFinder(cf: CollectionFinder): Unit = collectionFinder = cf

  /**
   * Get the collection finder associated with this configuration. This is used to dereference
   * collection URIs used in the fn:collection and fn:uri-collection functions
   *
   * @return the CollectionFinder to be used
   * @since 9.7
   */
  def getCollectionFinder: CollectionFinder = collectionFinder

  /**
   * Get the standard collection finder. This is always an instance of {@link StandardCollectionFinder}
   * and there is no way of changing it. It is available for calling from a user-supplied collection
   * finder as a way to delegate processing, in the event that the user-supplied collection finder
   * wants to handle some collection URIs its own way, and others in the standard way
   *
   * @return the standard collection finder
   */
  def getStandardCollectionFinder: StandardCollectionFinder = standardCollectionFinder

  /**
   * Register a specific URI and bind it to a specific ResourceCollection. This method modifies
   * the standard collection finder (as returned by {@link #getStandardCollectionFinder()},
   * and it also modifies the user-supplied collection finder (as returned by
   * {@link #getCollectionFinder()} if and only if this is an instance of the class
   * {@link StandardCollectionFinder}.
   *
   * @param collectionURI the collection URI to be registered. Must not be null.
   * @param collection    the ResourceCollection to be associated with this URI. Must not be null.
   * @since 9.7.0.2. Modified in 10.0 to work on the standard collection finder in all cases,
   *        so there is no failure if the user-supplied collection finder does not implement
   *        the { @link StandardCollectionFinder} interface.
   */
  def registerCollection(collectionURI: String, collection: ResourceCollection): Unit = {
    standardCollectionFinder.registerCollection(collectionURI, collection)
    if (collectionFinder.isInstanceOf[StandardCollectionFinder] && (collectionFinder ne standardCollectionFinder))
      collectionFinder.asInstanceOf[StandardCollectionFinder].registerCollection(collectionURI, collection)
  }

  /**
   * Set the media type to be associated with a file extension by the standard
   * collection handler
   *
   * @param extension the file extension, for example "xml". The value "" sets
   *                  the default media type to be used for unregistered file extensions.
   * @param mediaType the corresponding media type, for example "application/xml". The
   *                  choice of media type determines how a resource with this extension
   *                  gets parsed, when the file appears as part of a collection.
   * @since 9.7.0.1
   */
  def registerFileExtension(extension: String, mediaType: String): String = fileExtensions.put(extension, mediaType)

  /**
   * Associate a media type with a resource factory. This method may
   * be called to customize the behaviour of a collection to recognize different file extensions
   *
   * @param contentType a media type or MIME type, for example application/xsd+xml
   * @param factory     a ResourceFactory used to parse (or otherwise process) resources of that type
   * @since 9.7.0.6
   */
  def registerMediaType(contentType: String, factory: ResourceFactory): ResourceFactory = resourceFactoryMapping.put(contentType, factory)

  /**
   * Get the media type to be associated with a file extension by the standard
   * collection handler
   *
   * @param extension the file extension, for example "xml". The value "" gets
   *                  the default media type to be used for unregistered file extensions.
   *                  the default media type is also returned if the supplied file
   *                  extension is not registered.
   * @return the corresponding media type, for example "application/xml". The
   *         choice of media type determines how a resource with this extension
   *         gets parsed, when the file appears as part of a collection.
   * @since 9.7.0.1
   */
  def getMediaTypeForFileExtension(extension: String): String = {
    var mediaType = fileExtensions.get(extension)
    if (mediaType == null) mediaType = fileExtensions.get("")
    mediaType
  }

  /**
   * Get the resource factory associated with a media type
   *
   * @param mediaType the media type or MIME type, for example "application/xml"
   * @return the associated resource factory if one has been registered for this media type,
   *         or null otherwise
   */
  def getResourceFactoryForMediaType(mediaType: String): ResourceFactory = resourceFactoryMapping.get(mediaType)

  /**
   * Set the localizer factory to be used
   *
   * @param factory the LocalizerFactory
   * @since 9.2
   */
  def setLocalizerFactory(factory: LocalizerFactory): Unit = this.localizerFactory = factory

  /**
   * Get the localizer factory in use
   *
   * @return the LocalizerFactory, if any. If none has been set, returns null.
   * @since 9.2
   */
  def getLocalizerFactory: LocalizerFactory = localizerFactory

  /**
   * Set the default language to be used for number and date formatting when no language is specified.
   * If none is set explicitly, the default Locale for the Java Virtual Machine is used.
   *
   * @param language the default language to be used, as an ISO code for example "en" or "fr-CA"
   * @throws IllegalArgumentException if not valid as an xs:language instance.
   * @since 9.2. Validated from 9.7 against xs:language type.
   */
  def setDefaultLanguage(language: String): Unit = {
    val vf = StringConverter.StringToLanguage.INSTANCE.validate(language)
    if (vf != null) throw new IllegalArgumentException("The default language must be a valid language code")
    defaultLanguage = language
  }

  /**
   * Get the default language. Unless an explicit default is set, this will be the language
   * of the default Locale for the Java Virtual Machine
   *
   * @return the default language
   * @since 9.2
   */
  def getDefaultLanguage: String = defaultLanguage

  /**
   * Set the default country to be used for number and date formatting when no country is specified.
   * If none is set explicitly, the default Locale for the Java Virtual Machine is used.
   *
   * @param country the default country to be used, as an ISO code for example "US" or "GB"
   * @since 9.2
   */
  def setDefaultCountry(country: String): Unit = defaultCountry = country

  /**
   * Get the default country to be used for number and date formatting when no country is specified.
   * If none is set explicitly, the default Locale for the Java Virtual Machine is used.
   *
   * @return the default country to be used, as an ISO code for example "US" or "GB"
   * @since 9.2
   */
  def getDefaultCountry: String = defaultCountry

  /**
   * Set the default regular expression engine to be used
   *
   * @param engine the default regular expression engine to be used. The value must be one of:
   *               <ul>
   *               <li>S - the Saxon regular expression engine (derived form Apache Jakarta)</li>
   *               <li>J - the Java JDK regular expression engine</li>
   *               <li>N - (on .NET only) - the .NET regular expression engine</li>
   *               </ul>
   */
  def setDefaultRegexEngine(engine: String): Unit =
    if (! ("J" == engine || "N" == engine || "S" == engine))
      throw new IllegalArgumentException("Regex engine must be S|J|N")
    else
      defaultRegexEngine = engine

  /**
   * Get the default regular expression to be used
   *
   * @return the default regular expression to be used. Returns "S" if no value has been
   *         explicitly set.
   */
  def getDefaultRegexEngine: String = defaultRegexEngine

  /**
   * Compile a regular expression (or, in some configurations, get a compiled
   * regular expression from a cache
   *
   * @param regex        the regular expression as a string
   * @param flags        the value of the flags attribute
   * @param hostLanguage one of "XSD10", "XSD11", XP20" or "XP30". Also allow combinations, e.g. "XP20/XSD11".
   * @param warnings     if non-null, any warnings from the regular expression compiler will be added to this list.
   *                     If null, the warnings are ignored.
   * @return the compiled regular expression
   * @throws XPathException if the regular expression or the flags are invalid
   */
  @throws[XPathException]
  def compileRegularExpression(
    regex        : CharSequence,
    flags        : String,
    hostLanguage : String,
    warnings     : ju.List[String]
  ): RegularExpression =
    Version.platform.compileRegularExpression(this, regex, flags, hostLanguage, warnings)

  /**
   * Load a Numberer class for a given language and check it is OK.
   * This method is provided primarily for internal use.
   *
   * @param language the language for which a Numberer is required. May be null,
   *                 indicating default language
   * @param country  the country for which a Numberer is required. May be null,
   *                 indicating default country
   * @return a suitable numberer. If no specific numberer is available
   *         for the language, the default numberer (normally English) is used.
   */
  def makeNumberer(language: String, country: String): Numberer = if (localizerFactory == null) {
    val numberer = new Numberer_en
    if (language != null) numberer.setLanguage(language)
    if (country != null) numberer.setCountry(country)
    numberer
  }
  else {
    var numberer = localizerFactory.getNumberer(language, country)
    if (numberer == null) numberer = new Numberer_en
    numberer
  }

  /**
   * Set a user-defined ModuleURIResolver for resolving URIs used in "import module"
   * declarations in an XQuery prolog.
   * This acts as the default value for the ModuleURIResolver in the StaticQueryContext, and may be
   * overridden by a more specific ModuleURIResolver nominated as part of the StaticQueryContext.
   *
   * @param resolver the URI resolver for XQuery modules. May be null, in which case any existing
   *                 Module URI Resolver is removed from the configuration
   */
  def setModuleURIResolver(resolver: ModuleURIResolver): Unit = getDefaultStaticQueryContext.setModuleURIResolver(resolver)

  /**
   * Create and register an instance of a ModuleURIResolver with a specified class name.
   * This will be used for resolving URIs in XQuery "import module" declarations, unless
   * a more specific ModuleURIResolver has been nominated as part of the StaticQueryContext.
   *
   * @param className The fully-qualified name of the LocationHintResolver class
   * @throws TransformerException if the requested class does not
   *                              implement the org.orbeon.saxon.LocationHintResolver interface
   */
  @throws[TransformerException]
  def setModuleURIResolver(className: String): Unit = {
    val obj = dynamicLoader.getInstance(className, null)
    if (obj.isInstanceOf[ModuleURIResolver]) setModuleURIResolver(obj.asInstanceOf[ModuleURIResolver])
    else throw new XPathException("Class " + className + " is not a ModuleURIResolver")
  }

  /**
   * Get the user-defined ModuleURIResolver for resolving URIs used in "import module"
   * declarations in the XQuery prolog; returns null if none has been explicitly set.
   *
   * @return the resolver for Module URIs
   */
  def getModuleURIResolver: ModuleURIResolver = getDefaultStaticQueryContext.getModuleURIResolver

  /**
   * Get the standard system-defined ModuleURIResolver for resolving URIs used in "import module"
   * declarations in the XQuery prolog.
   *
   * @return the standard system-defined ModuleURIResolver for resolving URIs
   */
  def getStandardModuleURIResolver: ModuleURIResolver = standardModuleURIResolver

  /**
   * Get the URI resolver used for resolving URIs passed to the unparsed-text(),
   * unparsed-text-available(), and unparsed-text-lines() functions
   *
   * @return the URI resolver set for these functions, if one has been set, or the
   *         standard system-supplied resolver otherwise
   */
  def getUnparsedTextURIResolver: UnparsedTextURIResolver = unparsedTextURIResolver

  /**
   * Set the URI resolver to be used for resolving URIs passed to the unparsed-text(),
   * unparsed-text-available(), and unparsed-text-lines() functions
   *
   * @param resolver the URI resolver to be used for these functions.
   */
  def setUnparsedTextURIResolver(resolver: UnparsedTextURIResolver): Unit = this.unparsedTextURIResolver = resolver

  /**
   * Get the default options for XSLT compilation
   *
   * @return the default options for XSLT compilation. The CompilerInfo object will reflect any options
   *         set using other methods available for this Configuration object
   */
  //def getDefaultXsltCompilerInfo: CompilerInfo = defaultXsltCompilerInfo

  /**
   * Get the default options for XQuery compilation
   *
   * @return the default XQuery static context for this configuration
   */
  def getDefaultStaticQueryContext: StaticQueryContext = {
    if (defaultStaticQueryContext == null) defaultStaticQueryContext = makeStaticQueryContext(false)
    defaultStaticQueryContext
  }

  /**
   *
   * Make a new static query context object using the factory registered with this configuration
   *
   * @param copyFromDefault true if the new static query context is to copy the properties
   *                        of the default static query context held in the configuration
   * @return the new static query context object
   */
   def makeStaticQueryContext(copyFromDefault: Boolean): StaticQueryContext =
    staticQueryContextFactory.newStaticQueryContext(this, copyFromDefault)

  /**
   * Register a FunctionAnnotationHandler to handle XQuery function annotations
   * in a particular namespace
   *
   * @param handler a function annotation handler to be invoked in respect of function
   *                annotations in the relevant namespace
   */
  def registerFunctionAnnotationHandler(handler: FunctionAnnotationHandler): FunctionAnnotationHandler = functionAnnotationHandlers.put(handler.getAssertionNamespace, handler)

  /**
   * Get the FunctionAnnotationHandler used to handle XQuery function annotations
   * in a particular namespace
   *
   * @param namespace the namespace
   * @return a function annotation handler to be invoked in respect of function
   *         annotations in the relevant namespace, or null if none has
   *         been registered
   *
   */
  def getFunctionAnnotationHandler(namespace: String): FunctionAnnotationHandler = functionAnnotationHandlers.get(namespace)

  /**
   * Ask whether streamability checking is enabled for this configuration
   *
   * @return always false for Saxon-HE
   */
  def isStreamabilityEnabled: Boolean = false

  /**
   * Get the name of the class that will be instantiated to create a MessageEmitter,
   * to process the output of xsl:message instructions in XSLT.
   *
   * @return the full class name of the message emitter class.
   * @since 8.4
   */
  //def getMessageEmitterClass: String = defaultXsltCompilerInfo.getMessageReceiverClassName

//  /**
//   * Set the name of the class that will be instantiated to
//   * to process the output of xsl:message instructions in XSLT.
//   *
//   * @param messageReceiverClassName the full class name of the message receiver. This
//   *                                 must implement org.orbeon.saxon.event.Receiver.
//   * @since 8.4
//   */
  //def setMessageEmitterClass(messageReceiverClassName: String) = defaultXsltCompilerInfo.setMessageReceiverClassName(messageReceiverClassName)

  /**
   * Get the name of the class that will be instantiated to create an XML parser
   * for parsing source documents (for example, documents loaded using the document()
   * or doc() functions).
   * <p>This method is retained in Saxon for backwards compatibility, but the preferred way
   * of choosing an XML parser is to use JAXP interfaces, for example by supplying a
   * JAXP SAXSource object initialized with an appropriate implementation of org.xml.sax.XMLReader.</p>
   *
   * @return the fully qualified name of the XML parser class
   */
  def getSourceParserClass: String = sourceParserClass

  /**
   * Set the name of the class that will be instantiated to create an XML parser
   * for parsing source documents (for example, documents loaded using the document()
   * or doc() functions).
   * <p>This method is retained in Saxon for backwards compatibility, but the preferred way
   * of choosing an XML parser is to use JAXP interfaces, for example by supplying a
   * JAXP SAXSource object initialized with an appropriate implementation of org.xml.sax.XMLReader.</p>
   *
   * @param sourceParserClass the fully qualified name of the XML parser class. This must implement
   *                          the SAX2 XMLReader interface.
   */
  def setSourceParserClass(sourceParserClass: String): Unit = this.sourceParserClass = sourceParserClass

  /**
   * Get the name of the class that will be instantiated to create an XML parser
   * for parsing stylesheet modules.
   * <p>This method is retained in Saxon for backwards compatibility, but the preferred way
   * of choosing an XML parser is to use JAXP interfaces, for example by supplying a
   * JAXP Source object initialized with an appropriate implementation of org.xml.sax.XMLReader.</p>
   *
   * @return the fully qualified name of the XML parser class
   */
  def getStyleParserClass: String = styleParserClass

  /**
   * Set the name of the class that will be instantiated to create an XML parser
   * for parsing stylesheet modules.
   * <p>This method is retained in Saxon for backwards compatibility, but the preferred way
   * of choosing an XML parser is to use JAXP interfaces, for example by supplying a
   * JAXP Source object initialized with an appropriate implementation of org.xml.sax.XMLReader.</p>
   *
   * @param parser the fully qualified name of the XML parser class
   */
  def setStyleParserClass(parser: String): Unit = this.styleParserClass = parser

  /**
   * Get the OutputURIResolver that will be used to resolve URIs used in the
   * href attribute of the xsl:result-document instruction.
   *
   * @return the OutputURIResolver. If none has been supplied explicitly, the
   *         default OutputURIResolver is returned.
   * @since 8.4
   */
  //def getOutputURIResolver: OutputURIResolver = defaultXsltCompilerInfo.getOutputURIResolver

//  /**
//   * Set the OutputURIResolver that will be used to resolve URIs used in the
//   * href attribute of the xsl:result-document instruction.
//   *
//   * @param outputURIResolver the OutputURIResolver to be used.
//   * @since 8.4
//   * @deprecated since 9.9. Use { @link Xslt30Transformer#setResultDocumentHandler(java.util.function.Function)}
//   *             or { @link XsltController#setResultDocumentResolver(ResultDocumentResolver)} instead.
//   */
  //def setOutputURIResolver(outputURIResolver: OutputURIResolver) = defaultXsltCompilerInfo.setOutputURIResolver(outputURIResolver)

  /**
   * Set a custom SerializerFactory. This will be used to create a serializer for a given
   * set of output properties and result destination.
   *
   * @param factory a custom SerializerFactory
   * @since 8.8
   */
  def setSerializerFactory(factory: SerializerFactory): Unit = serializerFactory = factory

  /**
   * Get the SerializerFactory. This returns the standard built-in SerializerFactory, unless
   * a custom SerializerFactory has been registered.
   *
   * @return the SerializerFactory in use
   * @since 8.8
   */
  def getSerializerFactory: SerializerFactory = serializerFactory

  /**
   * Get the CharacterSetFactory. Note: at present this cannot be changed.
   *
   * @return the CharacterSetFactory in use.
   * @since 9.2
   */
  def getCharacterSetFactory: CharacterSetFactory = {
    if (characterSetFactory == null) characterSetFactory = new CharacterSetFactory
    characterSetFactory
  }

  /**
   * Set the default serialization properties.
   * <p>The method name is a misnomer, retained for backwards compatibility. A {@link SerializationProperties}
   * object contains a set of {@link Properties} holding simple properties, together with an optional
   * {@link CharacterMap} holding a character map. This method returns the simple properties only.
   * In fact the default properties held in the <code>Configuration</code> only contain simple properties,
   * and never a character map.</p>
   *
   * @param props the default properties
   */
  def setDefaultSerializationProperties(props: Properties): Unit = defaultOutputProperties = props

  /**
   * Get the default output properties. The returned value can be modified in-situ using
   * methods such as {@link Properties#setProperty(String, String)} to change the defaults
   * in force for the {@code Configuration}.
   * <p>The method name is a misnomer, retained for backwards compatibility. A {@link SerializationProperties}
   * object contains a set of {@link Properties} holding simple properties, together with an optional
   * {@link CharacterMap} holding a character map. This method returns the simple properties only.
   * In fact the default properties held in the <code>Configuration</code> only contain simple properties,
   * and never a character map.</p>
   *
   * @return the default output properties.
   */
  def getDefaultSerializationProperties: Properties = defaultOutputProperties

  /**
   * Obtain a default <code>SerializationProperties</code> object.
   *
   * @return the default serialization properties. This is obtained by wrapping the default output
   *         properties maintained in the { @code Configuration} within a { @link SerializationProperties} wrapper.
   *         The returned value can be modified in-situ using
   *         methods such as { @link SerializationProperties#setProperty(String, String)} to change the defaults
   *         in force for the { @code Configuration}.
   */
  def obtainDefaultSerializationProperties = new SerializationProperties(defaultOutputProperties)

  /**
   * Process an xsl:result-document instruction. The Saxon-HE version of this method simply executes the instruction.
   * The Saxon-EE version starts a new thread, and executes the instruction in that thread.
   *
   * @throws XPathException if any dynamic error occurs
   */
  // @throws[XPathException]
  //def processResultDocument(instruction: ResultDocument, content: Expression, context: XPathContext) = instruction.processInstruction(content, context)

  /**
   * Get an item mapping iterator suitable for multi-threaded execution, if this is permitted
   *
   * @param base   iterator over the input sequence
   * @param action mapping function to be applied to each item in the input sequence.
   * @return an iterator over the result sequence
   * @throws XPathException if (for example) a dynamic error occurs while priming the queue
   */
  @throws[XPathException]
  def getMultithreadedItemMappingIterator(base: SequenceIterator, action: ItemMappingFunction): SequenceIterator = new ItemMappingIterator(base, action)

  /**
   * Determine whether brief progress messages and timing information will be output.
   *
   * <p>This method is provided largely for internal use. Progress messages are normally
   * controlled directly from the command line interfaces, and are not normally used when
   * driving Saxon from the Java API.</p>
   *
   * @return true if these messages are to be output.
   */
  def isTiming: Boolean = enabledProperties.contains(FeatureCode.TIMING)

  /**
   * Determine whether brief progress messages and timing information will be output.
   * <p>This method is provided largely for internal use. Progress messages are normally
   * controlled directly from the command line interfaces, and are not normally used when
   * running from a Java application.</p>
   * <p>The name of the method is poorly chosen, since many of the progress messages that
   * are output have little to do with timing or instrumentation.</p>
   * <p>If enabled, the relevant messages will be sent either to the destination set using
   * {@link #setLogger(Logger)} or to the destination set using {@link #setStandardErrorOutput(PrintStream)},
   * depending on the message.</p>
   *
   * @param timing true if these messages are to be output.
   */
  def setTiming(timing: Boolean): Boolean = if (timing) enabledProperties.add(FeatureCode.TIMING)
  else enabledProperties.remove(FeatureCode.TIMING)

  /**
   * Determine whether a warning is to be output when running against a stylesheet labelled
   * as version="1.0". The XSLT specification requires such a warning unless the user disables it.
   *
   * @return always false.
   * @since 8.4
   * @deprecated since 10.0; the method has had no effect since Saxon 9.8
   */
  def isVersionWarning: Boolean = false

  /**
   * Determine whether a warning is to be output when the version attribute of the stylesheet does
   * not match the XSLT processor version. (In the case where the stylesheet version is "1.0",
   * the XSLT specification requires such a warning unless the user disables it.)
   *
   * @param warn ignored.
   * @since 8.4
   * @deprecated since 10.0; the method has had no effect since Saxon 9.8
   */
  def setVersionWarning(warn: Boolean): Unit = {
    // no action
  }

  /**
   * Determine whether the XML parser for source documents will be asked to perform
   * validation of source documents
   *
   * @return true if DTD validation is requested.
   * @since 8.4
   */
  def isValidation: Boolean = defaultParseOptions.getDTDValidationMode == Validation.STRICT || defaultParseOptions.getDTDValidationMode == Validation.LAX

  /**
   * Determine whether the XML parser for source documents will be asked to perform
   * DTD validation of source documents
   *
   * @param validation true if DTD validation is to be requested.
   * @since 8.4
   */
  def setValidation(validation: Boolean): Unit = defaultParseOptions.setDTDValidationMode(if (validation) Validation.STRICT
  else Validation.STRIP)

  /**
   * Create a document projector for a given path map root. Document projection is available only
   * in Saxon-EE, so the Saxon-HE version of this method throws an exception
   *
   * @param map a path map root in a path map. This might represent the call to the initial
   *            context item for a query, or it might represent a call on the doc() function. The path map
   *            contains information about the paths that the query uses within this document.
   * @return a push filter that implements document projection
   * @throws UnsupportedOperationException if this is not a schema-aware configuration, or
   *                                       if no Saxon-EE license is available
   */
  def makeDocumentProjector(map: PathMap.PathMapRoot) = throw new UnsupportedOperationException("Document projection requires Saxon-EE")

  /**
   * Create a document projector for the document supplied as the initial context item
   * in a query. Document projection is available only
   * in Saxon-EE, so the Saxon-HE version of this method throws an exception
   *
   * @param exp an XQuery expression. The document projector that is returned will
   *            be for the document supplied as the context item to this query.
   * @return a push filter that implements document projection
   * @throws UnsupportedOperationException if this is not a schema-aware configuration, or
   *                                       if no Saxon-EE license is available
   */
  def makeDocumentProjector(exp: XQueryExpression) = throw new UnsupportedOperationException("Document projection requires Saxon-EE")

  /**
   * Ask whether source documents (supplied as a StreamSource or SAXSource)
   * should be subjected to schema validation, and if so, in what validation mode
   *
   * @return the schema validation mode previously set using setSchemaValidationMode(),
   *         or the default mode { @link Validation#STRIP} otherwise.
   */
  def getSchemaValidationMode: Int = defaultParseOptions.getSchemaValidationMode

  /**
   * Say whether source documents (supplied as a StreamSource or SAXSource)
   * should be subjected to schema validation, and if so, in what validation mode.
   * This value may be overridden at the level of a Controller for an individual transformation or query.
   *
   * @param validationMode the validation (or construction) mode to be used for source documents.
   *                       One of { @link Validation#STRIP}, { @link Validation#PRESERVE}, { @link Validation#STRICT},
   *                       { @link Validation#LAX}
   * @since 8.4
   */
  def setSchemaValidationMode(validationMode: Int): Unit = { //        switch (validationMode) {
    //            case Validation.STRIP:
    //            case Validation.PRESERVE:
    //                break;
    //            case Validation.LAX:
    //                if (!isLicensedFeature(LicenseFeature.SCHEMA_VALIDATION)) {
    //                    // if schema processing isn't supported, then there's never a schema, so lax validation is a no-op.
    //                    validationMode = Validation.STRIP;
    //                }
    //            case Validation.STRICT:
    //                checkLicensedFeature(LicenseFeature.SCHEMA_VALIDATION, "strict validation", -1);
    //            default:
    //                throw new IllegalArgumentException("Unsupported validation mode " + validationMode);
    //        }
    defaultParseOptions.setSchemaValidationMode(validationMode)
  }

  /**
   * Indicate whether schema validation failures on result documents are to be treated
   * as fatal errors or as warnings. Note that all validation
   * errors are reported to the error() method of the ErrorListener, and processing always
   * continues except when that method chooses to throw an exception. At the end of the document,
   * a fatal error is thrown if (a) there have been any validation errors, and (b) this option
   * is not set.
   *
   * @param warn true if schema validation failures are to be treated as warnings; false if they
   *             are to be treated as fatal errors.
   * @since 8.4
   */
  def setValidationWarnings(warn: Boolean): Unit = defaultParseOptions.setContinueAfterValidationErrors(warn)

  /**
   * Determine whether schema validation failures on result documents are to be treated
   * as fatal errors or as warnings. Note that all validation
   * errors are reported to the error() method of the ErrorListener, and processing always
   * continues except when that method chooses to throw an exception. At the end of the document,
   * a fatal error is thrown if (a) there have been any validation errors, and (b) this option
   * is not set.
   *
   * @return true if validation errors are to be treated as warnings (that is, the
   *         validation failure is reported but processing continues as normal); false
   *         if validation errors are fatal.
   * @since 8.4
   */
  def isValidationWarnings: Boolean = defaultParseOptions.isContinueAfterValidationErrors

  /**
   * Indicate whether attributes that have a fixed or default value are to be expanded when
   * generating a final result tree. By default (and for conformance with the W3C specifications)
   * it is required that fixed and default values should be expanded. However, there are use cases
   * for example when generating XHTML when this serves no useful purpose and merely bloats the output.
   * <p>This option can be overridden at the level of a PipelineConfiguration</p>
   *
   * @param expand true if fixed and default values are to be expanded as required by the W3C
   *               specifications; false if this action is to be disabled. Note that this only affects the validation
   *               of final result trees; it is not possible to suppress expansion of fixed or default values on input
   *               documents, as this would make the type annotations on input nodes unsound.
   * @since 9.0
   */
  def setExpandAttributeDefaults(expand: Boolean): Unit = defaultParseOptions.setExpandAttributeDefaults(expand)

  /**
   * Determine whether elements and attributes that have a fixed or default value are to be expanded.
   * This option applies both to DTD-defined attribute defaults and to schema-defined defaults for
   * elements and attributes. If an XML parser is used that does not report whether defaults have
   * been used, this option is ignored.
   * <p>This option can be overridden at the level of a PipelineConfiguration</p>
   *
   * @return true if elements and attributes that have a fixed or default value are to be expanded,
   *         false if defaults are not to be expanded. The default value is true. Note that the setting "false"
   *         is potentially non-conformant with the W3C specifications.
   * @since 9.0
   */
  def isExpandAttributeDefaults: Boolean = defaultParseOptions.isExpandAttributeDefaults

  /**
   * Get the target namepool to be used for stylesheets/queries and for source documents.
   *
   * @return the target name pool. If no NamePool has been specified explicitly, the
   *         default NamePool is returned.
   * @since 8.4
   */
  def getNamePool: NamePool = namePool

  /**
   * Set the NamePool to be used for stylesheets/queries and for source documents.
   * <p> Using this method allows several Configurations to share the same NamePool. This
   * was the normal default arrangement until Saxon 8.9, which changed the default so
   * that each Configuration uses its own NamePool.</p>
   * <p>Sharing a NamePool creates a potential bottleneck, since changes to the namepool are
   * synchronized.</p>
   *
   * @param targetNamePool The NamePool to be used.
   * @since 8.4
   */
  def setNamePool(targetNamePool: NamePool): Unit = namePool = targetNamePool

  /**
   * Get the TypeHierarchy: a cache holding type information
   *
   * @return the type hierarchy cache
   */
  def getTypeHierarchy: TypeHierarchy = {
    if (typeHierarchy == null) typeHierarchy = new TypeHierarchy(this)
    typeHierarchy
  }

  /**
   * Get an appropriate type-checker. The type-checker that is
   * returned depends on whether backwards compatible (XPath 1.0) mode
   * is enabled (which requires Saxon-PE or higher)
   *
   * @param backwardsCompatible set to true if XPath 1.0 compatibility mode is required
   * @return a suitable TypeChecker
   */
  def getTypeChecker(backwardsCompatible: Boolean): TypeChecker = if (backwardsCompatible) typeChecker10
  else typeChecker

  /**
   * Make a TypeAliasManager appropriate to the type of Configuration
   *
   * @return a new TypeAliasManager
   */
  def makeTypeAliasManager: TypeAliasManager = new TypeAliasManager

  /**
   * Get the document number allocator.
   * <p>The document number allocator is used to allocate a unique number to each document built under this
   * configuration. The document number forms the basis of all tests for node identity; it is therefore essential
   * that when two documents are accessed in the same XPath expression, they have distinct document numbers.
   * Normally this is ensured by building them under the same Configuration. Using this method together with
   * {@link #setDocumentNumberAllocator}, however, it is possible to have two different Configurations that share
   * a single DocumentNumberAllocator</p>
   *
   * @return the current DocumentNumberAllocator
   * @since 9.0
   */
  def getDocumentNumberAllocator: DocumentNumberAllocator = documentNumberAllocator

  /**
   * Set the document number allocator.
   * <p>The document number allocator is used to allocate a unique number to each document built under this
   * configuration. The document number forms the basis of all tests for node identity; it is therefore essential
   * that when two documents are accessed in the same XPath expression, they have distinct document numbers.
   * Normally this is ensured by building them under the same Configuration. Using this method together with
   * {@link #getDocumentNumberAllocator}, however, it is possible to have two different Configurations that share
   * a single DocumentNumberAllocator</p>
   * <p>This method is for advanced applications only. Misuse of the method can cause problems with node identity.
   * The method should not be used except while initializing a Configuration, and it should be used only to
   * arrange for two different configurations to share the same DocumentNumberAllocators. In this case they
   * should also share the same NamePool.</p>
   *
   * @param allocator the DocumentNumberAllocator to be used
   * @since 9.0
   */
  def setDocumentNumberAllocator(allocator: DocumentNumberAllocator): Unit = documentNumberAllocator = allocator

  /**
   * Determine whether two Configurations are compatible. When queries, transformations, and path expressions
   * are run, all the Configurations used to build the documents and to compile the queries and stylesheets
   * must be compatible. Two Configurations are compatible if they share the same NamePool and the same
   * DocumentNumberAllocator.
   *
   * @param other the other Configuration to be compared with this one
   * @return true if the two configurations are compatible
   */
  def isCompatible(other: Configuration): Boolean = (namePool eq other.namePool) && (documentNumberAllocator eq other.documentNumberAllocator)

  /**
   * Get the global document pool. This is used for documents preloaded during query or stylesheet
   * compilation. The user application can preload documents into the global pool, where they will be found
   * if any query or stylesheet requests the specified document using the doc() or document() function.
   *
   * @return the global document pool
   * @since 9.1
   */
  def getGlobalDocumentPool: DocumentPool = globalDocumentPool

  /**
   * Determine whether whitespace-only text nodes are to be stripped unconditionally
   * from source documents.
   *
   * @return true if the space stripping rules for the default parseOptions cause whitespace text
   *         nodes to be stripped from all elements.
   * @since 8.4
   */
  def isStripsAllWhiteSpace: Boolean = defaultParseOptions.getSpaceStrippingRule eq AllElementsSpaceStrippingRule.getInstance

  /**
   * Get an XML parser for source documents.
   * <p>This method is intended primarily for internal use.</p>
   *
   * @return a parser
   */
  def createXMLParser: XMLReader =
    if (getSourceParserClass != null)
      makeParser(getSourceParserClass)
    else
      Configuration.loadParser

  /**
   * Get a parser for source documents. The parser is allocated from a pool if any are available
   * from the pool: the client should ideally return the parser to the pool after use, so that it
   * can be reused.
   * <p>This method is intended primarily for internal use.</p>
   *
   * @return a parser, in which the namespace properties must be set as follows:
   *         namespaces=true; namespace-prefixes=false. The DTD validation feature of the parser will be set
   *         on or off depending on the { @link #setValidation(boolean)} setting.
   * @throws javax.xml.transform.TransformerFactoryConfigurationError if a failure occurs
   *                                                                  configuring the parser for use.
   */
  @throws[TransformerFactoryConfigurationError]
  def getSourceParser: XMLReader = {
    if (sourceParserPool == null)
      sourceParserPool = new ConcurrentLinkedQueue[XMLReader]
    var parser = sourceParserPool.poll
    if (parser != null)
      return parser
    if (getSourceParserClass != null)
      parser = makeParser(getSourceParserClass)
    else
      parser = Configuration.loadParser
    if (isTiming)
      reportParserDetails(parser)
    try Sender.configureParser(parser)
    catch {
      case err: XPathException =>
        throw new TransformerFactoryConfigurationError(err)
    }
    if (isValidation) try parser.setFeature("http://xml.org/sax/features/validation", true)
    catch {
      case err: SAXException =>
        throw new TransformerFactoryConfigurationError("The XML parser does not support validation")
    }
    parser
  }

  /**
   * Report the parser details to the standard error output
   *
   * @param reader the parser
   */
  private def reportParserDetails(reader: XMLReader): Unit = {
    val name = reader.getClass.getName
    //        if (name.equals("com.sun.org.apache.xerces.internal.parsers.SAXParser")) {
    //            name += " version " + com.sun.org.apache.xerces.internal.impl.Version.getVersion();
    traceOutput.info("Using parser " + name)
  }

  /**
   * Return a source parser to the pool, for reuse
   *
   * @param parser The parser: the caller must not supply a parser that was obtained by any
   *               mechanism other than calling the getSourceParser() method.
   *               Must not be null.
   */
  def reuseSourceParser(parser: XMLReader): Unit = {
    if (sourceParserPool == null) sourceParserPool = new ConcurrentLinkedQueue[XMLReader]
    try {
      try { // give things back to the garbage collecter
        parser.setContentHandler(null)
        if (parser.getEntityResolver eq defaultParseOptions.getEntityResolver)
          parser.setEntityResolver(null)
        parser.setDTDHandler(null)
        parser.setErrorHandler(null)
        // Unfortunately setting the lexical handler to null doesn't work on Xerces, because
        // it tests "value instanceof LexicalHandler". So we set it to a lexical handler that
        // holds no references
        // 2020-10-07: ORBEON: I don't see any such thing in Xerces now.
        parser.setProperty("http://xml.org/sax/properties/lexical-handler", null)
      } catch {
        case err@(_: SAXNotRecognizedException | _: SAXNotSupportedException) =>
      }
      sourceParserPool.offer(parser)
    } catch {
      case e: Exception =>

      // setting the callbacks on an XMLReader to null doesn't always work; some parsers throw a
      // NullPointerException. If anything goes wrong, the simplest approach is to ignore the error
      // and not attempt to reuse the parser.
    }
  }

  /**
   * Get the parser for stylesheet documents. This parser is also used for schema documents.
   * <p>This method is intended for internal use only.</p>
   *
   * @return an XML parser (a SAX2 parser) that can be used for stylesheets and schema documents
   * @throws javax.xml.transform.TransformerFactoryConfigurationError if an error occurs
   *                                                                  configuring the parser
   */
  @throws[TransformerFactoryConfigurationError]
  def getStyleParser: XMLReader = {
    if (styleParserPool == null) styleParserPool = new ConcurrentLinkedQueue[XMLReader]
    var parser = styleParserPool.poll
    if (parser != null) return parser
    if (getStyleParserClass != null) parser = makeParser(getStyleParserClass)
    else {
      parser = Configuration.loadParser
      val resolver = new StandardEntityResolver(this)
      parser.setEntityResolver(resolver)
    }
    try {
      parser.setFeature("http://xml.org/sax/features/namespaces", true)
      parser.setFeature("http://xml.org/sax/features/namespace-prefixes", false)
    } catch {
      case e: Exception =>
        throw new TransformerFactoryConfigurationError(e)
    }
    // Following code attempts to bypass attribute value normalization for stylesheets: see bug 2445.
    // It works (the XercesAdapter has been dropped, but will be found somewhere in Subversion) but
    // there are question marks over conformance, as there will be incompatibilities in edge cases
    // for example when newlines appear within string literals within XPath expressions.
    //        if (parser.getClass().getCanonicalName().equals("org.apache.xerces.jaxp.SAXParserImpl.JAXPSAXParser")) {
    //            try {
    //                parser = new org.orbeon.saxon.event.XercesAdapter(parser);
    //            } catch (ClassNotFoundException err) {
    //                // ignore the error;
    //            }
    parser
  }

  /**
   * Return a stylesheet (or schema) parser to the pool, for reuse
   *
   * @param parser The parser: the caller must not supply a parser that was obtained by any
   *               mechanism other than calling the getStyleParser() method.
   */
  def reuseStyleParser(parser: XMLReader): Unit = {
    if (styleParserPool == null) styleParserPool = new ConcurrentLinkedQueue[XMLReader]
    try {
      try {
        parser.setContentHandler(null)
        //parser.setEntityResolver(null);
        parser.setDTDHandler(null)
        parser.setErrorHandler(null)
        // it tests "value instanceof LexicalHandler". Instead we set the lexical handler to one
        // that holds no references
        parser.setProperty("http://xml.org/sax/properties/lexical-handler", null)
      } catch {
        case err@(_: SAXNotRecognizedException | _: SAXNotSupportedException) =>
      }
      styleParserPool.offer(parser)
    } catch {
      case e: Exception =>
    }
  }

  /**
   * Simple interface to load a schema document
   *
   * @param absoluteURI the absolute URI of the location of the schema document
   * @throws org.orbeon.saxon.model.SchemaException if the schema document at the given location cannot be read or is invalid
   */
  @throws[SchemaException]
  def loadSchema(absoluteURI: String): Unit = readSchema(makePipelineConfiguration, "", absoluteURI, null)

  /**
   * Read a schema from a given schema location
   * <p>This method is intended for internal use.</p>
   *
   * @param pipe           the PipelineConfiguration
   * @param baseURI        the base URI of the instruction requesting the reading of the schema
   * @param schemaLocation the location of the schema to be read
   * @param expected       The expected targetNamespace of the schema being read, or null if there is no expectation
   * @return the target namespace of the schema; null if no schema has been read
   * @throws UnsupportedOperationException      when called in the non-schema-aware version of the product
   * @throws org.orbeon.saxon.model.SchemaException if the schema cannot be read
   */
  @throws[SchemaException]
  def readSchema(pipe: PipelineConfiguration, baseURI: String, schemaLocation: String, expected: String): String = {
    needEnterpriseEdition()
    null
  }

  /**
   * Read schemas from a list of schema locations.
   * <p>This method is intended for internal use.</p>
   *
   * @param pipe            the pipeline configuration
   * @param baseURI         the base URI against which the schema locations are to be resolved
   * @param schemaLocations the relative URIs specified as schema locations
   * @param expected        the namespace URI which is expected as the target namespace of the loaded schema
   * @throws org.orbeon.saxon.model.SchemaException if an error occurs
   */
  @throws[SchemaException]
  def readMultipleSchemas(pipe: PipelineConfiguration, baseURI: String, schemaLocations: util.Collection[String], expected: String): Unit = needEnterpriseEdition()

  /**
   * Read an inline schema from a stylesheet.
   * <p>This method is intended for internal use.</p>
   *
   * @param root          the xs:schema element in the stylesheet
   * @param expected      the target namespace expected; null if there is no
   *                      expectation.
   * @param errorReporter The destination for error messages. May be null, in which case
   *                      the errorListener registered with this Configuration is used.
   * @return the actual target namespace of the schema
   * @throws org.orbeon.saxon.model.SchemaException if the schema cannot be processed
   */
  @throws[SchemaException]
  def readInlineSchema(root: NodeInfo, expected: String, errorReporter: ErrorReporter): String = {
    needEnterpriseEdition()
    null
  }

  /**
   * Throw an error indicating that a request cannot be satisfied because it requires
   * the schema-aware edition of Saxon
   */
   def needEnterpriseEdition() = throw new UnsupportedOperationException("You need the Enterprise Edition of Saxon (with an EnterpriseConfiguration) for this operation")

  /**
   * Load a schema, which will be available for use by all subsequent operations using
   * this Configuration. Any errors will be notified to the ErrorListener associated with
   * this Configuration.
   *
   * @param schemaSource the JAXP Source object identifying the schema document to be loaded
   * @throws SchemaException               if the schema cannot be read or parsed or if it is invalid
   * @throws UnsupportedOperationException if the configuration is not schema-aware
   * @since 8.4
   */
  @throws[SchemaException]
  def addSchemaSource(schemaSource: Source): Unit = addSchemaSource(schemaSource, makeErrorReporter)

  /**
   * Load a schema, which will be available for use by all subsequent operations using
   * this EnterpriseConfiguration.
   *
   * @param schemaSource  the JAXP Source object identifying the schema document to be loaded
   * @param errorReporter the ErrorListener to be notified of any errors in the schema.
   * @throws SchemaException if the schema cannot be read or parsed or if it is invalid
   */
  @throws[SchemaException]
  def addSchemaSource(schemaSource: Source, errorReporter: ErrorReporter): Unit = needEnterpriseEdition()

  /**
   * Add a built-in schema for a given namespace. This is a no-op if the configuration is not schema-aware
   *
   * @param namespace the namespace. Currently built-in schemas are available for the XML and FN namespaces
   */
  def addSchemaForBuiltInNamespace(namespace: String): Unit = {
  }

  /**
   * Determine whether the Configuration contains a cached schema for a given target namespace
   *
   * @param targetNamespace the target namespace of the schema being sought (supply "" for the
   *                        unnamed namespace)
   * @return true if the schema for this namespace is available, false if not.
   */
  def isSchemaAvailable(targetNamespace: String): Boolean = false

  /**
   * Remove all schema components that have been loaded into this Configuration.
   * This method must not be used if any processes (such as stylesheet or query compilations
   * or executions) are currently active. In a multi-threaded environment, it is the user's
   * responsibility to ensure that this method is not called unless it is safe to do so.
   */
  def clearSchemaCache(): Unit = {
    // no-op except in Saxon-EE
  }

  /**
   * Get the set of namespaces of imported schemas
   *
   * @return a Set whose members are the namespaces of all schemas in the schema cache, as
   *         String objects
   */
  def getImportedNamespaces: util.Set[String] = Collections.emptySet[String]

  /**
   * Mark a schema namespace as being sealed. This is done when components from this namespace
   * are first used for validating a source document or compiling a source document or query. Once
   * a namespace has been sealed, it is not permitted to change the schema components in that namespace
   * by redefining them, deriving new types by extension, or adding to their substitution groups.
   *
   * @param namespace the namespace URI of the components to be sealed
   */
  def sealNamespace(namespace: String): Unit = {
  }

  /**
   * Get the set of saxon:param schema parameters declared in the schema held by this Configuration.
   *
   * @return the set of parameters. May return null if none have been declared.
   */
  def getDeclaredSchemaParameters: util.Collection[GlobalParam] = null

  /**
   * Get the set of complex types that have been defined as extensions of a given type.
   * Note that we do not seal the schema namespace, so this list is not necessarily final; we must
   * assume that new extensions of built-in simple types can be added at any time
   *
   * @param type the type whose extensions are required
   * @return an iterator over the types that are derived from the given type by extension
   */
  def getExtensionsOfType(`type`: SchemaType): util.Iterator[_ <: SchemaType] = Collections.emptyIterator[SchemaType]

  @throws[XPathException]
  def importComponents(source: Source): Unit = needEnterpriseEdition()

  @throws[XPathException]
  def exportComponents(out: Receiver): Unit = needEnterpriseEdition()

  /**
   * Get information about the schema in the form of a function item. Supports the extension function
   * saxon:schema
   *
   * @return null for a non-schema-aware configuration
   */
  def getSchemaAsFunctionItem: om.Function = null

  /**
   * Get information about the schema in the form of a function item. Supports the extension function
   * saxon:schema
   *
   * @param kind the component kind, e.g. "element declaration"
   * @param name the component name
   * @return null for a non-schema-aware configuration
   * @throws XPathException if an error occurs
   */
  @throws[XPathException]
  def getSchemaComponentAsFunctionItem(kind: String, name: QNameValue): om.Function = null

  /**
   * Get a global element declaration by fingerprint
   *
   * @param fingerprint the NamePool fingerprint of the element name
   * @return the element declaration whose name matches the given
   *         fingerprint, or null if no element declaration with this name has
   *         been registered.
   */
  def getElementDeclaration(fingerprint: Int): SchemaDeclaration = null
  def getElementDeclaration(qName: StructuredQName): SchemaDeclaration = null
  def getAttributeDeclaration(fingerprint: Int): SchemaDeclaration = null
  def getAttributeDeclaration(attributeName: StructuredQName): SchemaDeclaration = null


  def getSchemaType(name: StructuredQName): SchemaType =
    if (name.hasURI(NamespaceConstant.SCHEMA))
      BuiltInType.getSchemaTypeByLocalName(name.getLocalPart)
    else
      null

  def makeUserUnionType(memberTypes: java.util.List[AtomicType]): ItemType = null

  override def isDeclaredNotation(uri: String, local: String): Boolean = false

  @throws[SchemaException]
  def checkTypeDerivationIsOK(derived: SchemaType, base: SchemaType, block: Int): Unit = ()
  def prepareValidationReporting(context: XPathContext, options: ParseOptions): Unit = ()

  def getDocumentValidator(
    receiver           : Receiver,
    systemId           : String,
    validationOptions  : ParseOptions,
    initiatingLocation : Location
  ): Receiver = receiver

  @throws[XPathException]
  def getElementValidator(
    receiver           : Receiver,
    validationOptions  : ParseOptions,
    locationId         : Location
  ): Receiver = receiver

  @throws[ValidationException]
  @throws[MissingComponentException]
  def validateAttribute(nodeName: StructuredQName, value: CharSequence, validation: Int): SimpleType =
    BuiltInAtomicType.UNTYPED_ATOMIC

  def getAnnotationStripper(destination: Receiver): Receiver = destination

  @throws[TransformerFactoryConfigurationError]
  def makeParser(className: String): XMLReader = {
    // ORBEON: JVM only
    ???
//    try {
//      val obj = dynamicLoader.getInstance(className, null)
//      obj match {
//        case reader: XMLReader => return reader
//        case _ =>
//      }
//      obj match {
//        case factory: SAXParserFactory => try {
//          val saxParser = factory.newSAXParser
//          return saxParser.getXMLReader
//        } catch {
//          case e@(_: ParserConfigurationException | _: SAXException) =>
//            throw new XPathException(e)
//        }
//        case _ =>
//      }
//    } catch {
//      case err: XPathException =>
//        throw new TransformerFactoryConfigurationError(err)
//    }
//    throw new TransformerFactoryConfigurationError("Class " + className + " is not a SAX2 XMLReader or SAXParserFactory")
  }

  @throws[XPathException]
  def newExpressionParser(language: String, updating: Boolean, languageVersion: Int): XPathParser =
    if ("XQ" == language) {
      if (updating) {
        throw new XPathException("XQuery Update is supported only in Saxon-EE")
      } else if (languageVersion == 31 || languageVersion == 30 || languageVersion == 10) {
        val parser = new XQueryParser
        parser.setLanguage(ParsedLanguage.XQUERY, 31)
        parser
      } else {
          throw new XPathException("Unknown XQuery version " + languageVersion)
      }
    } else if ("XP" == language) {
      if (languageVersion == 31 || languageVersion == 30 || languageVersion == 305 || languageVersion == 20) {
        val parser = new XPathParser
        parser.setLanguage(ParsedLanguage.XPATH, languageVersion)
        parser
      } else
        throw new XPathException("Unknown XPath version " + languageVersion)
    } else if ("PATTERN" == language) {
      if (languageVersion == 30 || languageVersion == 20 || languageVersion == 305 || languageVersion == 31)
        new PatternParser30
      else
        throw new XPathException("Unknown XPath version " + languageVersion)
    } else {
      throw new XPathException("Unknown expression language " + language)
    }

  def setDebugger(debugger: Debugger): Unit = this.debugger = debugger
  def getDebugger: Debugger = debugger

  def makeSlotManager: SlotManager =
    if (debugger == null) new SlotManager() else debugger.makeSlotManager()

  @throws[XPathException]
  def makeStreamingTransformer(
    mode           : Mode,
    ordinaryParams : ParameterSet,
    tunnelParams   : ParameterSet,
    output         : Outputter,
    context        : XPathContext
  ): Receiver = throw new XPathException("Streaming is only available in Saxon-EE")

//  @throws[XPathException]
//  def makeStreamInstruction(hrefExp: Expression, body: Expression, streaming: Boolean,
//                            options: ParseOptions, packageData: PackageData, location: Location, rsc: RetainedStaticContext): Expression = {
//    val si = new SourceDocument(hrefExp, body, options)
//    si.setLocation(location)
//    si.setRetainedStaticContext(rsc)
//    si
//  }

  def getFocusTrackerFactory(exec: Executable, multithreaded: Boolean): SequenceIterator => FocusTrackingIterator =
    new FocusTrackingIterator(_)

  def isStreamedNode(node: NodeInfo): Boolean =
    false

  def getOptimizerOptions: OptimizerOptions =
    optimizerOptions.intersect(OptimizerOptions.FULL_HE_OPTIMIZATION)

  def obtainOptimizer: Optimizer =
    if (optimizer == null) {
      optimizer = new Optimizer(this)
      optimizer.setOptimizerOptions(optimizerOptions.intersect(OptimizerOptions.FULL_HE_OPTIMIZATION))
      optimizer
    } else
      optimizer

  /**
   * Factory method to get an Optimizer with specified optimizer options.
   * <p>This method is intended for internal use only.</p>
   *
   * @param options the optimizer options
   * @return a new optimizer with the specified options set (provided the optimizations
   *         are available in this Saxon configuration)
   */
  def obtainOptimizer(options: OptimizerOptions): Optimizer = {
    val optimizer = new Optimizer(this)
    optimizer.setOptimizerOptions(options.intersect(OptimizerOptions.FULL_HE_OPTIMIZATION))
    optimizer
  }

  /**
   * Factory method to make a ContextItemStaticInfo
   *
   * @param itemType       the item type of the context item. If the context item is absent, set this to
   *                       { @link org.orbeon.saxon.model.ErrorType#getInstance()}.
   * @param maybeUndefined set to true if it is possible (or certain) that the context item will be absent.
   * @return the ContextItemStaticInfo
   */
  def makeContextItemStaticInfo(itemType: ItemType, maybeUndefined: Boolean): ContextItemStaticInfo =
    new ContextItemStaticInfo(itemType, maybeUndefined)

  /**
   * Get a default ContextItemStaticInfo, indication no information is available about the context item
   * type
   *
   * @return the default ContextItemStaticInfo
   */
  def getDefaultContextItemStaticInfo: ContextItemStaticInfo = ContextItemStaticInfo.DEFAULT

  /**
   * Factory method to make an XQueryExpression
   *
   * @param exp        the expression forming the body of the query
   * @param mainModule the query module containing the expression
   * @param streaming  true if streamed execution is requested
   * @return the XQueryExpression
   * @throws XPathException if an error occurs
   */
  @throws[XPathException]
  def makeXQueryExpression(exp: Expression, mainModule: QueryModule, streaming: Boolean): XQueryExpression = {
    val xqe = new XQueryExpression(exp, mainModule, false)
    if (mainModule.getCodeInjector != null)
      mainModule.getCodeInjector.process(xqe)
    xqe
  }

  /**
   * Make a Closure, given the expected reference count
   *
   * @param expression the expression to be evaluated
   * @param ref        the (nominal) number of times the value of the expression is required
   * @param context    the XPath dynamic evaluation context
   * @return the constructed Closure
   * @throws XPathException if a failure occurs constructing the Closure
   */
  @throws[XPathException]
  def makeClosure(expression: Expression, ref: Int, context: XPathContext): Sequence = {
    if (getBooleanProperty(Feature.EAGER_EVALUATION)) {
      // Using eager evaluation can make for easier debugging
      val iter = expression.iterate(context)
      return iter.materialize
    }
    val closure =
      if (ref > 1)
        new MemoClosure
      else
        new Closure
    closure.setExpression(expression)
    closure.setSavedXPathContext(context.newContext())
    closure.saveContext(expression, context)
    closure
  }

  /**
   * Make a SequenceExtent, given the expected reference count
   *
   * @param expression the expression to be evaluated
   * @param ref        the (nominal) number of times the value of the expression is required
   * @param context    the XPath dynamic evaluation context
   * @return the constructed SequenceExtent
   * @throws XPathException if evaluation of the expression fails
   */
  @throws[XPathException]
  def makeSequenceExtent(expression: Expression, ref: Int, context: XPathContext): GroundedValue =
    expression.iterate(context).materialize

  def makeAccumulatorRegistry: AccumulatorRegistry = new AccumulatorRegistry

  /**
   * Register an external object model with this Configuration.
   *
   * @param model The external object model.
   *              This can either be one of the system-supplied external
   *              object models for JDOM, XOM, or DOM, or a user-supplied external object model.
   *              <p>This method is intended for advanced users only, and is subject to change.</p>
   */
  def registerExternalObjectModel(model: ExternalObjectModel): Unit = {
    try
      getConfClass(model.getDocumentClassName, tracing = false)
    catch {
      case _: XPathException =>
        // If the model can't be loaded, do nothing
        return
    }
    if (externalObjectModels == null)
      externalObjectModels = new java.util.ArrayList[ExternalObjectModel](4)
    if (! externalObjectModels.contains(model))
      externalObjectModels.add(model)
  }


  def getExternalObjectModel(uri: String): ExternalObjectModel = {
    for (model <- externalObjectModels.asScala)
      if (model.getIdentifyingURI.equals(uri))
        return model
    null
  }

  /**
   * Get the external object model that recognizes a particular class of node, if available
   *
   * @param nodeClass the class of the Node object in the external object model
   * @return the requested external object model if available, or null otherwise
   */
  def getExternalObjectModel(nodeClass: Class[_]): ExternalObjectModel = {
    for (model <- externalObjectModels.asScala) {
      val converter = model.getPJConverter(nodeClass)
      if (converter != null)
        return model
    }
    null
  }

  /**
   * Get all the registered external object models.
   * <p>This method is intended for internal use only.</p>
   *
   * @return a list of external object models supported. The members of the list are of
   *         type { @link ExternalObjectModel}
   */
  def getExternalObjectModels: util.List[ExternalObjectModel] = externalObjectModels

  /**
   * Get the JavaExternalObjectType object representing a particular Java class
   *
   * @param theClass the class in question
   * @return the corresponding JavaExternalObjectType
   */
  def getJavaExternalObjectType(theClass: Class[_]): JavaExternalObjectType = new JavaExternalObjectType(this, theClass)

  /**
   * Make a map representing the methods defined in a class. This map is specific to the class, not to
   * a particular instance. The functions present in this map take an extra first argument representing
   * the target instance; the functions returned in the final instance-level map will be partial applications
   * of the functions in the class-level map.
   *
   * @param javaClass the Java class whose methods are required
   * @param required  if non-null, indicates the key of the entry that is required in the map. If
   *                  this parameter is supplied, then the map will be limited to a single entry
   *                  with this key, since it is known that the other entries would never be used
   * @return a map whose entries represent public instance-level methods in the supplied Java class,
   *         to the extent that these methods have unique names.
   * @throws UnsupportedOperationException except in subclasses
   */
  def makeMethodMap(javaClass: Class[_], required: String): util.Map[String, Function[_, _]] = throw new UnsupportedOperationException

  /**
   * Convert a Java object to a map
   *
   * @param value    the (wrapped) Java object to be converted
   * @param required if non-null, indicates the key of the entry that is required in the map. If
   *                 this parameter is supplied, then the map will be limited to a single entry
   *                 with this key, since it is known that the other entries would never be used.
   * @return an XDM map containing entries representing the public instance-level methods
   *         available in the object, to the extent that they have unique names.
   * @throws UnsupportedOperationException except in subclasses
   */
  def externalObjectAsMap(value: ObjectValue[_], required: String): MapItem = throw new UnsupportedOperationException

  /**
   * Make an object lookup expression: supports the construct X?Y where X is an external Java object.
   * Requires Saxon-PE or higher
   *
   * @param lhs the left-hand operand
   * @param rhs the right-hand operand
   * @return the constructed expression
   * @throws XPathException if anything goes wrong
   */
  @throws[XPathException]
  def makeObjectLookupExpression(lhs: Expression, rhs: Expression): Expression = throw new UnsupportedOperationException

  /**
   * Get a NodeInfo corresponding to a DOM or other external Node,
   * either by wrapping or unwrapping the external Node.
   * <p>This method is intended for internal use.</p>
   *
   * @param source A Source representing the wrapped or unwrapped external Node. This will typically
   *               be a DOMSource, but it may be a similar Source recognized by some other registered external
   *               object model.
   * @return If the Source is a DOMSource and the underlying node is a wrapper around a Saxon NodeInfo,
   *         returns the wrapped Saxon NodeInfo. If the Source is a DOMSource and the undelying node is not such a wrapper,
   *         returns a new Saxon NodeInfo that wraps the DOM Node. If the Source is any other kind of source, it
   *         is offered to each registered external object model for similar treatment. The result is the
   *         NodeInfo object obtained by wrapping or unwrapping the supplied external node.
   * @throws IllegalArgumentException if the source object is not of a recognized class. This method does
   *                                  <em>not</em> call the registered { @link SourceResolver to resolve the Source}.
   */
  def unravel(source: Source): NodeInfo = {
    val externalObjectModels = getExternalObjectModels
    if (!source.isInstanceOf[NodeInfo]) {
      for (model <- externalObjectModels.asScala) {
        val node = model.unravel(source, this)
        if (node != null) {
          if (!node.getConfiguration.isCompatible(this))
            throw new IllegalArgumentException("Externally supplied Node belongs to the wrong Configuration")
          return node
        }
      }
    }
    source match {
      case nodeInfo: NodeInfo =>
        if (! nodeInfo.getConfiguration.isCompatible(this))
          throw new IllegalArgumentException("Externally supplied NodeInfo belongs to the wrong Configuration")
        return nodeInfo
      case _ =>
    }
    throw new IllegalArgumentException("A source of class " + source.getClass + " is not recognized by any registered object model")
  }

  /**
   * Ask whether an extension element with a particular name is available
   *
   * @param qName the extension element name
   * @return false (always, in the case of Saxon-HE)
   * @since 9.7
   */
  def isExtensionElementAvailable(qName: StructuredQName): Boolean = false

  /**
   * Set the StaticQueryContextFactory used for creating instances of StaticQueryContext
   *
   * @param factory the factory class to be used when a new StaticQueryContext is required.
   *                Note that this is not used for the default StaticQueryContext held in the Configuration itself.
   * @since 9.5.1.2
   */
  def setStaticQueryContextFactory(factory: StaticQueryContextFactory): Unit = staticQueryContextFactory = factory

  /**
   * Get a new StaticQueryContext (which is also the factory class for creating a query parser).
   * Note that this method is used to underpin the s9api and XQJ APIs for XQuery compilation, and
   * that modifying the behaviour of the StaticQueryContext can affect the behaviour of those APIs
   *
   * @return a new StaticQueryContext
   */
  def newStaticQueryContext: StaticQueryContext = makeStaticQueryContext(true)

  /**
   * Get a new Pending Update List
   *
   * @return the new Pending Update List
   * @throws UnsupportedOperationException if called when using Saxon-HE
   */
  def newPendingUpdateList: PendingUpdateList = throw new UnsupportedOperationException("XQuery update is supported only in Saxon-EE")

  /**
   * Make a PipelineConfiguration from the properties of this Configuration
   *
   * @return a new PipelineConfiguration
   * @since 8.4
   */
  def makePipelineConfiguration: PipelineConfiguration = {
    val pipe = new PipelineConfiguration(this)
    pipe.setURIResolver(getURIResolver)
    pipe.setParseOptions(new ParseOptions(defaultParseOptions))
    pipe.setErrorReporter(makeErrorReporter)
    pipe
  }

  /**
   * Make a SchemaURIResolver that wraps a supplied URIResolver
   *
   * @return a new SchemaURIResolver (or null if this is not an EnterpriseConfiguration)
   * @since 10.0
   */
  def makeSchemaURIResolver(resolver: URIResolver): SchemaURIResolver = null

  /**
   * Supply a SourceResolver. This is used for handling unknown implementations of the
   * {@link javax.xml.transform.Source} interface: a user-supplied SourceResolver can handle
   * such Source objects and translate them to a kind of Source that Saxon understands.
   *
   * @param resolver the source resolver.
   */
  def setSourceResolver(resolver: SourceResolver): Unit = sourceResolver = resolver

  /**
   * Get the current SourceResolver. If none has been supplied, a system-defined SourceResolver
   * is returned.
   *
   * @return the current SourceResolver
   */
  def getSourceResolver: SourceResolver = sourceResolver

  /**
   * Resolve a Source.
   *
   * @param source A source object, typically the source supplied as the first
   *               argument to { @link javax.xml.transform.Transformer#transform(javax.xml.transform.Source, javax.xml.transform.Result)}
   *               or similar methods.
   * @param config The Configuration. This provides the SourceResolver with access to
   *               configuration information; it also allows the SourceResolver to invoke the
   *               resolveSource() method on the Configuration object as a fallback implementation.
   * @return a source object that Saxon knows how to process. This must be an instance of one
   *         of the classes  StreamSource, SAXSource, DOMSource, { @link org.orbeon.saxon.lib.AugmentedSource},
   *         { @link org.orbeon.saxon.om.NodeInfo},
   *         or { @link org.orbeon.saxon.pull.PullSource}. Return null if the Source object is not
   *         recognized
   * @throws XPathException if the Source object is recognized but cannot be processed
   */
  @throws[XPathException]
  override def resolveSource(source: Source, config: Configuration): Source = {
    if (source.isInstanceOf[AugmentedSource]) return source
    if (source.isInstanceOf[StreamSource]) return source
    if (source.isInstanceOf[SAXSource]) return source
    if (source.isInstanceOf[DOMSource]) return source
    if (source.isInstanceOf[NodeInfo]) return source
    if (source.isInstanceOf[PullSource]) return source
//    if (source.isInstanceOf[StAXSource]) return source
    if (source.isInstanceOf[EventSource]) return source
    if (source.isInstanceOf[SaplingDocument]) return source
    null
  }

  /**
   * Build a document tree, using options set on this Configuration and on the supplied source
   * object. Options set on the source object override options set in the Configuration. The Source
   * object must be one of the kinds of source recognized by Saxon, or a source that can be resolved
   * using the registered {@link SourceResolver}. This method always constructs a new tree, it never
   * wraps or returns an existing tree.
   *
   * @param source the Source to be used. This may be an { @link AugmentedSource}, allowing options
   *               to be specified for the way in which this document will be built. If an AugmentedSource
   *               is supplied then options set in the AugmentedSource take precedence over options
   *               set in the Configuration.
   *               <p>If any error occurs reading or parsing the supplied Source, the error is notified
   *               to the { @link ErrorListener} registered with this { @link Configuration}.</p>
   * @return the constructed document as a TreeInfo
   * @throws XPathException if any errors occur during document parsing or validation. Detailed
   *                        errors occurring during schema validation will be written to the ErrorListener associated
   *                        with the AugmentedSource, if supplied, or with the Configuration otherwise.
   * @since 9.7; based on the original buildDocument(Source) method, but adapted to return the
   *        TreeInfo containing information about the constructed tree, including a reference to its root node.
   */
  @throws[XPathException]
  def buildDocumentTree(source: Source): TreeInfo =
    source match {
      case null => throw new NullPointerException("source")
      case augmentedSource: AugmentedSource => buildDocumentTree(augmentedSource.getContainedSource, augmentedSource.getParseOptions)
      case _                                => buildDocumentTree(source, new ParseOptions(defaultParseOptions))
    } // see bug 3678

  /**
   * Build a document, using specified options for parsing and building. This method always
   * constructs a new tree, it never wraps an existing document (regardless of anything in
   * the parseOptions)
   *
   * @param source       the source of the document to be constructed. If this is an
   *                     AugmentedSource, then any parser options contained in the AugmentedSource take precedence
   *                     over options specified in the parseOptions argument.
   * @param parseOptions options for parsing and constructing the document. Any options that
   *                     are not explicitly set in parseOptions default first to the values supplied in the source
   *                     argument if it is an AugmentedSource, and then to the values set in this Configuration.
   *                     The supplied parseOptions object is not modified.
   * @return the constructed document as a TreeInfo
   * @throws XPathException if parsing fails, or if the Source represents a node other than
   *                        a document node
   * @since 9.7; based on the original buildDocument(Source, ParseOptions) method, but adapted to return the
   *        TreeInfo containing information about the constructed tree, including a reference to its root node.
   */
  @throws[XPathException]
  def buildDocumentTree(source: Source, parseOptions: ParseOptions): TreeInfo = {
    var src: Source = source
    if (source == null)
      throw new NullPointerException("source")
    var finallyClose = false
    try {
      val options: ParseOptions = new ParseOptions(parseOptions)
      // Resolve user-defined implementations of Source
      val src2 = resolveSource(source, this)
      if (src2 == null)
        throw new XPathException("Unknown source class " + src.getClass.getName)
      src = src2
      if (source.isInstanceOf[AugmentedSource])
        options.merge(src.asInstanceOf[AugmentedSource].getParseOptions)
      options.applyDefaults(this)
      finallyClose = options.isPleaseCloseAfterUse
      // Create an appropriate Builder
      val treeModel = options.getModel
      // Decide whether line numbering is in use
      val lineNumbering = options.isLineNumbering
      val pipe = makePipelineConfiguration
      pipe.setParseOptions(options)
      val builder = treeModel.makeBuilder(pipe)
      builder.setTiming(isTiming)
      builder.setLineNumbering(lineNumbering)
      builder.setPipelineConfiguration(pipe)
      builder.setSystemId(src.getSystemId)
      Sender.send(src, builder, options)
      // Get the constructed document
      val newdoc = builder.getCurrentRoot
      if (newdoc.getNodeKind != Type.DOCUMENT)
        throw new XPathException("Source object represents a node other than a document node")
      // Reset the builder, detaching it from the constructed document
      builder.reset()
      // Return the constructed document
      newdoc.getTreeInfo
    } finally {
      // If requested, close the input stream
      if (finallyClose) {
        ParseOptions.close(source)
        ParseOptions.close(src)
      }
    }
  }

  /**
   * Get the collection of tree-builder statistics for this configuration, used
   * for learning suitable amounts of space to allocate for different kinds of tree
   *
   * @return the object in which tree statistics are accumulated
   */
  def getTreeStatistics: TreeStatistics = treeStatistics

  @throws[XPathException]
  def makeEmitter(clarkName: String, props: Properties): Receiver = {
    val brace = clarkName.indexOf('}')
    val localName = clarkName.substring(brace + 1)
    val colon = localName.indexOf(':')
    val className = localName.substring(colon + 1)
    var handler: Any = null
    try handler = dynamicLoader.getInstance(className, null)
    catch {
      case e: XPathException =>
        throw new XPathException("Cannot create user-supplied output method. " + e.getMessage, SaxonErrorCode.SXCH0004)
    }
    handler match {
      case receiver: Receiver => receiver
      case contentHandler: ContentHandler =>
        val emitter = new ContentHandlerProxy
        emitter.setUnderlyingContentHandler(contentHandler)
        emitter.setOutputProperties(props)
        emitter
      case _ => throw new XPathException("Output method " + className + " is neither a Receiver nor a SAX2 ContentHandler")
    }
  }

  /**
   * Set a property of the configuration. This method underpins the setAttribute() method of the
   * TransformerFactory implementation, and is provided
   * to enable setting of Configuration properties using URIs without instantiating a TransformerFactory:
   * specifically, this may be useful when running XQuery, and it is also used by the Validator API.
   *
   * <p>From Saxon 9.9, an alternative interface is available: {@link #setConfigurationProperty(Feature, Object)}.
   * The new interface is more efficient because it avoids expensive string comparisons. The old interface is
   * retained mainly for compatibility, and also because there are a few cases where it cannot easily be replaced,
   * for example when using composite feature URIs to delegate configuration options to the XML parser.</p>
   *
   * @param name  the URI identifying the property to be set. See the class { @link FeatureKeys} for
   *              constants representing the property names that can be set.
   * @param value the value of the property. Note that boolean values may be supplied either as a Boolean,
   *              or as one of the strings "0", "1", "true", "false", "yes", "no", "on", or "off".
   * @throws IllegalArgumentException if the property name is not recognized or if the value is not
   *                                  a valid value for the named property
   */
  def setConfigurationProperty(name: String, value: Any): Unit = {
    val feature = Feature.byName(name)
    if (feature == null) {
      if (name.startsWith(FeatureKeys.XML_PARSER_FEATURE)) {
        var uri = name.substring(FeatureKeys.XML_PARSER_FEATURE.length)
        try uri = URLDecoder.decode(uri, "utf-8")
        catch {
          case e: UnsupportedEncodingException =>
            throw new IllegalArgumentException(e)
        }
        defaultParseOptions.addParserFeature(uri, Configuration.requireBoolean(name, value))
      } else if (name.startsWith(FeatureKeys.XML_PARSER_PROPERTY)) {
        var uri = name.substring(FeatureKeys.XML_PARSER_PROPERTY.length)
        try uri = URLDecoder.decode(uri, "utf-8")
        catch {
          case e: UnsupportedEncodingException =>
            throw new IllegalArgumentException(e)
        }
        defaultParseOptions.addParserProperties(uri, value)
      } else
        throw new IllegalArgumentException("Unrecognized configuration feature: " + name)
    } else {
      setConfigurationProperty(feature.asInstanceOf[String], value)
    }
  }

  /**
   * Set a property of the configuration. This method underpins the setAttribute() method of the
   * TransformerFactory implementation, and is provided
   * to enable setting of Configuration properties using URIs without instantiating a TransformerFactory:
   * specifically, this may be useful when running XQuery, and it is also used by the Validator API
   *
   * @param feature the property to be set. See the class { @link Feature} for
   *                constants representing the property names that can be set.
   * @param value   the value of the property. Note that boolean values may be supplied either as a Boolean,
   *                or as one of the strings "0", "1", "true", "false", "yes", "no", "on", or "off".
   * @throws IllegalArgumentException if the property name is not recognized or if the value is not
   *                                  a valid value for the named property
   */
  def setConfigurationProperty[T](feature: Feature[T], value: T): Unit = {
    val name = feature.name
    if (Configuration.booleanFeatures.contains(feature)) {
      if (feature eq Feature.COMPILE_WITH_TRACING) {
        val b = Configuration.requireBoolean(name, value)
        setCompileWithTracing(b)
      }
      else if (feature eq Feature.DTD_VALIDATION) {
        val b = Configuration.requireBoolean(name, value)
        setValidation(b)
      }
      else if (feature eq Feature.EXPAND_ATTRIBUTE_DEFAULTS) {
        val b = Configuration.requireBoolean(name, value)
        setExpandAttributeDefaults(b)
      }
      internalSetBooleanProperty(feature, value)
    }
    else feature.code match {
      case FeatureCode.ALLOWED_PROTOCOLS =>
        allowedUriTest = ProtocolRestricter.make(value.asInstanceOf[String])
      case FeatureCode.COLLATION_URI_RESOLVER =>
        if (!value.isInstanceOf[CollationURIResolver]) throw new IllegalArgumentException("COLLATION_URI_RESOLVER value must be an instance of org.orbeon.saxon.lib.CollationURIResolver")
        setCollationURIResolver(value.asInstanceOf[CollationURIResolver])
      case FeatureCode.COLLATION_URI_RESOLVER_CLASS =>
        setCollationURIResolver(instantiateClassName(name, value, classOf[CollationURIResolver]).asInstanceOf[CollationURIResolver])
      case FeatureCode.COLLECTION_FINDER =>
        if (!value.isInstanceOf[CollectionFinder]) throw new IllegalArgumentException("COLLECTION_FINDER value must be an instance of org.orbeon.saxon.lib.ICollectionFinder")
        setCollectionFinder(value.asInstanceOf[CollectionFinder])
      case FeatureCode.COLLECTION_FINDER_CLASS =>
        setCollectionFinder(instantiateClassName(name, value, classOf[CollectionFinder]).asInstanceOf[CollectionFinder])
      case FeatureCode.DEFAULT_COLLATION =>
        defaultCollationName = value.toString
      case FeatureCode.DEFAULT_COLLECTION =>
        setDefaultCollection(value.toString)
      case FeatureCode.DEFAULT_COUNTRY =>
        setDefaultCountry(value.toString)
      case FeatureCode.DEFAULT_LANGUAGE =>
        setDefaultLanguage(value.toString)
      case FeatureCode.DEFAULT_REGEX_ENGINE =>
        setDefaultRegexEngine(value.toString)
      case FeatureCode.DTD_VALIDATION_RECOVERABLE =>
        val b = Configuration.requireBoolean(name, value)
        if (b) defaultParseOptions.setDTDValidationMode(Validation.LAX)
        else defaultParseOptions.setDTDValidationMode(if (isValidation) Validation.STRICT
        else Validation.SKIP)
        internalSetBooleanProperty(Feature.DTD_VALIDATION_RECOVERABLE, b)
      case FeatureCode.ENTITY_RESOLVER_CLASS =>
        if ("" == value) defaultParseOptions.setEntityResolver(null)
        else defaultParseOptions.setEntityResolver(instantiateClassName(name, value, classOf[EntityResolver]).asInstanceOf[EntityResolver])
      case FeatureCode.ENVIRONMENT_VARIABLE_RESOLVER =>
        if (!value.isInstanceOf[EnvironmentVariableResolver]) throw new IllegalArgumentException("ENVIRONMENT_VARIABLE_RESOLVER value must be an instance of org.orbeon.saxon.lib.EnvironmentVariableResolver")
        environmentVariableResolver = value.asInstanceOf[EnvironmentVariableResolver]
      case FeatureCode.ENVIRONMENT_VARIABLE_RESOLVER_CLASS =>
        environmentVariableResolver = instantiateClassName(name, value, classOf[EnvironmentVariableResolver]).asInstanceOf[EnvironmentVariableResolver]
      case FeatureCode.ERROR_LISTENER_CLASS =>
      // No action, obsolete
      case FeatureCode.LINE_NUMBERING =>
        val b = Configuration.requireBoolean(name, value)
        setLineNumbering(b)
      case FeatureCode.MESSAGE_EMITTER_CLASS =>
        if (!value.isInstanceOf[String]) throw new IllegalArgumentException("MESSAGE_EMITTER_CLASS class must be a String")
        //setMessageEmitterClass(value.asInstanceOf[String])
      case FeatureCode.MODULE_URI_RESOLVER =>
        if (!value.isInstanceOf[ModuleURIResolver]) throw new IllegalArgumentException("MODULE_URI_RESOLVER value must be an instance of org.orbeon.saxon.lib.ModuleURIResolver")
        setModuleURIResolver(value.asInstanceOf[ModuleURIResolver])
      case FeatureCode.MODULE_URI_RESOLVER_CLASS =>
        setModuleURIResolver(instantiateClassName(name, value, classOf[ModuleURIResolver]).asInstanceOf[ModuleURIResolver])
      case FeatureCode.NAME_POOL =>
        if (!value.isInstanceOf[NamePool]) throw new IllegalArgumentException("NAME_POOL value must be an instance of org.orbeon.saxon.om.NamePool")
        setNamePool(value.asInstanceOf[NamePool])
      case FeatureCode.OPTIMIZATION_LEVEL =>
        if (value.isInstanceOf[Integer]) { // See Saxon bug 2076. It seems Ant passes an integer value as an integer, not as a string. Not tested.
          // Integer values retained for compatibility: 0=none, 10 = all
          val v = value.asInstanceOf[Integer]
          optimizerOptions = if (v == 0) new OptimizerOptions(0)
          else OptimizerOptions.FULL_EE_OPTIMIZATION
        }
        else {
          val s = requireString(name, value)
          if (s.matches("[0-9]+")) { // For backwards compatibility
            optimizerOptions = if ("0" == s) new OptimizerOptions(0)
            else OptimizerOptions.FULL_EE_OPTIMIZATION
          }
          else optimizerOptions = new OptimizerOptions(s)
        }
        if (optimizer != null) optimizer.setOptimizerOptions(optimizerOptions)
        internalSetBooleanProperty(Feature.GENERATE_BYTE_CODE, optimizerOptions.isSet(OptimizerOptions.BYTE_CODE))
        //defaultXsltCompilerInfo.setOptimizerOptions(optimizerOptions)
      case FeatureCode.OUTPUT_URI_RESOLVER =>
        if (!value.isInstanceOf[OutputURIResolver]) throw new IllegalArgumentException("OUTPUT_URI_RESOLVER value must be an instance of org.orbeon.saxon.lib.OutputURIResolver")
        //setOutputURIResolver(value.asInstanceOf[OutputURIResolver])
      case FeatureCode.OUTPUT_URI_RESOLVER_CLASS =>
        //setOutputURIResolver(instantiateClassName(name, value, classOf[OutputURIResolver]).asInstanceOf[OutputURIResolver])
      case FeatureCode.RECOGNIZE_URI_QUERY_PARAMETERS =>
        val b = Configuration.requireBoolean(name, value)
        getSystemURIResolver.setRecognizeQueryParameters(b)
      case FeatureCode.RECOVERY_POLICY =>
      // Obsolete: no action
      case FeatureCode.RECOVERY_POLICY_NAME =>
      case FeatureCode.REGEX_BACKTRACKING_LIMIT =>
        regexBacktrackingLimit = requireInteger(name, value)
      case FeatureCode.SERIALIZER_FACTORY_CLASS =>
        setSerializerFactory(instantiateClassName(name, value, classOf[SerializerFactory]).asInstanceOf[SerializerFactory])
      case FeatureCode.SCHEMA_VALIDATION =>
        setSchemaValidationMode(requireInteger(feature.name, value))
      case FeatureCode.SCHEMA_VALIDATION_MODE =>
        val mode = requireString(feature.name, value)
        setSchemaValidationMode(Validation.getCode(mode))
      case FeatureCode.SOURCE_PARSER_CLASS =>
        setSourceParserClass(requireString(feature.name, value))
      case FeatureCode.SOURCE_RESOLVER_CLASS =>
        setSourceResolver(instantiateClassName(name, value, classOf[SourceResolver]).asInstanceOf[SourceResolver])
      // ORBEON: No `File` support.
//      case FeatureCode.STANDARD_ERROR_OUTPUT_FILE =>
//        // Note, this property is write-only
//        try {
//          val append = true
//          val autoFlush = true
//          setStandardErrorOutput(new PrintStream(new FileOutputStream(value.asInstanceOf[String], append), autoFlush))
//        } catch {
//          case fnf: FileNotFoundException =>
//            throw new IllegalArgumentException(fnf)
//        }
      case FeatureCode.STRIP_WHITESPACE =>
        val s = requireString(name, value)
        s match {
          case "all" =>
            defaultParseOptions.setSpaceStrippingRule(AllElementsSpaceStrippingRule.getInstance)
          case "none" =>
            defaultParseOptions.setSpaceStrippingRule(NoElementsSpaceStrippingRule.getInstance)
          case "ignorable" =>
            defaultParseOptions.setSpaceStrippingRule(IgnorableSpaceStrippingRule.getInstance)
          case _ =>
            throw new IllegalArgumentException("Unrecognized value STRIP_WHITESPACE = '" + value + "': must be 'all', 'none', or 'ignorable'")
        }
      case FeatureCode.STYLE_PARSER_CLASS =>
        setStyleParserClass(requireString(name, value))
      case FeatureCode.TIMING =>
        setTiming(Configuration.requireBoolean(name, value))
      case FeatureCode.TRACE_LISTENER =>
        if (! value.isInstanceOf[TraceListener])
          throw new IllegalArgumentException("TRACE_LISTENER is of wrong class")
        setTraceListener(value.asInstanceOf[TraceListener])
      case FeatureCode.TRACE_LISTENER_CLASS =>
        setTraceListenerClass(requireString(name, value))
      case FeatureCode.TRACE_LISTENER_OUTPUT_FILE =>
        setTraceListenerOutputFile(requireString(name, value))
      case FeatureCode.TREE_MODEL =>
        setTreeModel(requireInteger(name, value))
      case FeatureCode.TREE_MODEL_NAME =>
        val s = requireString(name, value)
        import org.orbeon.saxon.event.Builder._
        s match {
          case "tinyTree" =>
            setTreeModel(TINY_TREE)
          case "tinyTreeCondensed" =>
            setTreeModel(TINY_TREE_CONDENSED)
          case "linkedTree" =>
            setTreeModel(LINKED_TREE)
          case "jdom" =>
            setTreeModel(JDOM_TREE)
          case "jdom2" =>
            setTreeModel(JDOM2_TREE)
          case _ =>
            throw new IllegalArgumentException("Unrecognized value TREE_MODEL_NAME = '" + value + "': must be linkedTree|tinyTree|tinyTreeCondensed")
        }
      case FeatureCode.UNPARSED_TEXT_URI_RESOLVER =>
        setUnparsedTextURIResolver(value.asInstanceOf[UnparsedTextURIResolver])
      case FeatureCode.UNPARSED_TEXT_URI_RESOLVER_CLASS =>
        setUnparsedTextURIResolver(instantiateClassName(name, value, classOf[UnparsedTextURIResolver]).asInstanceOf[UnparsedTextURIResolver])
      case FeatureCode.URI_RESOLVER_CLASS =>
        setURIResolver(instantiateClassName(name, value, classOf[URIResolver]).asInstanceOf[URIResolver])
      case FeatureCode.USE_XSI_SCHEMA_LOCATION =>
        defaultParseOptions.setUseXsiSchemaLocation(Configuration.requireBoolean(name, value))
      case FeatureCode.VALIDATION_COMMENTS =>
        defaultParseOptions.setAddCommentsAfterValidationErrors(Configuration.requireBoolean(name, value))
      case FeatureCode.VALIDATION_WARNINGS =>
        setValidationWarnings(Configuration.requireBoolean(name, value))
      case FeatureCode.VERSION_WARNING =>
      case FeatureCode.XINCLUDE =>
        setXIncludeAware(Configuration.requireBoolean(name, value))
      case FeatureCode.XPATH_VERSION_FOR_XSD =>
        val `val`: Int = requireInteger(name, value)
        if (`val` != 20 && `val` != 30 && `val` != 31) throw new IllegalArgumentException("XPath version for XSD must be 20 (XPath 2.0), 30 (XPath 3.0), or 31 (XPath 3.1)")
        xpathVersionForXsd = `val`
      case FeatureCode.XPATH_VERSION_FOR_XSLT =>
        val `val` = requireInteger(name, value)
        if (`val` != 20 && `val` != 30 && `val` != 305 && `val` != 31) throw new IllegalArgumentException("XPath version for XSLT must be 20 (XPath 2.0), 30 (XPath 3.0), 31 (XPath 3.1), or 305 (XPath 3.0 with XSLT-defined extensions)")
        xpathVersionForXslt = `val`
      case FeatureCode.XQUERY_ALLOW_UPDATE =>
        getDefaultStaticQueryContext.setUpdatingEnabled(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_CONSTRUCTION_MODE =>
        getDefaultStaticQueryContext.setConstructionMode(Validation.getCode(value.toString))
      case FeatureCode.XQUERY_DEFAULT_ELEMENT_NAMESPACE =>
        getDefaultStaticQueryContext.setDefaultElementNamespace(value.toString)
      case FeatureCode.XQUERY_DEFAULT_FUNCTION_NAMESPACE =>
        getDefaultStaticQueryContext.setDefaultFunctionNamespace(value.toString)
      case FeatureCode.XQUERY_EMPTY_LEAST =>
        getDefaultStaticQueryContext.setEmptyLeast(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_INHERIT_NAMESPACES =>
        getDefaultStaticQueryContext.setInheritNamespaces(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_PRESERVE_BOUNDARY_SPACE =>
        getDefaultStaticQueryContext.setPreserveBoundarySpace(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_PRESERVE_NAMESPACES =>
        getDefaultStaticQueryContext.setPreserveNamespaces(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_REQUIRED_CONTEXT_ITEM_TYPE =>
        val parser: XPathParser = new XPathParser
        parser.setLanguage(ParsedLanguage.SEQUENCE_TYPE, 31)
        try {
          val `type` = parser.parseSequenceType(value.toString, new IndependentContext(this))
          if (`type`.getCardinality != StaticProperty.EXACTLY_ONE) throw new IllegalArgumentException("Context item type must have no occurrence indicator")
          getDefaultStaticQueryContext.setRequiredContextItemType(`type`.getPrimaryType)
        } catch {
          case err: XPathException =>
            throw new IllegalArgumentException(err)
        }
      case FeatureCode.XQUERY_SCHEMA_AWARE =>
        getDefaultStaticQueryContext.setSchemaAware(Configuration.requireBoolean(name, value))
      case FeatureCode.XQUERY_STATIC_ERROR_LISTENER_CLASS =>
        getDefaultStaticQueryContext.setErrorListener(instantiateClassName(name, value, classOf[ErrorListener]).asInstanceOf[ErrorListener])
      case FeatureCode.XQUERY_VERSION =>
        if (!("3.1" == value)) makeErrorReporter.report(new XmlProcessingIncident("XQuery version ignored: only \"3.1\" is recognized").asWarning())
      //getDefaultStaticQueryContext().setLanguageVersion(31);
      case FeatureCode.XML_VERSION =>
        val xv = requireString(name, value)
        if (!(xv == "1.0" || xv == "1.1")) throw new IllegalArgumentException("XML_VERSION value must be \"1.0\" or \"1.1\" as a String")
        setXMLVersion(if (xv == "1.0") Configuration.XML10
        else Configuration.XML11)
      case FeatureCode.XSD_VERSION =>
        val vn = requireString(name, value)
        if (!(vn == "1.0" || vn == "1.1")) throw new IllegalArgumentException("XSD_VERSION value must be \"1.0\" or \"1.1\" as a String")
        xsdVersion = if (value == "1.0") Configuration.XSD10
        else Configuration.XSD11
        theConversionRules = null
//      case FeatureCode.XSLT_ENABLE_ASSERTIONS =>
        //getDefaultXsltCompilerInfo.setAssertionsEnabled(Configuration.requireBoolean(name, value))
//      case FeatureCode.XSLT_INITIAL_MODE =>
//        val s = requireString(name, value)
        //getDefaultXsltCompilerInfo.setDefaultInitialMode(StructuredQName.fromClarkName(s))
//      case FeatureCode.XSLT_INITIAL_TEMPLATE =>
//        val s = requireString(name, value)
        //getDefaultXsltCompilerInfo.setDefaultInitialTemplate(StructuredQName.fromClarkName(s))
//      case FeatureCode.XSLT_SCHEMA_AWARE =>
//        getDefaultXsltCompilerInfo.setSchemaAware(Configuration.requireBoolean(name, value))
//      case FeatureCode.XSLT_STATIC_ERROR_LISTENER_CLASS =>
//        getDefaultXsltCompilerInfo.setErrorListener(instantiateClassName(name, value, classOf[ErrorListener]).asInstanceOf[ErrorListener])
//      case FeatureCode.XSLT_STATIC_URI_RESOLVER_CLASS =>
//        getDefaultXsltCompilerInfo.setURIResolver(instantiateClassName(name, value, classOf[URIResolver]).asInstanceOf[URIResolver])
//      case FeatureCode.XSLT_VERSION =>
//        if (!("3.0" == value)) makeErrorReporter.report(new XmlProcessingIncident("XSLT version ignored: only \"3.0\" is recognized").asWarning)
      //getDefaultXsltCompilerInfo().setXsltVersion(v);
      case _ =>
        throw new IllegalArgumentException("Unknown configuration property " + name)
    }
  }

  /**
   * Validate a property value where the required type is integer
   *
   * @param propertyName the name of the property
   * @param value        the supplied value of the property. This may be either a java.lang.Integer, or a string
   *                     that can be parsed as an integer (suited to the conventions of different
   *                     configuration APIs that end up calling this method)
   * @return the value as an integer
   * @throws IllegalArgumentException if the supplied value cannot be validated as a recognized boolean value
   */
   def requireInteger(propertyName: String, value: Any): Int =
     value match {
       case integer: Integer => integer
       case string: String =>
         try string.toInt
         catch {
           case _: NumberFormatException =>
             throw new IllegalArgumentException(propertyName + " must be an integer")
         }
       case _ =>
         throw new IllegalArgumentException(propertyName + " must be an integer (or a string representing an integer)")
     }

  /**
   * Set a boolean property value, without checking that it is a recognized property name
   *
   * @param property the name of the property to be set
   * @param value    a representation of the boolean value.
   *                 This may be either a java.lang.Boolean, or a string
   *                 taking one of the values on|off, true|false, yes|no, or 1|0 (suited to the conventions of different
   *                 configuration APIs that end up calling this method)
   */
   def internalSetBooleanProperty(property: Feature[_], value: Any): Boolean = {
    val b = Configuration.requireBoolean(property.name, value)
    if (b)
      enabledProperties.add(property.code)
    else
      enabledProperties.remove(property.code)
  }

  /**
   * Get a boolean property of the configuration
   *
   * @param feature the integer code of the required property. See the class { @link FeatureCode} for
   *                constants representing the property names that can be requested. This class only recognizes
   *                properties whose type is boolean.
   * @return the value of the property. In the case of an unrecognized property name, the value returned is
   *         false: no error is thrown.
   */
  def getBooleanProperty(feature: Feature[_]): Boolean = enabledProperties.contains(feature.code)

  /**
   * Set a boolean property of the configuration
   *
   * @param propertyName the name of the required property. See the class { @link FeatureKeys} for
   *                     constants representing the property names that can be requested. This class only recognizes
   *                     properties whose type is boolean.
   * @param value        the value of the property.
   * @throws IllegalArgumentException if the property name is not recognized (as a property whose expected
   *                                  value is boolean)
   */
  def setBooleanProperty(propertyName: String, value: Boolean): Unit = setConfigurationProperty(propertyName, value)

  /**
   * Set a boolean property of the configuration
   *
   * @param feature the required property. See the class { @link Feature} for
   *                constants representing the property names that can be requested. This class only recognizes
   *                properties whose type is boolean.
   * @param value   the value of the property.
   * @throws IllegalArgumentException if the feature is not recognized (as a feature whose expected
   *                                  value is boolean)
   */
  def setBooleanProperty(feature: Feature[Boolean], value: Boolean): Unit = setConfigurationProperty(feature, value)

   def requireString(propertyName: String, value: Any): String = if (value.isInstanceOf[String]) value.asInstanceOf[String]
  else throw new IllegalArgumentException("The value of " + propertyName + " must be a string")

   def instantiateClassName(propertyName: String, value: Any, requiredClass: Class[_]): Any = {
    if (!value.isInstanceOf[String]) throw new IllegalArgumentException(propertyName + " must be a String")
    try {
      val obj = getInstance(value.asInstanceOf[String])
      if (!requiredClass.isAssignableFrom(obj.getClass)) throw new IllegalArgumentException("Error in " + propertyName + ": Class " + value + " does not implement " + requiredClass.getName)
      obj
    } catch {
      case err: XPathException =>
        throw new IllegalArgumentException("Cannot use " + value + " as the value of " + propertyName + ". " + err.getMessage)
    }
  }

  /**
   * Get a property of the configuration
   *
   * @param name the name of the required property. See the class { @link FeatureKeys} for
   *             constants representing the property names that can be requested.
   * @return the value of the property. Note that boolean values are returned as a Boolean,
   *         even if the value was supplied as a string (for example "true" or "on").
   * @throws IllegalArgumentException thrown if the property is not one that Saxon recognizes.
   */
  def getConfigurationProperty(name: String): Any = {
    val feature = Feature.byName(name)
    if (feature == null)
      throw new IllegalArgumentException("Unknown configuration property " + name)
    else
      getConfigurationProperty(feature)
  }

  @SuppressWarnings(Array("unchecked")) def getConfigurationProperty[T](feature: Feature[T]): T = {
    if (Configuration.booleanFeatures.contains(feature)) return java.lang.Boolean.valueOf(getBooleanProperty(feature)).asInstanceOf[T]
    feature.code match {
      case FeatureCode.ALLOWED_PROTOCOLS =>
        if (allowedUriTest.isInstanceOf[ProtocolRestricter]) return allowedUriTest.toString.asInstanceOf[T]
        else return "all".asInstanceOf[T]
      case FeatureCode.COLLATION_URI_RESOLVER =>
        return getCollationURIResolver.asInstanceOf[T]
      case FeatureCode.COLLATION_URI_RESOLVER_CLASS =>
        return getCollationURIResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.CONFIGURATION =>
        return this.asInstanceOf[T]
      case FeatureCode.DEFAULT_COLLATION =>
        return defaultCollationName.asInstanceOf[T]
      case FeatureCode.DEFAULT_COLLECTION =>
        return getDefaultCollection.asInstanceOf[T]
      case FeatureCode.DEFAULT_COUNTRY =>
        return getDefaultCountry.asInstanceOf[T]
      case FeatureCode.DEFAULT_LANGUAGE =>
        return getDefaultLanguage.asInstanceOf[T]
      case FeatureCode.DTD_VALIDATION =>
        return java.lang.Boolean.valueOf(isValidation).asInstanceOf[T]
      case FeatureCode.DTD_VALIDATION_RECOVERABLE =>
        return java.lang.Boolean.valueOf(defaultParseOptions.getDTDValidationMode == Validation.LAX).asInstanceOf[T]
      case FeatureCode.ERROR_LISTENER_CLASS =>
        // Obsolete
        return null.asInstanceOf[T]
      case FeatureCode.ENTITY_RESOLVER_CLASS =>
        val er = defaultParseOptions.getEntityResolver
        if (er == null) return "".asInstanceOf[T]
        else return er.getClass.getName.asInstanceOf[T]
      case FeatureCode.ENVIRONMENT_VARIABLE_RESOLVER =>
        return environmentVariableResolver.asInstanceOf[T]
      case FeatureCode.ENVIRONMENT_VARIABLE_RESOLVER_CLASS =>
        return environmentVariableResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.EXPAND_ATTRIBUTE_DEFAULTS =>
        return java.lang.Boolean.valueOf(isExpandAttributeDefaults).asInstanceOf[T]
      case FeatureCode.LINE_NUMBERING =>
        return java.lang.Boolean.valueOf(isLineNumbering).asInstanceOf[T]
//      case FeatureCode.MESSAGE_EMITTER_CLASS =>
//        return getMessageEmitterClass.asInstanceOf[T]
      case FeatureCode.MODULE_URI_RESOLVER =>
        return getModuleURIResolver.asInstanceOf[T]
      case FeatureCode.MODULE_URI_RESOLVER_CLASS =>
        return getModuleURIResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.NAME_POOL =>
        return getNamePool.asInstanceOf[T]
      case FeatureCode.OPTIMIZATION_LEVEL =>
        return optimizerOptions.toString.asInstanceOf[T]
//      case FeatureCode.OUTPUT_URI_RESOLVER =>
//        return getOutputURIResolver.asInstanceOf[T]
//      case FeatureCode.OUTPUT_URI_RESOLVER_CLASS =>
//        return getOutputURIResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.RECOGNIZE_URI_QUERY_PARAMETERS =>
        return java.lang.Boolean.valueOf(getSystemURIResolver.queryParametersAreRecognized).asInstanceOf[T]
      case FeatureCode.RECOVERY_POLICY =>
        return Integer.valueOf(0).asInstanceOf[T]
      case FeatureCode.RECOVERY_POLICY_NAME =>
        return "recoverWithWarnings".asInstanceOf[T]
      case FeatureCode.REGEX_BACKTRACKING_LIMIT =>
        return Integer.valueOf(regexBacktrackingLimit).asInstanceOf[T]
      case FeatureCode.SCHEMA_VALIDATION =>
        return Integer.valueOf(getSchemaValidationMode).asInstanceOf[T]
      case FeatureCode.SCHEMA_VALIDATION_MODE =>
        return Validation.toString(getSchemaValidationMode).asInstanceOf[T]
      case FeatureCode.SERIALIZER_FACTORY_CLASS =>
        return getSerializerFactory.getClass.getName.asInstanceOf[T]
      case FeatureCode.SOURCE_PARSER_CLASS =>
        return getSourceParserClass.asInstanceOf[T]
      case FeatureCode.SOURCE_RESOLVER_CLASS =>
        return getSourceResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.STRIP_WHITESPACE =>
        val rule = getParseOptions.getSpaceStrippingRule
        if (rule eq AllElementsSpaceStrippingRule.getInstance) return "all".asInstanceOf[T]
        else if (rule == null || (rule eq IgnorableSpaceStrippingRule.getInstance)) return "ignorable".asInstanceOf[T]
        else return "none".asInstanceOf[T]
      case FeatureCode.STYLE_PARSER_CLASS =>
        return getStyleParserClass.asInstanceOf[T]
      case FeatureCode.TIMING =>
        return java.lang.Boolean.valueOf(isTiming).asInstanceOf[T]
      case FeatureCode.TRACE_LISTENER =>
        return traceListener.asInstanceOf[T]
      case FeatureCode.TRACE_LISTENER_CLASS =>
        return traceListenerClass.asInstanceOf[T]
      case FeatureCode.TRACE_LISTENER_OUTPUT_FILE =>
        return traceListenerOutput.asInstanceOf[T]
      case FeatureCode.TREE_MODEL =>
        return Integer.valueOf(getTreeModel).asInstanceOf[T]
      case FeatureCode.TREE_MODEL_NAME =>
        import org.orbeon.saxon.event.Builder._
        getTreeModel match {
          case TINY_TREE =>
          case TINY_TREE_CONDENSED => return "tinyTreeCondensed".asInstanceOf[T]
          case LINKED_TREE => return "linkedTree".asInstanceOf[T]
          case _ => return "tinyTree".asInstanceOf[T]
        }
      case FeatureCode.UNPARSED_TEXT_URI_RESOLVER =>
        return getUnparsedTextURIResolver.asInstanceOf[T]
      case FeatureCode.UNPARSED_TEXT_URI_RESOLVER_CLASS =>
        return getUnparsedTextURIResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.URI_RESOLVER_CLASS =>
        return getURIResolver.getClass.getName.asInstanceOf[T]
      case FeatureCode.USE_XSI_SCHEMA_LOCATION =>
        return java.lang.Boolean.valueOf(defaultParseOptions.isUseXsiSchemaLocation).asInstanceOf[T]
      case FeatureCode.VALIDATION_COMMENTS =>
        return java.lang.Boolean.valueOf(defaultParseOptions.isAddCommentsAfterValidationErrors).asInstanceOf[T]
      case FeatureCode.VALIDATION_WARNINGS =>
        return java.lang.Boolean.valueOf(isValidationWarnings).asInstanceOf[T]
      case FeatureCode.VERSION_WARNING =>
        return java.lang.Boolean.valueOf(false).asInstanceOf[T]
      case FeatureCode.XINCLUDE =>
        return java.lang.Boolean.valueOf(isXIncludeAware).asInstanceOf[T]
      case FeatureCode.XML_VERSION =>
        return (if (getXMLVersion == Configuration.XML10) "1.0"
        else "1.1").asInstanceOf[T]
      case FeatureCode.XQUERY_ALLOW_UPDATE =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isUpdatingEnabled).asInstanceOf[T]
      case FeatureCode.XQUERY_CONSTRUCTION_MODE =>
        return Integer.valueOf(getDefaultStaticQueryContext.getConstructionMode).asInstanceOf[T]
      case FeatureCode.XQUERY_DEFAULT_ELEMENT_NAMESPACE =>
        return getDefaultStaticQueryContext.getDefault_ElementNamespace.asInstanceOf[T]
      case FeatureCode.XQUERY_DEFAULT_FUNCTION_NAMESPACE =>
        return getDefaultStaticQueryContext.getDefaultFunctionNamespace.asInstanceOf[T]
      case FeatureCode.XQUERY_EMPTY_LEAST =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isEmptyLeast).asInstanceOf[T]
      case FeatureCode.XQUERY_INHERIT_NAMESPACES =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isInheritNamespaces).asInstanceOf[T]
      case FeatureCode.XQUERY_PRESERVE_BOUNDARY_SPACE =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isPreserveBoundarySpace).asInstanceOf[T]
      case FeatureCode.XQUERY_PRESERVE_NAMESPACES =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isPreserveNamespaces).asInstanceOf[T]
      case FeatureCode.XQUERY_REQUIRED_CONTEXT_ITEM_TYPE =>
        return getDefaultStaticQueryContext.getRequiredContextItemType.asInstanceOf[T]
      case FeatureCode.XQUERY_SCHEMA_AWARE =>
        return java.lang.Boolean.valueOf(getDefaultStaticQueryContext.isSchemaAware).asInstanceOf[T]
      case FeatureCode.XQUERY_STATIC_ERROR_LISTENER_CLASS =>
        return getDefaultStaticQueryContext.getErrorListener.getClass.getName.asInstanceOf[T]
      case FeatureCode.XQUERY_VERSION =>
        return "3.1".asInstanceOf[T]
      case FeatureCode.XPATH_VERSION_FOR_XSD =>
        return xpathVersionForXsd.asInstanceOf[Integer].asInstanceOf[T]
      case FeatureCode.XPATH_VERSION_FOR_XSLT =>
        return xpathVersionForXslt.asInstanceOf[Integer].asInstanceOf[T]
      case FeatureCode.XSD_VERSION =>
        return (if (xsdVersion == Configuration.XSD10) "1.0"
        else "1.1").asInstanceOf[T]
//      case FeatureCode.XSLT_ENABLE_ASSERTIONS =>
//        return java.lang.Boolean.valueOf(getDefaultXsltCompilerInfo.isAssertionsEnabled).asInstanceOf[T]
//      case FeatureCode.XSLT_INITIAL_MODE =>
//        return getDefaultXsltCompilerInfo.getDefaultInitialMode.getClarkName.asInstanceOf[T]
//      case FeatureCode.XSLT_INITIAL_TEMPLATE =>
//        return getDefaultXsltCompilerInfo.getDefaultInitialTemplate.getClarkName.asInstanceOf[T]
//      case FeatureCode.XSLT_SCHEMA_AWARE =>
//        return java.lang.Boolean.valueOf(getDefaultXsltCompilerInfo.isSchemaAware).asInstanceOf[T]
//      case FeatureCode.XSLT_STATIC_ERROR_LISTENER_CLASS =>
//        return getDefaultXsltCompilerInfo.getErrorListener.getClass.getName.asInstanceOf[T]
//      case FeatureCode.XSLT_STATIC_URI_RESOLVER_CLASS =>
//        return getDefaultXsltCompilerInfo.getURIResolver.getClass.getName.asInstanceOf[T]
//      case FeatureCode.XSLT_VERSION =>
//        return Integer.valueOf(30).asInstanceOf[T]
    }
    throw new IllegalArgumentException("Unknown configuration property " + feature.name)
  }

  /**
   * Ask whether bytecode should be generated. The default setting
   * is true in Saxon Enterprise Edition and false in all other cases. Setting the option to
   * true has no effect if Saxon-EE is not available (but if it is set to true, this method will
   * return true). Setting the option to false in Saxon-EE
   * is permitted if for some reason bytecode generation is to be suppressed (one possible reason
   * is to improve compilation performance at the expense of evaluation performance).
   *
   * @param hostLanguage one of XSLT or XQUERY
   * @return true if the option is switched on
   */
  def isGenerateByteCode(hostLanguage: HostLanguage): Boolean = false

  /**
   * Ask whether bytecode should be generated in Just-In-time compilation and therefore deferring the byte code generation. The default setting
   * is false. Setting the option to
   * true has no effect if Saxon-EE is not available (but if it is set to true, this method will
   * return true). Setting the option to false in Saxon-EE
   * is permitted and therefore byte code generation will be generated in the compile phase.
   *
   * @param hostLanguage one of XSLT or XQUERY
   * @return true if the option is switched on
   */
  def isDeferredByteCode(hostLanguage: HostLanguage): Boolean = false

  /**
   * Ask whether just-in-time compilation of XSLT template rules is in force
   *
   * @return true if just-in-time compilation is enabled (this is the default in Saxon-EE and
   *         not available in other configurations)
   */
  def isJITEnabled: Boolean = false

  /**
   * Close any resources held by the Configuration. This implementation
   * closes the Logger and/or trace output file if one has been allocated.
   */
  def close(): Unit = if (traceOutput != null) traceOutput.close()

  /**
   * Create a package loader, for reloading SEF files, appropriate to the Saxon edition being used
   *
   * @return a package loader
   */
//  def makePackageLoader: IPackageLoader = new PackageLoaderHE(this)

  /**
   * Register a report generator for reporting invalidities detected in the course
   * of schema validation
   *
   * @return a report generator.
   * @throws UnsupportedOperationException (always) in Saxon-HE
   */
  def createValidityReporter: InvalidityReportGenerator = throw new UnsupportedOperationException("Schema validation requires Saxon-EE")

  /**
   * Get the threshold for generating byte code
   *
   * @return a value indicating the number of times an expression should be evaluated intepretatively before
   *         it is optimized by generating bytecode
   */
  def getCountDown: Int = byteCodeThreshold

//  /**
//   * Make a new Mode - this can be overridden in subclasses to produce optimized variants
//   *
//   * @param modeName     the name of the mode
//   * @param compilerInfo information on the compiler, that can alter rule optimization
//   * @return an instantiated Mode
//   */
//  def makeMode(modeName: StructuredQName, compilerInfo: CompilerInfo): SimpleMode = new SimpleMode(modeName)

  /**
   * Factory method to create a Template Rule
   *
   * @return a new TemplateRule appropriate to this Configuration
   */
  def makeTemplateRule: TemplateRule = new TemplateRule

  /**
   * Make a ThreadManager for asynchronous xsl:result-document instructions
   *
   * @return a new ThreadManager (or null in the case of Saxon-HE)
   */
  def makeThreadManager: XPathContextMajor.ThreadManager = null

  /**
   * Make an XSLT CompilerInfo object - can be overridden in a subclass to produce variants
   * capable of optimization
   *
   * @return a new CompilerInfo object
   */
//  def makeCompilerInfo: CompilerInfo = new CompilerInfo(this)

  /**
   * Make a CompilerService object, to handle byte code generation, or null if byte code
   * generation is not available
   *
   * @param hostLanguage eg Configuration.XSLT
   * @return a CompilerService, or null
   */
  def makeCompilerService(hostLanguage: HostLanguage): ICompilerService = null

  /**
   * Generate a report on byte code instrumentation to a specified file
   *
   * @param fileName the specified file name
   */
  def createByteCodeReport(fileName: String): Unit = {
  }

  /**
   * Set a label for this configuration
   *
   * @param label the label to associate
   */
  def setLabel(label: String): Unit = this.label = label

  /**
   * Get the associated label for this configuration (typically, the value of the <code>@label</code>
   * attribute in the configuration file)
   *
   * @return the associated label
   */
  def getLabel: String = label
}
