package net.sf.saxon.trans

import java.io.File
import java.net.{URI, URISyntaxException}
import java.util._

import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import net.sf.saxon.event.{ContentHandlerProxy, ReceiverOption}
import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.instruct.ResultDocument
import net.sf.saxon.expr.parser.{Loc, Token, XPathParser}
import net.sf.saxon.functions.ResolveURI
import net.sf.saxon.lib._
import net.sf.saxon.om._
import net.sf.saxon.s9api.XmlProcessingError
import net.sf.saxon.sxpath.IndependentContext
import net.sf.saxon.trans.packages.{PackageDetails, PackageLibrary, VersionedPackageName}
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.utils.{Configuration, Version}
import org.xml.sax._

import scala.jdk.CollectionConverters._


class ConfigurationReader extends ContentHandler with NamespaceResolver {

  private var level: Int = 0

  private var section: String = null

  private var subsection: String = null

  private var buffer: FastStringBuffer = new FastStringBuffer(100)

   var config: Configuration = _

  private var classLoader: ClassLoader = null

  private var errors: List[XmlProcessingError] = new ArrayList()

  private var locator: Locator = _

  private var namespaceStack: Stack[List[Array[String]]] = new Stack()

  private var packageLibrary: PackageLibrary = _

  private var currentPackage: PackageDetails = _

  private var baseConfiguration: Configuration = _

  private var documentLocator: Locator =  null

  def setClassLoader(classLoader: ClassLoader): Unit = {
    this.classLoader = classLoader
  }

  def setBaseConfiguration(base: Configuration): Unit = {
    this.baseConfiguration = base
  }

  def makeConfiguration(source: Source): Configuration = {
    if (source.isInstanceOf[NodeInfo]) {
      val proxy: ContentHandlerProxy = new ContentHandlerProxy() {
        override def startDocument(properties: Int): Unit = {
          getUnderlyingContentHandler.startDocument()
        }

        override def endDocument(): Unit = {
          getUnderlyingContentHandler.endDocument()
        }
      }
      proxy.setUnderlyingContentHandler(this)
      proxy.setPipelineConfiguration(
        source
          .asInstanceOf[NodeInfo]
          .getConfiguration
          .makePipelineConfiguration)
      proxy.open()
      this.documentLocator = new Loc(source.getSystemId, -1, -1)
      proxy.startDocument(ReceiverOption.NONE)
      source
        .asInstanceOf[NodeInfo]
        .copy(proxy, CopyOptions.ALL_NAMESPACES, Loc.NONE)
      proxy.endDocument()
      proxy.close()
    } else {
      var is: InputSource = null
      var parser: XMLReader = null
      if (source.isInstanceOf[SAXSource]) {
        parser = source.asInstanceOf[SAXSource].getXMLReader
        is = source.asInstanceOf[SAXSource].getInputSource
      } else if (source.isInstanceOf[StreamSource]) {
        is = new InputSource(source.getSystemId)
        is.setCharacterStream(source.asInstanceOf[StreamSource].getReader)
        is.setByteStream(source.asInstanceOf[StreamSource].getInputStream)
      } else {
        throw new XPathException(
          "Source for configuration file must be a StreamSource or SAXSource or NodeInfo")
      }
      if (parser == null) {
        parser = Version.platform.loadParser()
        parser.setFeature("http://xml.org/sax/features/namespaces", true)
        parser.setFeature("http://xml.org/sax/features/namespace-prefixes",
          false)
      }
      parser.setContentHandler(this)
      parser.parse(is)
    }
    if (!errors.isEmpty) {
      var reporter: ErrorReporter = null
      reporter =
        if (config == null) new StandardErrorReporter()
        else config.makeErrorReporter
      for (err <- errors.asScala) {
        reporter.report(err.asWarning())
      }
      throw XPathException.fromXmlProcessingError(errors.get(0))
    }
    if (baseConfiguration != null) {
      config.importLicenseDetails(baseConfiguration)
    }
    config
  }

  def setDocumentLocator(locator: Locator): Unit = {
    this.locator = locator
  }

  def startDocument(): Unit = {
    namespaceStack.push(new ArrayList())
  }

  def endDocument(): Unit = {
    namespaceStack.pop()
    if (config != null) {
      config.getDefaultXsltCompilerInfo.setPackageLibrary(packageLibrary)
    }
  }

  def startPrefixMapping(prefix: String, uri: String): Unit = {
    namespaceStack.peek().add(Array(prefix, uri))
  }

  def endPrefixMapping(prefix: String): Unit = {}

  def startElement(uri: String,
                   localName: String,
                   qName: String,
                   atts: Attributes): Unit = {
    buffer.setLength(0)
    if (NamespaceConstant.SAXON_CONFIGURATION == uri) {
      if (level == 0) {
        if ("configuration" != localName) {
          error(localName, null, null, "configuration")
        }
        var edition: String = atts.getValue("edition")
        if (edition == null) {
          edition = "HE"
        }
        edition match {
          case "HE" => config = new Configuration()
          case "PE" =>
            config = Configuration.makeLicensedConfiguration(
              classLoader,
              "com.saxonica.config.ProfessionalConfiguration")
          case "EE" =>
            config = Configuration.makeLicensedConfiguration(
              classLoader,
              "com.saxonica.config.EnterpriseConfiguration")
          case _ =>
            error("configuration", "edition", edition, "HE|PE|EE")
            config = new Configuration()

        }
        if (baseConfiguration != null) {
          config.setNamePool(baseConfiguration.getNamePool)
          config.setDocumentNumberAllocator(
            baseConfiguration.getDocumentNumberAllocator)
        }
        packageLibrary = new PackageLibrary(config.getDefaultXsltCompilerInfo)
        val licenseLoc: String = atts.getValue("licenseFileLocation")
        if (licenseLoc != null && edition.!=("HE") &&
          !config.isLicensedFeature(
            Configuration.LicenseFeature.PROFESSIONAL_EDITION)) {
          val base: String = locator.getSystemId
          try {
            val absoluteLoc: URI = ResolveURI.makeAbsolute(licenseLoc, base)
            config.setConfigurationProperty(FeatureKeys.LICENSE_FILE_LOCATION,
              absoluteLoc.toString)
          } catch {
            case err: Exception => {
              val incident: XmlProcessingIncident = new XmlProcessingIncident(
                "Failed to process license at " + licenseLoc)
              incident.setCause(err)
              errors.add(incident)
            }

          }
        }
        val targetEdition: String = atts.getValue("targetEdition")
        if (targetEdition != null) {
          packageLibrary.getCompilerInfo.setTargetEdition(targetEdition)
        }
        val label: String = atts.getValue("label")
        if (label != null) {
          config.setLabel(label)
        }
        config.getDynamicLoader.setClassLoader(classLoader)
      }
      if (level == 1) {
        section = localName
        if ("global" == localName) {
          readGlobalElement(atts)
        } else if ("serialization" == localName) {
          readSerializationElement(atts)
        } else if ("xquery" == localName) {
          readXQueryElement(atts)
        } else if ("xslt" == localName) {
          readXsltElement(atts)
        } else if ("xsltPackages" == localName) {} else if ("xsd" == localName) {
          readXsdElement(atts)
        } else if ("resources" == localName) {} else if ("collations" == localName) {} else if ("localizations" == localName) {
          readLocalizationsElement(atts)
        } else {
          error(localName, null, null, null)
        }
      } else if (level == 2) {
        subsection = localName
        section match {
          case "resources" =>
            if ("fileExtension" == localName) {
              readFileExtension(atts)
            }
          case "collations" =>
            if ("collation" != localName) {
              error(localName, null, null, "collation")
            } else {
              readCollation(atts)
            }
          case "localizations" =>
            if ("localization" != localName) {
              error(localName, null, null, "localization")
            } else {
              readLocalization(atts)
            }
          case "xslt" =>
            if ("extensionElement" == localName) {
              readExtensionElement(atts)
            } else {
              error(localName, null, null, null)
            }
          case "xsltPackages" =>
            if ("package" == localName) {
              readXsltPackage(atts)
            }

        }
      } else if (level == 3) {
        if ("package" == subsection) {
          if ("withParam" == localName) {
            readWithParam(atts)
          } else {
            error(localName, null, null, null)
          }
        }
      }
    } else {
      val incident: XmlProcessingIncident = new XmlProcessingIncident(
        "Configuration elements must be in namespace " + NamespaceConstant.SAXON_CONFIGURATION)
      errors.add(incident)
    }
    { level += 1; level - 1 }
    namespaceStack.push(new ArrayList())
  }

  private def readGlobalElement(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    for (i <- 0 until atts.getLength) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (!value.isEmpty && atts.getURI(i).isEmpty) {
        props.put(name, value)
      }
    }
    props.put("#element", "global")
    applyProperty(props, "allowedProtocols", Feature.ALLOWED_PROTOCOLS)
    applyProperty(props,
      "allowExternalFunctions",
      Feature.ALLOW_EXTERNAL_FUNCTIONS)
    applyProperty(props, "allowMultiThreading", Feature.ALLOW_MULTITHREADING)
    applyProperty(props,
      "allowOldJavaUriFormat",
      Feature.ALLOW_OLD_JAVA_URI_FORMAT)
    applyProperty(props,
      "allowSyntaxExtensions",
      Feature.ALLOW_SYNTAX_EXTENSIONS)
    applyProperty(props,
      "collationUriResolver",
      Feature.COLLATION_URI_RESOLVER_CLASS)
    applyProperty(props, "compileWithTracing", Feature.COMPILE_WITH_TRACING)
    applyProperty(props, "debugByteCode", Feature.DEBUG_BYTE_CODE)
    applyProperty(props, "debugByteCodeDirectory", Feature.DEBUG_BYTE_CODE_DIR)
    applyProperty(props, "defaultCollation", Feature.DEFAULT_COLLATION)
    applyProperty(props, "defaultCollection", Feature.DEFAULT_COLLECTION)
    applyProperty(props, "defaultRegexEngine", Feature.DEFAULT_REGEX_ENGINE)
    applyProperty(props, "displayByteCode", Feature.DISPLAY_BYTE_CODE)
    applyProperty(props, "dtdValidation", Feature.DTD_VALIDATION)
    applyProperty(props,
      "dtdValidationRecoverable",
      Feature.DTD_VALIDATION_RECOVERABLE)
    applyProperty(props, "eagerEvaluation", Feature.EAGER_EVALUATION)
    applyProperty(props, "entityResolver", Feature.ENTITY_RESOLVER_CLASS)
    applyProperty(props, "errorListener", Feature.ERROR_LISTENER_CLASS)
    applyProperty(props,
      "environmentVariableResolver",
      Feature.ENVIRONMENT_VARIABLE_RESOLVER_CLASS)
    applyProperty(props,
      "expandAttributeDefaults",
      Feature.EXPAND_ATTRIBUTE_DEFAULTS)
    applyProperty(props, "generateByteCode", Feature.GENERATE_BYTE_CODE)
    applyProperty(props,
      "ignoreSAXSourceParser",
      Feature.IGNORE_SAX_SOURCE_PARSER)
    applyProperty(props, "lineNumbering", Feature.LINE_NUMBERING)
    applyProperty(props,
      "markDefaultedAttributes",
      Feature.MARK_DEFAULTED_ATTRIBUTES)
    applyProperty(props, "maxCompiledClasses", Feature.MAX_COMPILED_CLASSES)
    applyProperty(props,
      "monitorHotSpotByteCode",
      Feature.MONITOR_HOT_SPOT_BYTE_CODE)
    applyProperty(props, "optimizationLevel", Feature.OPTIMIZATION_LEVEL)
    applyProperty(props, "parser", Feature.SOURCE_PARSER_CLASS)
    applyProperty(props, "preEvaluateDoc", Feature.PRE_EVALUATE_DOC_FUNCTION)
    applyProperty(props, "preferJaxpParser", Feature.PREFER_JAXP_PARSER)
    applyProperty(props,
      "recognizeUriQueryParameters",
      Feature.RECOGNIZE_URI_QUERY_PARAMETERS)
    applyProperty(props,
      "retainNodeForDiagnostics",
      Feature.RETAIN_NODE_FOR_DIAGNOSTICS)
    applyProperty(props, "schemaValidation", Feature.SCHEMA_VALIDATION_MODE)
    applyProperty(props, "serializerFactory", Feature.SERIALIZER_FACTORY_CLASS)
    applyProperty(props, "sourceResolver", Feature.SOURCE_RESOLVER_CLASS)
    applyProperty(props, "stableCollectionUri", Feature.STABLE_COLLECTION_URI)
    applyProperty(props, "stableUnparsedText", Feature.STABLE_UNPARSED_TEXT)
    applyProperty(props,
      "standardErrorOutputFile",
      Feature.STANDARD_ERROR_OUTPUT_FILE)
    applyProperty(props, "streamability", Feature.STREAMABILITY)
    applyProperty(props, "streamingFallback", Feature.STREAMING_FALLBACK)
    applyProperty(props, "stripSpace", Feature.STRIP_WHITESPACE)
    applyProperty(props, "styleParser", Feature.STYLE_PARSER_CLASS)
    applyProperty(props,
      "suppressEvaluationExpiryWarning",
      Feature.SUPPRESS_EVALUATION_EXPIRY_WARNING)
    applyProperty(props,
      "suppressXPathWarnings",
      Feature.SUPPRESS_XPATH_WARNINGS)
    applyProperty(props,
      "suppressXsltNamespaceCheck",
      Feature.SUPPRESS_XSLT_NAMESPACE_CHECK)
    applyProperty(props,
      "thresholdForHotspotByteCode",
      Feature.THRESHOLD_FOR_HOTSPOT_BYTE_CODE)
    applyProperty(props, "timing", Feature.TIMING)
    applyProperty(props,
      "traceExternalFunctions",
      Feature.TRACE_EXTERNAL_FUNCTIONS)
    applyProperty(props, "traceListener", Feature.TRACE_LISTENER_CLASS)
    applyProperty(props,
      "traceListenerOutputFile",
      Feature.TRACE_LISTENER_OUTPUT_FILE)
    applyProperty(props,
      "traceOptimizerDecisions",
      Feature.TRACE_OPTIMIZER_DECISIONS)
    applyProperty(props, "treeModel", Feature.TREE_MODEL_NAME)
    applyProperty(props,
      "unparsedTextUriResolver",
      Feature.UNPARSED_TEXT_URI_RESOLVER_CLASS)
    applyProperty(props, "uriResolver", Feature.URI_RESOLVER_CLASS)
    applyProperty(props,
      "usePiDisableOutputEscaping",
      Feature.USE_PI_DISABLE_OUTPUT_ESCAPING)
    applyProperty(props, "useTypedValueCache", Feature.USE_TYPED_VALUE_CACHE)
    applyProperty(props, "validationComments", Feature.VALIDATION_COMMENTS)
    applyProperty(props, "validationWarnings", Feature.VALIDATION_WARNINGS)
    applyProperty(props, "versionOfXml", Feature.XML_VERSION)
    applyProperty(props, "xInclude", Feature.XINCLUDE)
  }

  private def applyProperty[A](props: Properties,
                            attributeName: String,
                            feature: Feature[A]): Unit = {
    val value: String = props.getProperty(attributeName)
    if (value != null) {
      try config.setConfigurationProperty(feature.name, value)
      catch {
        case e: IllegalArgumentException => {
          var message: String = e.getMessage
          if (message.startsWith(attributeName)) {
            message = message.replace(attributeName, "Value")
          }
          if (message.startsWith("Unknown configuration property")) {
            message = "Property not available in Saxon-" + config.getEditionCode
          }
          error(props.getProperty("#element"), attributeName, value, message)
        }

      }
    }
  }

  private def readSerializationElement(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    for (i <- 0 until atts.getLength) {
      val uri: String = atts.getURI(i)
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (value.isEmpty) {
        //continue
      }
      try ResultDocument.setSerializationProperty(props,
        uri,
        name,
        value,
        this,
        false,
        config)
      catch {
        case e: XPathException => errors.add(new XmlProcessingException(e))

      }
    }
    config.setDefaultSerializationProperties(props)
  }

  private def readCollation(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    var collationUri: String = null
    for (i <- 0 until atts.getLength if atts.getURI(i).isEmpty) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (value.isEmpty) {
        //continue
      }
      if ("uri" == name) {
        collationUri = value
      } else {
        props.put(name, value)
      }
    }
    if (collationUri == null) {
      errors.add(new XmlProcessingIncident("collation specified with no uri"))
    }
    var collator: StringCollator = null
    try collator = Version.platform.makeCollation(config, props, collationUri)
    catch {
      case e: XPathException =>
        errors.add(new XmlProcessingIncident(e.getMessage))

    }
    config.registerCollation(collationUri, collator)
  }

  private def readLocalizationsElement(atts: Attributes): Unit = {
    for (i <- 0 until atts.getLength if atts.getURI(i).isEmpty) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if ("defaultLanguage" == name && !value.isEmpty) {
        config.setConfigurationProperty(FeatureKeys.DEFAULT_LANGUAGE, value)
      }
      if ("defaultCountry" == name && !value.isEmpty) {
        config.setConfigurationProperty(FeatureKeys.DEFAULT_COUNTRY, value)
      }
    }
  }

  private def readLocalization(atts: Attributes): Unit = {
    var lang: String = null
    val properties: Properties = new Properties()
    for (i <- 0 until atts.getLength if atts.getURI(i).isEmpty) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if ("lang" == name && !value.isEmpty) {
        lang = value
      } else if (!value.isEmpty) {
        properties.setProperty(name, value)
      }
    }
    if (lang != null) {
      val factory: LocalizerFactory = config.getLocalizerFactory
      if (factory != null) {
        factory.setLanguageProperties(lang, properties)
      }
    }
  }

  private def readFileExtension(atts: Attributes): Unit = {
    val extension: String = atts.getValue("", "extension")
    val mediaType: String = atts.getValue("", "mediaType")
    if (extension == null) {
      error("fileExtension", "extension", null, null)
    }
    if (mediaType == null) {
      error("fileExtension", "mediaType", null, null)
    }
    config.registerFileExtension(extension, mediaType)
  }

   def readExtensionElement(atts: Attributes): Unit = {
    val err: XmlProcessingIncident = new XmlProcessingIncident(
      "Extension elements are not available in Saxon-HE")
    err.setLocation(Loc.makeFromSax(locator))
    errors.add(err)
  }

   def readXsltPackage(atts: Attributes): Unit = {
    val name: String = atts.getValue("name")
    if (name == null) {
      var attName: String = "exportLocation"
      var location: String = atts.getValue("exportLocation")
      var uri: URI = null
      if (location == null) {
        attName = "sourceLocation"
        location = atts.getValue("sourceLocation")
      }
      if (location == null) {
        error("package", attName, null, null)
      }
      try uri = ResolveURI.makeAbsolute(location, locator.getSystemId)
      catch {
        case e: URISyntaxException =>
          error("package", attName, location, "Requires a valid URI.")

      }
      val file: File = new File(uri)
      try packageLibrary.addPackage(file)
      catch {
        case e: XPathException => error(e)

      }
    } else {
      var version: String = atts.getValue("version")
      if (version == null) {
        version = "1"
      }
      var vpn: VersionedPackageName = null
      val details: PackageDetails = new PackageDetails()
      try vpn = new VersionedPackageName(name, version)
      catch {
        case err: XPathException => error("package", "version", version, null)

      }
      details.nameAndVersion = vpn
      currentPackage = details
      val sourceLoc: String = atts.getValue("sourceLocation")
      var source: StreamSource = null
      if (sourceLoc != null) {
        try source = new StreamSource(
          ResolveURI.makeAbsolute(sourceLoc, locator.getSystemId).toString)
        catch {
          case e: URISyntaxException =>
            error("package",
              "sourceLocation",
              sourceLoc,
              "Requires a valid URI.")

        }
        details.sourceLocation = source
      }
      val exportLoc: String = atts.getValue("exportLocation")
      if (exportLoc != null) {
        try source = new StreamSource(
          ResolveURI.makeAbsolute(exportLoc, locator.getSystemId).toString)
        catch {
          case e: URISyntaxException =>
            error("package",
              "exportLocation",
              exportLoc,
              "Requires a valid URI.")

        }
        details.exportLocation = source
      }
      val priority: String = atts.getValue("priority")
      if (priority != null) {
        try details.priority = java.lang.Integer.parseInt(priority)
        catch {
          case err: NumberFormatException =>
            error("package", "priority", priority, "Requires an integer.")

        }
      }
      details.baseName = atts.getValue("base")
      details.shortName = atts.getValue("shortName")
      packageLibrary.addPackage(details)
    }
  }

   def readWithParam(atts: Attributes): Unit = {
    if (currentPackage.exportLocation != null) {
      error("withParam", null, null, "Not allowed when @exportLocation exists")
    }
    val name: String = atts.getValue("name")
    if (name == null) {
      error("withParam", "name", null, null)
    }
    val qp: QNameParser = new QNameParser(this).withAcceptEQName(true)
    var qName: StructuredQName = null
    try qName = qp.parse(name, "")
    catch {
      case e: XPathException =>
        error("withParam", "name", name, "Requires valid QName")

    }
    val select: String = atts.getValue("select")
    if (select == null) {
      error("withParam", "select", null, null)
    }
    val env: IndependentContext = new IndependentContext(config)
    env.setNamespaceResolver(this)
    val parser: XPathParser = new XPathParser()
    var value: GroundedValue = null
    try {
      val exp: Expression = parser.parse(select, 0, Token.EOF, env)
      value = exp.iterate(env.makeEarlyEvaluationContext()).materialize()
    } catch {
      case e: XPathException => error(e)

    }
    if (currentPackage.staticParams == null) {
      currentPackage.staticParams = new HashMap()
    }
    currentPackage.staticParams.put(qName, value)
  }

  private def readXQueryElement(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    for (i <- 0 until atts.getLength) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (!value.isEmpty && atts.getURI(i).isEmpty) {
        props.put(name, value)
      }
    }
    props.put("#element", "xquery")
    applyProperty(props, "allowUpdate", Feature.XQUERY_ALLOW_UPDATE)
    applyProperty(props, "constructionMode", Feature.XQUERY_CONSTRUCTION_MODE)
    applyProperty(props,
      "defaultElementNamespace",
      Feature.XQUERY_DEFAULT_ELEMENT_NAMESPACE)
    applyProperty(props,
      "defaultFunctionNamespace",
      Feature.XQUERY_DEFAULT_FUNCTION_NAMESPACE)
    applyProperty(props, "emptyLeast", Feature.XQUERY_EMPTY_LEAST)
    applyProperty(props,
      "inheritNamespaces",
      Feature.XQUERY_INHERIT_NAMESPACES)
    applyProperty(props,
      "moduleUriResolver",
      Feature.MODULE_URI_RESOLVER_CLASS)
    applyProperty(props,
      "preserveBoundarySpace",
      Feature.XQUERY_PRESERVE_BOUNDARY_SPACE)
    applyProperty(props,
      "preserveNamespaces",
      Feature.XQUERY_PRESERVE_NAMESPACES)
    applyProperty(props,
      "requiredContextItemType",
      Feature.XQUERY_REQUIRED_CONTEXT_ITEM_TYPE)
    applyProperty(props, "schemaAware", Feature.XQUERY_SCHEMA_AWARE)
    applyProperty(props,
      "staticErrorListener",
      Feature.XQUERY_STATIC_ERROR_LISTENER_CLASS)
    applyProperty(props, "version", Feature.XQUERY_VERSION)
  }

  private def readXsltElement(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    for (i <- 0 until atts.getLength) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (!value.isEmpty && atts.getURI(i).isEmpty) {
        props.put(name, value)
      }
    }
    props.put("#element", "xslt")
    applyProperty(props, "disableXslEvaluate", Feature.DISABLE_XSL_EVALUATE)
    applyProperty(props, "enableAssertions", Feature.XSLT_ENABLE_ASSERTIONS)
    applyProperty(props, "initialMode", Feature.XSLT_INITIAL_MODE)
    applyProperty(props, "initialTemplate", Feature.XSLT_INITIAL_TEMPLATE)
    applyProperty(props, "messageEmitter", Feature.MESSAGE_EMITTER_CLASS)
    applyProperty(props,
      "outputUriResolver",
      Feature.OUTPUT_URI_RESOLVER_CLASS)
    applyProperty(props, "recoveryPolicy", Feature.RECOVERY_POLICY_NAME)
    applyProperty(props,
      "resultDocumentThreads",
      Feature.RESULT_DOCUMENT_THREADS)
    applyProperty(props, "schemaAware", Feature.XSLT_SCHEMA_AWARE)
    applyProperty(props,
      "staticErrorListener",
      Feature.XSLT_STATIC_ERROR_LISTENER_CLASS)
    applyProperty(props,
      "staticUriResolver",
      Feature.XSLT_STATIC_URI_RESOLVER_CLASS)
    applyProperty(props, "strictStreamability", Feature.STRICT_STREAMABILITY)
    applyProperty(props, "styleParser", Feature.STYLE_PARSER_CLASS)
    applyProperty(props, "version", Feature.XSLT_VERSION)
  }

  private def readXsdElement(atts: Attributes): Unit = {
    val props: Properties = new Properties()
    for (i <- 0 until atts.getLength) {
      val name: String = atts.getLocalName(i)
      val value: String = atts.getValue(i)
      if (!value.isEmpty && atts.getURI(i).isEmpty) {
        props.put(name, value)
      }
    }
    props.put("#element", "xsd")
    applyProperty(props,
      "assertionsCanSeeComments",
      Feature.ASSERTIONS_CAN_SEE_COMMENTS)
    applyProperty(props,
      "implicitSchemaImports",
      Feature.IMPLICIT_SCHEMA_IMPORTS)
    applyProperty(props,
      "multipleSchemaImports",
      Feature.MULTIPLE_SCHEMA_IMPORTS)
    applyProperty(props, "occurrenceLimits", Feature.OCCURRENCE_LIMITS)
    applyProperty(props,
      "schemaUriResolver",
      Feature.SCHEMA_URI_RESOLVER_CLASS)
    applyProperty(props,
      "thresholdForCompilingTypes",
      Feature.THRESHOLD_FOR_COMPILING_TYPES)
    applyProperty(props,
      "useXsiSchemaLocation",
      Feature.USE_XSI_SCHEMA_LOCATION)
    applyProperty(props, "version", Feature.XSD_VERSION)
  }

  private def error(element: String,
                    attribute: String,
                    actual: String,
                    required: String): Unit = {
    var err: XmlProcessingIncident = null
    err =
      if (attribute == null)
        new XmlProcessingIncident("Invalid configuration element " + element)
      else if (actual == null)
        new XmlProcessingIncident(
          "Missing configuration property " + element + "/@" + attribute)
      else
        new XmlProcessingIncident(
          "Invalid configuration property " + element + "/@" + attribute +
            ". Supplied value '" +
            actual +
            "'. " +
            required)
    err.setLocation(Loc.makeFromSax(locator))
    errors.add(err)
  }

   def error(err: XPathException): Unit = {
    err.setLocator(Loc.makeFromSax(locator))
    errors.add(new XmlProcessingException(err))
  }

   def errorClass(element: String,
                           attribute: String,
                           actual: String,
                           required: Class[_],
                           cause: Exception): Unit = {
    val err: XmlProcessingIncident = new XmlProcessingIncident(
      "Invalid configuration property " + element + (if (attribute == null) ""
      else "/@" + attribute) +
        ". Supplied value '" +
        actual +
        "', required value is the name of a class that implements '" +
        required.getName +
        "'")
    err.setCause(cause)
    err.setLocation(Loc.makeFromSax(locator))
    errors.add(err)
  }

  def endElement(uri: String, localName: String, qName: String): Unit = {
    if (level == 3 && "resources" == section) {
      val content: String = buffer.toString
      if (!content.isEmpty) {
        if ("externalObjectModel" == localName) {
          try {
            val model: ExternalObjectModel = config
              .getInstance(content, null)
              .asInstanceOf[ExternalObjectModel]
            config.registerExternalObjectModel(model)
          } catch {
            case e  @ (_: XPathException | _: ClassCastException) =>
              errorClass("externalObjectModel",
                null,
                content,
                classOf[ExternalObjectModel],
                e.asInstanceOf[Exception])

          }
        } else if ("extensionFunction" == localName) {
          try {
            val model: ExtensionFunctionDefinition = config
              .getInstance(content, null)
              .asInstanceOf[ExtensionFunctionDefinition]
            config.registerExtensionFunction(model)
          } catch {
            case e @ (_: XPathException | _: ClassCastException |
                      _: IllegalArgumentException) =>
              errorClass("extensionFunction",
                null,
                content,
                classOf[ExtensionFunctionDefinition],
                e.asInstanceOf[Exception])

          }
        } else if ("schemaDocument" == localName) {
          try {
            val source: Source = getInputSource(content)
            config.addSchemaSource(source)
          } catch {
            case e: XPathException => errors.add(new XmlProcessingException(e))

          }
        } else if ("schemaComponentModel" == localName) {
          try {
            val source: Source = getInputSource(content)
            config.importComponents(source)
          } catch {
            case e: XPathException => errors.add(new XmlProcessingException(e))

          }
        } else if ("fileExtension" == localName) {} else {
          error(localName, null, null, null)
        }
      }
    }
    { level -= 1; level + 1 }
    buffer.setLength(0)
    namespaceStack.pop()
  }

  def getInputSource(href: String): Source = {
    val base: String = locator.getSystemId
    val abs: URI = ResolveURI.makeAbsolute(href, base)
    new StreamSource(abs.toString)
  }

  def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    buffer.append(ch, start, length)
  }

  def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {}

  def processingInstruction(target: String, data: String): Unit = {}

  def skippedEntity(name: String): Unit = {}

  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    var i: Int = namespaceStack.size - 1
    while (i >= 0) {
      val list: List[Array[String]] = namespaceStack.get(i)
      for (pair <- list.asScala if pair(0) == prefix) {
        pair(1)
      }
      { i -= 1; i + 1 }
    }
    null
  }

  def iteratePrefixes(): Iterator[String] = {
    val prefixes: Set[String] = new HashSet[String]()
    for (list <- namespaceStack.asScala; pair <- list.asScala) {
      prefixes.add(pair(0))
    }
    prefixes.iterator()
  }

}
