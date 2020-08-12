package net.sf.saxon

import scala.util.Using
import net.sf.saxon.event.Receiver
import net.sf.saxon.expr.instruct.TerminationException
import net.sf.saxon.lib._
import net.sf.saxon.model.ConversionResult
import net.sf.saxon.model.SchemaException
import net.sf.saxon.model.Type
import net.sf.saxon.om._
import net.sf.saxon.query.QueryReader
import net.sf.saxon.query.QueryResult
import net.sf.saxon.query.UpdateAgent
import net.sf.saxon.query.XQueryExpression
import net.sf.saxon.s9api._
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.trace._
import net.sf.saxon.trans.CommandLineOptions
import net.sf.saxon.trans.LicenseException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.DateTimeValue
import org.xml.sax.InputSource
import javax.xml.transform.Source
import javax.xml.transform.TransformerFactoryConfigurationError
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.stream.StreamSource
import java.io._
import java.net.URI
import java.util.ArrayList
import java.util.List
import java.util.Set
import Query._
import net.sf.saxon.utils.{Configuration, Version}

object Query {

  def main(args: Array[String]): Unit = {
    new Query().doQuery(args, "java net.sf.saxon.Query")
  }

  private def rewriteToDisk(doc: NodeInfo,
                            serializer: Serializer,
                            backup: Boolean,
                            log: PrintStream): Unit = {
    doc.getNodeKind match {
      case Type.DOCUMENT => //break
      case Type.ELEMENT =>
        var parent: NodeInfo = doc.getParent
        if (parent != null && parent.getNodeKind != Type.DOCUMENT) {
          throw new SaxonApiException(
            "Cannot rewrite an element node unless it is top-level")
        }
      case _ =>
        throw new SaxonApiException(
          "Node to be rewritten must be a document or element node")

    }
    val uri: String = doc.getSystemId
    if (uri == null || uri.isEmpty) {
      throw new SaxonApiException(
        "Cannot rewrite a document with no known URI")
    }
    var u: URI = null
    u = new URI(uri)
    val existingFile: File = new File(u)
    val dir: File = existingFile.getParentFile
    if (backup && existingFile.exists()) {
      val backupFile: File = new File(dir, existingFile.getName + ".bak")
      if (log != null) {
        log.println("Creating backup file " + backupFile)
      }
      val success: Boolean = existingFile.renameTo(backupFile)
      if (!success) {
        throw new SaxonApiException(
          "Failed to create backup file of " + backupFile)
      }
    }
    if (!existingFile.exists()) {
      if (log != null) {
        log.println("Creating file " + existingFile)
      }
      existingFile.createNewFile()
    } else {
      if (log != null) {
        log.println("Overwriting file " + existingFile)
      }
    }
    serializer.setOutputFile(existingFile)
    serializer.getProcessor.writeXdmValue(XdmValue.wrap(doc), serializer)
  }

  def createFileIfNecessary(file: File): Unit = {
    if (!file.exists()) {
      val directory: File = file.getParentFile
      if (directory != null && !directory.exists()) {
        directory.mkdirs()
      }
      file.createNewFile()
    }
  }

}

class Query {

   var processor: Processor = _

   var config: Configuration = _

   var showTime: Boolean = false

   var repeat: Int = 1

   var sourceFileName: String = null

   var queryFileName: String = null

   var useURLs: Boolean = false

   var outputFileName: String = null

   var moduleURIResolverClass: String = null

   val uriResolverClass: String = null

   var explain: Boolean = false

   var wrap: Boolean = false

   var projection: Boolean = false

   var streaming: Boolean = false

   var updating: Boolean = false

   var writeback: Boolean = false

   var backup: Boolean = true

   var explainOutputFileName: String = null

  private var traceDestination: Logger = new StandardLogger()

  private var closeTraceDestination: Boolean = false

  private var allowExit: Boolean = true

   def getConfiguration(): Configuration = config

  def setPermittedOptions(options: CommandLineOptions): Unit = {
    options.addRecognizedOption("backup",
      CommandLineOptions.TYPE_BOOLEAN,
      "Save updated documents before overwriting")
    options.addRecognizedOption(
      "catalog",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "Use specified catalog file to resolve URIs")
    options.addRecognizedOption(
      "config",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "Use specified configuration file")
    options.addRecognizedOption(
      "cr",
      CommandLineOptions.TYPE_CLASSNAME | CommandLineOptions.VALUE_REQUIRED,
      "Use specified collection URI resolver class")
    options.addRecognizedOption("dtd",
      CommandLineOptions.TYPE_ENUMERATION,
      "Validate using DTD")
    options.setPermittedValues("dtd", Array("on", "off", "recover"), "on")
    options.addRecognizedOption("expand",
      CommandLineOptions.TYPE_BOOLEAN,
      "Expand attribute defaults from DTD or Schema")
    options.addRecognizedOption(
      "explain",
      CommandLineOptions.TYPE_FILENAME,
      "Display compiled expression tree and optimization decisions")
    options.addRecognizedOption(
      "ext",
      CommandLineOptions.TYPE_BOOLEAN,
      "Allow calls to Java extension functions and xsl:result-document")
    options.addRecognizedOption(
      "init",
      CommandLineOptions.TYPE_CLASSNAME,
      "User-supplied net.sf.saxon.lib.Initializer class to initialize the Saxon Configuration")
    options.addRecognizedOption("l",
      CommandLineOptions.TYPE_BOOLEAN,
      "Maintain line numbers for source documents")
    options.addRecognizedOption(
      "mr",
      CommandLineOptions.TYPE_CLASSNAME | CommandLineOptions.VALUE_REQUIRED,
      "Use named ModuleURIResolver class")
    options.addRecognizedOption(
      "now",
      CommandLineOptions.TYPE_DATETIME | CommandLineOptions.VALUE_REQUIRED,
      "Run with specified current date/time")
    options.addRecognizedOption(
      "o",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "Use specified file for primary output")
    options.addRecognizedOption(
      "opt",
      CommandLineOptions.TYPE_STRING | CommandLineOptions.VALUE_REQUIRED,
      "Enable/disable optimization options [-]cfgklmnsvwx")
    options.addRecognizedOption(
      "outval",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Action when validation of output file fails")
    options.setPermittedValues("outval", Array("recover", "fatal"), null)
    options.addRecognizedOption(
      "p",
      CommandLineOptions.TYPE_BOOLEAN,
      "Recognize query parameters in URI passed to doc()")
    options.addRecognizedOption("projection",
      CommandLineOptions.TYPE_BOOLEAN,
      "Use source document projection")
    options.addRecognizedOption(
      "q",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "Query filename")
    options.addRecognizedOption(
      "qs",
      CommandLineOptions.TYPE_STRING | CommandLineOptions.VALUE_REQUIRED,
      "Query string (usually in quotes)")
    options.addRecognizedOption(
      "quit",
      CommandLineOptions.TYPE_BOOLEAN | CommandLineOptions.VALUE_REQUIRED,
      "Quit JVM if query fails")
    options.addRecognizedOption(
      "r",
      CommandLineOptions.TYPE_CLASSNAME | CommandLineOptions.VALUE_REQUIRED,
      "Use named URIResolver class")
    options.addRecognizedOption(
      "repeat",
      CommandLineOptions.TYPE_INTEGER | CommandLineOptions.VALUE_REQUIRED,
      "Run N times for performance measurement")
    options.addRecognizedOption(
      "s",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "Source file for primary input")
    options.addRecognizedOption("sa",
      CommandLineOptions.TYPE_BOOLEAN,
      "Run in schema-aware mode")
    options.addRecognizedOption("scmin",
      CommandLineOptions.TYPE_FILENAME,
      "Pre-load schema in SCM format")
    options.addRecognizedOption("stream",
      CommandLineOptions.TYPE_BOOLEAN,
      "Execute in streamed mode")
    options.addRecognizedOption(
      "strip",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Handling of whitespace text nodes in source documents")
    options.setPermittedValues("strip",
      Array("none", "all", "ignorable"),
      null)
    options.addRecognizedOption("t",
      CommandLineOptions.TYPE_BOOLEAN,
      "Display version and timing information")
    options.addRecognizedOption(
      "T",
      CommandLineOptions.TYPE_CLASSNAME,
      "Use named TraceListener class, or standard TraceListener")
    options.addRecognizedOption(
      "TB",
      CommandLineOptions.TYPE_FILENAME,
      "Trace hotspot bytecode generation to specified XML file")
    options.addRecognizedOption(
      "TJ",
      CommandLineOptions.TYPE_BOOLEAN,
      "Debug binding and execution of extension functions")
    options.setPermittedValues("TJ", Array("on", "off"), "on")
    options.addRecognizedOption(
      "tree",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Use specified tree model for source documents")
    options.addRecognizedOption("Tlevel",
      CommandLineOptions.TYPE_STRING,
      "Level of detail for trace listener output")
    options.setPermittedValues("Tlevel",
      Array("none", "low", "normal", "high"),
      "normal")
    options.addRecognizedOption("Tout",
      CommandLineOptions.TYPE_FILENAME,
      "File for trace listener output")
    options.addRecognizedOption(
      "TP",
      CommandLineOptions.TYPE_FILENAME,
      "Use profiling trace listener, with specified output file")
    options.addRecognizedOption(
      "traceout",
      CommandLineOptions.TYPE_FILENAME | CommandLineOptions.VALUE_REQUIRED,
      "File for output of trace() messages")
    options.setPermittedValues("tree", Array("linked", "tiny", "tinyc"), null)
    options.addRecognizedOption("u",
      CommandLineOptions.TYPE_BOOLEAN,
      "Interpret filename arguments as URIs")
    options.setPermittedValues("u", Array("on", "off"), "on")
    options.addRecognizedOption(
      "update",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Enable or disable XQuery updates, or enable the syntax but discard the updates"
    )
    options.setPermittedValues("update", Array("on", "off", "discard"), null)
    options.addRecognizedOption("val",
      CommandLineOptions.TYPE_ENUMERATION,
      "Apply validation to source documents")
    options.setPermittedValues("val", Array("strict", "lax"), "strict")
    options.addRecognizedOption("wrap",
      CommandLineOptions.TYPE_BOOLEAN,
      "Wrap result sequence in XML elements")
    options.addRecognizedOption(
      "x",
      CommandLineOptions.TYPE_CLASSNAME | CommandLineOptions.VALUE_REQUIRED,
      "Use named XMLReader class for parsing source documents")
    options.addRecognizedOption(
      "xi",
      CommandLineOptions.TYPE_BOOLEAN,
      "Expand XInclude directives in source documents")
    options.addRecognizedOption(
      "xmlversion",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Indicate whether XML 1.1 is supported")
    options.setPermittedValues("xmlversion", Array("1.0", "1.1"), null)
    options.addRecognizedOption(
      "xsd",
      CommandLineOptions.TYPE_FILENAME_LIST | CommandLineOptions.VALUE_REQUIRED,
      "List of schema documents to be preloaded")
    options.addRecognizedOption(
      "xsdversion",
      CommandLineOptions.TYPE_ENUMERATION | CommandLineOptions.VALUE_REQUIRED,
      "Indicate whether XSD 1.1 is supported")
    options.setPermittedValues("xsdversion", Array("1.0", "1.1"), null)
    options.addRecognizedOption(
      "xsiloc",
      CommandLineOptions.TYPE_BOOLEAN,
      "Load schemas named in xsi:schemaLocation (default on)")
    options.addRecognizedOption("?",
      CommandLineOptions.VALUE_PROHIBITED,
      "Display command line help text")
  }

   def doQuery(args: Array[String], command: String): Unit = {
    val options: CommandLineOptions = new CommandLineOptions()
    this.setPermittedOptions(options)
    try options.setActualOptions(args)
    catch {
      case err: XPathException => quit(err.getMessage, 2)

    }
    var schemaAware: Boolean = false
    val configFile: String = options.getOptionValue("config")
    if (configFile != null) {
      try {
        config = Configuration.readConfiguration(new StreamSource(configFile))
        schemaAware = config.isLicensedFeature(
          Configuration.LicenseFeature.ENTERPRISE_XQUERY)
      } catch {
        case e: XPathException => quit(e.getMessage, 2)

      }
    }
    if (config == null && !schemaAware) {
      schemaAware = options.testIfSchemaAware()
    }
    if (config == null) {
      config = Configuration.newConfiguration
    }
    processor = new Processor(config)
    try {
      parseOptions(options)
      val compiler: XQueryCompiler = processor.newXQueryCompiler()
      compiler.setSchemaAware(schemaAware)
      if (updating) {
        compiler.setUpdatingEnabled(true)
      }
      if (config.getTraceListener != null) {
        compiler.setCompileWithTracing(true)
      }
      if (moduleURIResolverClass != null) {
        val mr: AnyRef = config.getInstance(moduleURIResolverClass, null).asInstanceOf[AnyRef]
        if (!(mr.isInstanceOf[ModuleURIResolver])) {
          badUsage(moduleURIResolverClass + " is not a ModuleURIResolver")
        }
        compiler.setModuleURIResolver(mr.asInstanceOf[ModuleURIResolver])
      }
      if (uriResolverClass != null) {
        config.setURIResolver(config.makeURIResolver(uriResolverClass))
      }
      config.displayLicenseMessage()
      if (schemaAware &&
        !config.isLicensedFeature(
          Configuration.LicenseFeature.ENTERPRISE_XQUERY)) {
        if ("EE" == config.getEditionCode) {
          quit("Installed license does not allow schema-aware query", 2)
        } else {
          quit("Schema-aware query requires Saxon Enterprise Edition", 2)
        }
      }
      if (explain) {
        config.setBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS, true)
      }
      compiler.setStreaming(streaming)
      var sourceInput: Source = null
      if (sourceFileName != null) {
        sourceInput = processSourceFile(sourceFileName, useURLs)
      }
      var startTime: Long = System.nanoTime()
      if (showTime) {
        System.err.println("Analyzing query from " + queryFileName)
      }
      var exp: XQueryExecutable = null
      try {
        exp = compileQuery(compiler, queryFileName, useURLs)
        if (showTime) {
          val endTime: Long = System.nanoTime()
          System.err.println(
            "Analysis time: " + ((endTime - startTime) / 1e6) + " milliseconds")
          startTime = endTime
        }
      } catch {
        case e: SaxonApiException =>
          if (e.getCause.isInstanceOf[XPathException]) {
            val err: XPathException = e.getCause.asInstanceOf[XPathException]
            var line: Int = -1
            var module: String = null
            if (err.getLocator != null) {
              line = err.getLocator.getLineNumber
              module = err.getLocator.getSystemId
            }
            if (err.hasBeenReported) {
              quit("Static error(s) in query", 2)
            } else {
              if (line == -1) {
                System.err.println("Static error in query: " + err.getMessage)
              } else {
                System.err.println(
                  "Static error at line " + line + " of " + module + ':')
                System.err.println(err.getMessage)
              }
            }
            exp = null
            if (allowExit) {
              System.exit(2)
            } else {
              throw new RuntimeException(err)
            }
          } else {
            quit(e.getMessage, 2)
          }

      }
      if (explain && exp != null) {
        var out: Serializer = null
        out =
          if (explainOutputFileName == null || explainOutputFileName.==(""))
            processor.newSerializer(System.err)
          else processor.newSerializer(new File(explainOutputFileName))
        out.setOutputProperty(Serializer.Property.METHOD, "xml")
        out.setOutputProperty(Serializer.Property.INDENT, "yes")
        out.setOutputProperty(Serializer.Property.OMIT_XML_DECLARATION, "yes")
        if ("HE" != processor.getSaxonEdition) {
          out.setOutputProperty(Serializer.Property.SAXON_INDENT_SPACES, "2")
        }
        exp.explain(out)
      }
      exp.getUnderlyingCompiledQuery.setAllowDocumentProjection(projection)
      val evaluator: XQueryEvaluator = exp.load()
      evaluator.setTraceFunctionDestination(traceDestination)
      if (options.getOptionValue("now") != null) {
        val now: String = options.getOptionValue("now")
        val dt: ConversionResult =
          DateTimeValue.makeDateTimeValue(now, config.getConversionRules)
        if (dt.isInstanceOf[DateTimeValue]) {
          evaluator.getUnderlyingQueryContext.setCurrentDateTime(
            dt.asInstanceOf[DateTimeValue])
        } else {
          System.err.println("Invalid dateTime: " + now + " (ignored)")
        }
      }
      if (uriResolverClass != null) {
        evaluator.setURIResolver(config.makeURIResolver(uriResolverClass))
      }
      processSource(sourceInput, exp, evaluator)
      options.setParams(processor, evaluator.setExternalVariable)
      startTime = System.nanoTime()
      var totalTime: Long = 0
      var r: Int = 0
      r = 0
      while (r < repeat) {
        try {
          var out: OutputStream = null
          if (outputFileName != null) {
            val outputFile: File = new File(outputFileName)
            if (outputFile.isDirectory) {
              quit("Output is a directory", 2)
            }
            createFileIfNecessary(outputFile)
            out = new FileOutputStream(outputFile)
          } else {
            out = System.out
          }
          val serializer: Serializer = processor.newSerializer(out)
          try options.setSerializationProperties(serializer)
          catch {
            case e: IllegalArgumentException => quit(e.getMessage, 2)

          }
          if (updating && exp.isUpdateQuery) {
            serializer.setOutputProperties(
              exp.getUnderlyingCompiledQuery.getExecutable.getPrimarySerializationProperties.getProperties)
            runUpdate(exp, evaluator, serializer)
          } else {
            runQuery(exp, evaluator, sourceInput, serializer)
          }
        } catch {
          case err: SaxonApiException =>
            if (err.getCause.isInstanceOf[XPathException] &&
              err.getCause.asInstanceOf[XPathException].hasBeenReported) {
              val category: String =
                if (err.getCause.asInstanceOf[XPathException].isTypeError)
                  "type"
                else "dynamic"
              quit(
                "Query failed with " + category + " error: " + err.getCause.getMessage,
                2)
            } else {
              throw err
            }

        }
        if (showTime) {
          val endTime: Long = System.nanoTime()
          if (r >= 3) {
            totalTime += endTime - startTime
          }
          if (repeat < 100) {
            System.err.println(
              "Execution time: " +
                CommandLineOptions.showExecutionTimeNano(endTime - startTime))
            CommandLineOptions.showMemoryUsed()
            Instrumentation.report()
          } else if (totalTime > 1000000000000L) {
            //break
          }
          startTime = endTime
        }
        {
          r += 1;
          r - 1
        }
      }
      if (repeat > 3) {
        System.err.println(
          "Average execution time: " +
            CommandLineOptions.showExecutionTimeNano(totalTime / (r - 3)))
      }
      if (options.getOptionValue("TB") != null) {
        config.createByteCodeReport(options.getOptionValue("TB"))
      }
    } catch {
      case err: TerminationException => quit(err.getMessage, 1)

      case err: SchemaException =>
        quit("Schema processing failed: " + err.getMessage, 2)

      case err@(_: XPathException | _: LicenseException |
                _: SaxonApiException) =>
        quit("Query processing failed: " + err.getMessage, 2)

      case err: TransformerFactoryConfigurationError => {
        err.printStackTrace()
        quit("Query processing failed", 2)
      }

      case err2: Exception => {
        err2.printStackTrace()
        quit("Fatal error during query: " + err2.getClass.getName +
          ": " +
          (if (err2.getMessage == null) " (no message)"
          else err2.getMessage),
          2)
      }

    }
  }

   def parseOptions(options: CommandLineOptions): Unit = {
    options.applyToConfiguration(processor)
    allowExit = "off" != options.getOptionValue("quit")
    backup = "on" == options.getOptionValue("backup")
    explainOutputFileName = options.getOptionValue("explain")
    explain = explainOutputFileName != null
    moduleURIResolverClass = options.getOptionValue("mr")
    outputFileName = options.getOptionValue("o")
    streaming = "on" == options.getOptionValue("stream")
    var value: String = options.getOptionValue("p")
    if ("on" == value) {
      config.setParameterizedURIResolver()
      useURLs = true
    }
    projection = "on" == options.getOptionValue("projection")
    value = options.getOptionValue("q")
    if (value != null) {
      queryFileName = value
    }
    value = options.getOptionValue("qs")
    if (value != null) {
      queryFileName = "{" + value + "}"
    }
    val qv: String = options.getOptionValue("qversion")
    if (qv != null && "3.1" != qv) {
      System.err.println("-qversion ignored: 3.1 is assumed")
    }
    value = options.getOptionValue("repeat")
    if (value != null) {
      repeat = java.lang.Integer.parseInt(value)
    }
    sourceFileName = options.getOptionValue("s")
    value = options.getOptionValue("t")
    if ("on" == value) {
      System.err.println(config.getProductTitle)
      System.err.println(Version.platform.getPlatformVersion)
      config.setTiming(true)
      showTime = true
    }
    value = options.getOptionValue("traceout")
    if (value != null) {
      value match {
        case "#err" => //break
        case "#out" => traceDestination = new StandardLogger(System.out)
        case "#null" => traceDestination = null
        case _ =>
          try {
            traceDestination = new StandardLogger(new File(value))
            closeTraceDestination = true
          } catch {
            case e: FileNotFoundException =>
              badUsage("Trace output file " + value + " cannot be created")

          }

      }
    }
    value = options.getOptionValue("T")
    if (value != null) {
      if ("" == value) {
        makeXQueryTraceListener(options)
      } else {
        config.setTraceListenerClass(value)
      }
      config.setLineNumbering(true)
    }
    value = options.getOptionValue("Tout")
    if (value != null) {
      config.setTraceListenerOutputFile(value)
      if (options.getOptionValue("T") == null) {
        makeXQueryTraceListener(options)
      }
    }
    value = options.getOptionValue("TB")
    if (value != null) {
      config.setBooleanProperty(Feature.MONITOR_HOT_SPOT_BYTE_CODE, true)
    }
    value = options.getOptionValue("TP")
    if (value != null) {
      //val listener: TimingTraceListener = new TimingTraceListener() // TimingTraceListener not exist
     // config.setTraceListener(listener)
      config.setLineNumbering(true)
      config.getDefaultStaticQueryContext.setCodeInjector(
        new TimingCodeInjector())
    /*  if (!value.isEmpty) {
        try listener.setOutputDestination(new StandardLogger(new File(value)))
        catch {
          case e: FileNotFoundException =>
            badUsage("Trace output file " + value + " cannot be created")

        }
      }*/
    }
    value = options.getOptionValue("u")
    if (value != null) {
      useURLs = "on" == value
    }
    value = options.getOptionValue("update")
    if (value != null) {
      if ("off" != value) {
        updating = true
      }
      writeback = "discard" != value
    }
    wrap = "on" == options.getOptionValue("wrap")
    value = options.getOptionValue("x")
    if (value != null) {
      config.setSourceParserClass(value)
    }
    val additionalSchemas: String = options.getOptionValue("xsd")
    value = options.getOptionValue("?")
    if (value != null) {
      badUsage("")
    }
    if (options.getOptionValue("xsiloc") != null && options.getOptionValue(
      "val") == null) {
      System.err.println("-xsiloc is ignored when -val is absent")
    }
    applyLocalOptions(options, config)
    val positional: List[String] = options.getPositionalOptions
    var currentPositionalOption: Int = 0
    if (queryFileName == null) {
      if (positional.size == currentPositionalOption) {
        badUsage("No query file name")
      }
      queryFileName = positional.get({
        currentPositionalOption += 1;
        currentPositionalOption - 1
      })
    }
    if (currentPositionalOption < positional.size) {
      badUsage(
        "Unrecognized option: " + positional.get(currentPositionalOption))
    }
    val scmInput: String = options.getOptionValue("scmin")
    if (scmInput != null) {
      config.importComponents(new StreamSource(scmInput))
    }
    if (additionalSchemas != null) {
      CommandLineOptions.loadAdditionalSchemas(config, additionalSchemas)
    }
  }

  private def makeXQueryTraceListener(options: CommandLineOptions): Unit = {
    val listener: XQueryTraceListener = new XQueryTraceListener()
    var value: String = options.getOptionValue("Tout")
    if (value != null) {
      try listener.setOutputDestination(
        new StandardLogger(new PrintStream(value)))
      catch {
        case e: FileNotFoundException => badUsage("Cannot write to " + value)

      }
    }
    value = options.getOptionValue("Tlevel")
    if (value != null) {
      value match {
        case "none" => listener.setLevelOfDetail(0)
        case "low" => listener.setLevelOfDetail(1)
        case "normal" => listener.setLevelOfDetail(2)
        case "high" => listener.setLevelOfDetail(3)

      }
    }
    config.setTraceListener(listener)
  }

  def applyLocalOptions(options: CommandLineOptions,
                        config: Configuration): Unit = {}

  def processSourceFile(sourceFileName: String,
                        useURLs: Boolean): Source = {
    var sourceInput: Source = null
    if (useURLs || CommandLineOptions.isImplicitURI(sourceFileName)) {
      sourceInput = config.getURIResolver.resolve(sourceFileName, null)
      if (sourceInput == null) {
        sourceInput = config.getSystemURIResolver.resolve(sourceFileName, null)
      }
    } else if (sourceFileName.==("-")) {
      val sysId: String =
        new File(System.getProperty("user.dir")).toURI().toASCIIString()
      sourceInput = new StreamSource(System.in, sysId)
    } else {
      val sourceFile: File = new File(sourceFileName)
      if (!sourceFile.exists()) {
        quit("Source file " + sourceFile + " does not exist", 2)
      }
      if (Version.platform.isJava) {
        val eis: InputSource = new InputSource(sourceFile.toURI().toString)
        sourceInput = new SAXSource(eis)
      } else {
        sourceInput = new StreamSource(sourceFile.toURI().toString)
      }
    }
    sourceInput
  }

  def compileQuery(compiler: XQueryCompiler,
                   queryFileName: String,
                   useURLs: Boolean): XQueryExecutable = {
    var exp: XQueryExecutable = null
    if (queryFileName.==("-")) {
      val queryReader: Reader = new InputStreamReader(System.in)
      compiler.setBaseURI(new File(System.getProperty("user.dir")).toURI())
      exp = compiler.compile(queryReader)
    } else if (queryFileName.startsWith("{") && queryFileName.endsWith("}")) {
      val q: String = queryFileName.substring(1, queryFileName.length - 1)
      compiler.setBaseURI(new File(System.getProperty("user.dir")).toURI())
      exp = compiler.compile(q)
    } else if (useURLs || CommandLineOptions.isImplicitURI(queryFileName)) {
      var resolver: ModuleURIResolver = compiler.getModuleURIResolver
      var isStandardResolver: Boolean = false
      if (resolver == null) {
        resolver = getConfiguration.getStandardModuleURIResolver
        isStandardResolver = true
      }
      while (true) {
        val locations: Array[String] = Array(queryFileName)
        var sources: Array[Source] = null
        try sources = resolver.resolve(null, null, locations).asInstanceOf[Array[Source]]
        catch {
          case e: Exception =>
            if (e.isInstanceOf[XPathException]) {
              throw new SaxonApiException(e)
            } else {
              val xe: XPathException =
                new XPathException("Exception in ModuleURIResolver: ", e)
              xe.setErrorCode("XQST0059")
              throw new SaxonApiException(xe)
            }

        }
        if (sources == null) {
          if (isStandardResolver) {
            quit("System problem: standard ModuleURIResolver returned null", 4)
          } else {
            resolver = getConfiguration.getStandardModuleURIResolver
            isStandardResolver = true
          }
        } else {
          if (sources.length != 1 || !(sources(0)
            .isInstanceOf[StreamSource])) {
            quit("Module URI Resolver must return a single StreamSource", 2)
          }
          val queryText: String = QueryReader.readSourceQuery(
            sources(0).asInstanceOf[StreamSource],
            config.getValidCharacterChecker)
          exp = compiler.compile(queryText)
          //break
        }
      }
    } else {
      Using(new FileInputStream(queryFileName)) { queryStream =>
        compiler.setBaseURI(new File(queryFileName).toURI())
        exp = compiler.compile(queryStream)
      }
    }
    exp
  }

   def explain(exp: XQueryExpression): Unit = {
    var explainOutput: OutputStream = null
    explainOutput =
      if (explainOutputFileName == null || "" == explainOutputFileName)
        System.err
      else new FileOutputStream(new File(explainOutputFileName))
    val props: SerializationProperties =
      ExpressionPresenter.makeDefaultProperties(config)
    val diag: Receiver = config.getSerializerFactory
      .getReceiver(new StreamResult(explainOutput), props)
    val expressionPresenter: ExpressionPresenter =
      new ExpressionPresenter(config, diag)
    exp.explain(expressionPresenter)
  }

  def processSource(sourceInput: Source,
                    exp: XQueryExecutable,
                    evaluator: XQueryEvaluator): Unit = {
    var sourceIn = sourceInput
    if (sourceIn != null && !streaming) {
      val builder: DocumentBuilder = processor.newDocumentBuilder()
      if (exp.isUpdateQuery) {
        builder.setTreeModel(TreeModel.LINKED_TREE)
      }
      if (showTime) {
        System.err.println("Processing " + sourceIn.getSystemId)
      }
      if (!exp.getUnderlyingCompiledQuery.usesContextItem()) {
        System.err.println(
          "Source document ignored - query can be evaluated without reference to the context item")
        return
      }
      if (projection) {
        builder.setDocumentProjectionQuery(exp)
        if (explain) {
          exp.getUnderlyingCompiledQuery.explainPathMap()
        }
      }
      builder.setDTDValidation(
        getConfiguration.getBooleanProperty(Feature.DTD_VALIDATION))
      if (getConfiguration.getBooleanProperty(
        Feature.DTD_VALIDATION_RECOVERABLE)) {
        sourceIn = new AugmentedSource(sourceIn, getConfiguration.getParseOptions)
      }
      val doc: XdmNode = builder.build(sourceIn)
      evaluator.setContextItem(doc)
    }
  }

   def runQuery(exp: XQueryExecutable,
                         evaluator: XQueryEvaluator,
                         input: Source,
                         destination: Destination): Unit = {
    try if (wrap) {
      val e: XQueryExpression = exp.getUnderlyingCompiledQuery
      val results: SequenceIterator =
        e.iterator(evaluator.getUnderlyingQueryContext)
      val resultDoc: NodeInfo = QueryResult.wrap(results, config)
      val wrappedResultDoc: XdmValue = XdmValue.wrap(resultDoc)
      processor.writeXdmValue(wrappedResultDoc, destination)
      destination.closeAndNotify()
    } else if (streaming) {
      evaluator.runStreamed(input, destination)
    } else {
      evaluator.run(destination)
    } finally if (closeTraceDestination && traceDestination != null) {
      traceDestination.close()
    }
  }

   def runUpdate(exp: XQueryExecutable,
                          evaluator: XQueryEvaluator,
                          serializer: Serializer): Unit = {
    try {
      if (serializer.getOutputProperty(Serializer.Property.METHOD) ==
        null) {
        serializer.setOutputProperty(Serializer.Property.METHOD, "xml")
      }
      if (writeback) {
        val errors: List[SaxonApiException] =
          new ArrayList[SaxonApiException](3)
        val agent: UpdateAgent = (node, controller) =>
          try {
            var pool: DocumentPool = controller.getDocumentPool
            var documentURI: String = pool.getDocumentURI(node)
            if (documentURI != null) {
              rewriteToDisk(node,
                serializer,
                backup,
                if (showTime) System.err else null)
            } else if (showTime) {
              System.err.println(
                "Updated document discarded because it was not read using doc()")
            }
          } catch {
            case err: SaxonApiException => {
              System.err.println(err.getMessage)
              errors.add(err)
            }

          }
        evaluator.run()
        exp.getUnderlyingCompiledQuery
          .runUpdate(evaluator.getUnderlyingQueryContext, agent)
        if (!errors.isEmpty) {
          throw errors.get(0)
        }
      } else {
        if (evaluator.getContextItem != null) {
          val affectedDocuments: Set[MutableNodeInfo] =
            exp.getUnderlyingCompiledQuery.runUpdate(
              evaluator.getUnderlyingQueryContext)
          val initial: Item =
            evaluator.getContextItem.getUnderlyingValue.head()
          if (initial.isInstanceOf[NodeInfo] && affectedDocuments.contains(
            initial)) {
            processor.writeXdmValue(evaluator.getContextItem, serializer)
          }
        }
      }
    } finally if (closeTraceDestination && traceDestination != null) {
      traceDestination.close()
    }
  }

   def quit(message: String, code: Int): Unit = {
    System.err.println(message)
    if (allowExit) {
      System.exit(code)
    } else {
      throw new RuntimeException(message)
    }
  }

   def badUsage(message: String): Unit = {
    if ("" != message) {
      System.err.println(message)
    }
    if (!showTime) {
      System.err.println(config.getProductTitle)
    }
    System.err.println("Usage: see http://www.saxonica.com/documentation/index.html#!using-xquery/commandline")
    System.err.println(
      "Format: " + CommandLineOptions.getCommandName(this) +
        " options params")
    val options: CommandLineOptions = new CommandLineOptions()
    this.setPermittedOptions(options)
    System.err.println(
      "Options available:" + options.displayPermittedOptions())
    System.err.println(
      "Use -XYZ:? for details of option XYZ or --? to list configuration features")
    System.err.println("Params: ")
    System.err.println("  param=value           Set query string parameter")
    System.err.println("  +param=filename       Set query document parameter")
    System.err.println(
      "  ?param=expression     Set query parameter using XPath")
    System.err.println("  !param=value          Set serialization parameter")
    if (allowExit) {
      System.exit(if ("" == message) 0 else 2)
    } else {
      throw new RuntimeException(message)
    }
  }

  private def getCommandName(): String = {
    var s: String = getClass.getName
    if (s.==("cli.Saxon.Cmd.DotNetQuery")) {
      s = "Query"
    }
    s
  }

}
