package org.orbeon.saxon.trans

import javax.xml.transform.{Source, TransformerException}
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.event.Builder
import org.orbeon.saxon.functions.AccessorFn
import org.orbeon.saxon.lib._
import org.orbeon.saxon.s9api._
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{DayTimeDurationValue, NumericValue}
import org.xml.sax.{InputSource, XMLReader}
import java.math.BigDecimal
import java.text.Collator
import java.util._

import org.orbeon.saxon.trans.CommandLineOptions._
import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

object CommandLineOptions {

  val TYPE_BOOLEAN: Int = 1
  val TYPE_FILENAME: Int = 2
  val TYPE_CLASSNAME: Int = 3
  val TYPE_ENUMERATION: Int = 4
  val TYPE_INTEGER: Int = 5
  val TYPE_QNAME: Int = 6
  val TYPE_FILENAME_LIST: Int = 7
  val TYPE_DATETIME: Int = 8
  val TYPE_STRING: Int = 9
  val TYPE_INTEGER_PAIR: Int = 10
  val VALUE_REQUIRED: Int = 1 << 8
  val VALUE_PROHIBITED: Int = 2 << 8

  def isImplicitURI(name: String) = name.startsWith("http:") || name.startsWith("https:") || name.startsWith("file:") || name.startsWith("classpath:")

  private def displayPermittedValues(permittedValues: Set[String]): String = {
    val sb: FastStringBuffer = new FastStringBuffer(20)
    for (perVal <- permittedValues.asScala) {
      if ("" == perVal) {
        sb.append("\"\"")
      } else {
        sb.append(perVal)
      }
      sb.cat('|')
    }
    sb.setLength(sb.length - 1)
    sb.toString
  }

  trait ParamSetter {

    def setParam(qName: QName, value: XdmValue): Unit

  }

  def loadDocuments(sourceFileName: String,
                    useURLs: Boolean,
                    processor: Processor,
                    useSAXSource: Boolean,
                    sources: List[Source]): Boolean = {
    var sourceInput: Source = null
    var parser: XMLReader = null
    val config: Configuration = processor.getUnderlyingConfiguration
    if (useURLs || isImplicitURI(sourceFileName)) {
      sourceInput = config.getURIResolver.resolve(sourceFileName, null)
      if (sourceInput == null) {
        sourceInput = config.getSystemURIResolver.resolve(sourceFileName, null)
      }
      sources.add(sourceInput)
      false
    } else if (sourceFileName.==("-")) {
      if (useSAXSource) {
        parser = config.getSourceParser
        sourceInput = new SAXSource(parser, new InputSource(System.in))
      } else {
        sourceInput = new StreamSource(System.in)
      }
      sources.add(sourceInput)
      false
    } else {
      // ORBEON: No `File` support.
      false
    }
//      val sourceFile = new File(sourceFileName)
//      if (! sourceFile.exists()) {
//        throw new SaxonApiException(
//          "Source file " + sourceFile + " does not exist")
//      }
//      if (sourceFile.isDirectory) {
//        parser = config.getSourceParser
//        val files: Array[String] = sourceFile.list()
//        if (files != null) {
//          for (file1 <- files) {
//            val file: File = new File(sourceFile, file1)
//            if (!file.isDirectory) {
//              if (useSAXSource) {
//                val eis: InputSource = new InputSource(file.toURI().toString)
//                sourceInput = new SAXSource(parser, eis)
//              } else {
//                sourceInput = new StreamSource(file.toURI().toString)
//              }
//              sources.add(sourceInput)
//            }
//          }
//        }
//        true
//      } else {
//        if (useSAXSource) {
//          val eis: InputSource = new InputSource(sourceFile.toURI().toString)
//          sourceInput = new SAXSource(config.getSourceParser, eis)
//        } else {
//          sourceInput = new StreamSource(sourceFile.toURI().toString)
//        }
//        sources.add(sourceInput)
//        false
//      }
//    }
  }

//  def loadAdditionalSchemas(config: Configuration,
//                            additionalSchemas: String): Unit = {
//    val st: StringTokenizer =
//      new StringTokenizer(additionalSchemas, File.pathSeparator)
//    while (st.hasMoreTokens()) {
//      val schema: String = st.nextToken()
//      val schemaFile: File = new File(schema)
//      if (!schemaFile.exists()) {
//        throw new SchemaException("Schema document " + schema + " not found")
//      }
//      config.addSchemaSource(new StreamSource(schemaFile))
//    }
//  }

  def featureKeys(): String = {
    val index = "http://saxon.sf.net/feature/".length
    val sb: StringBuilder = new StringBuilder()
    Feature.getNames.forEachRemaining((s) =>
      sb.append("\n  ").append(s.substring(index)))
    sb.toString
  }

  private val milliSecond: DayTimeDurationValue =
    new DayTimeDurationValue(1, 0, 0, 0, 0, 1000)

  def showExecutionTimeNano(nanosecs: Long): String =
    if (nanosecs < 1e9) {
      (nanosecs / 1e6).toString + "ms"
    } else {
      try {
        val millisecs: Double = nanosecs / 1e6
        val d: DayTimeDurationValue = milliSecond.multiply(millisecs)
        val days: Long = d
          .getComponent(AccessorFn.Component.DAY)
          .asInstanceOf[NumericValue]
          .longValue()
        val hours: Long = d
          .getComponent(AccessorFn.Component.HOURS)
          .asInstanceOf[NumericValue]
          .longValue()
        val minutes: Long = d
          .getComponent(AccessorFn.Component.MINUTES)
          .asInstanceOf[NumericValue]
          .longValue()
        val seconds: BigDecimal = d
          .getComponent(AccessorFn.Component.SECONDS)
          .asInstanceOf[NumericValue]
          .getDecimalValue
        val fsb: FastStringBuffer = new FastStringBuffer(256)
        if (days > 0) {
          fsb.append(days.toString + "days ")
        }
        if (hours > 0) {
          fsb.append(hours.toString + "h ")
        }
        if (minutes > 0) {
          fsb.append(minutes.toString + "m ")
        }
        fsb.append(seconds.toString + "s")
        fsb.toString + " (" + (nanosecs / 1e6).toString + "ms)"
      } catch {
        case e: XPathException => (nanosecs / 1e6).toString + "ms"

      }
    }

  def getCommandName(command: AnyRef): String = {
    var s: String = command.getClass.getName
    if (s.startsWith("cli.Saxon.Cmd.DotNet")) {
      s = s.substring("cli.Saxon.Cmd.DotNet".length)
    }
    s
  }

  def showMemoryUsed(): Unit = {
    val value: Long = Runtime.getRuntime.totalMemory() - Runtime.getRuntime
      .freeMemory()
    System.err.println("Memory used: " + (value / 1000000) + "Mb")
  }

}

class CommandLineOptions {

  private val recognizedOptions: HashMap[String, Integer] = new HashMap()

  private val optionHelp: HashMap[String, String] = new HashMap()

  private val namedOptions: Properties = new Properties()

  private val configOptions: Properties = new Properties()

  private val permittedValues: Map[String, Set[String]] = new HashMap()

  private val defaultValues: Map[String, String] = new HashMap()

  @BeanProperty
  val positionalOptions: List[String] = new ArrayList()

  private val paramValues: Properties = new Properties()

  private val paramExpressions: Properties = new Properties()

  private val paramFiles: Properties = new Properties()

  private val serializationParams: Properties = new Properties()

  def addRecognizedOption(option: String,
                          optionProperties: Int,
                          helpText: String): Unit = {
    recognizedOptions.put(option, optionProperties)
    optionHelp.put(option, helpText)
    if ((optionProperties & 0xff) == TYPE_BOOLEAN) {
      setPermittedValues(option, Array("on", "off"), "on")
    }
  }

  def setPermittedValues(option: String,
                         values: Array[String],
                         defaultValue: String): Unit = {
    val valueSet: Set[String] = new HashSet[String](Arrays.asList(values: _*))
    permittedValues.put(option, valueSet)
    if (defaultValue != null) {
      defaultValues.put(option, defaultValue)
    }
  }

  def setActualOptions(args: Array[String]): Unit = {
    for (arg <- args) {
      if ("-" == arg) {
        positionalOptions.add(arg)
      } else if (arg.==("--?")) {
        System.err.println("Configuration features:" + featureKeys())
      } else if (arg.charAt(0) == '-') {
        var option: String = null
        var value: String = ""
        if (arg.length > 5 && arg.charAt(1) == '-') {
          val colon: Int = arg.indexOf(':')
          if (colon > 0 && colon < arg.length - 1) {
            option = arg.substring(2, colon)
            value = arg.substring(colon + 1)
            configOptions.setProperty(option, value)
          } else if (colon > 0 && colon == arg.length - 1) {
            option = arg.substring(2, colon)
            configOptions.setProperty(option, "")
          } else {
            option = arg.substring(2)
            configOptions.setProperty(option, "true")
          }
        } else {
          val colon: Int = arg.indexOf(':')
          if (colon > 0 && colon < arg.length - 1) {
            option = arg.substring(1, colon)
            value = arg.substring(colon + 1)
          } else {
            option = arg.substring(1)
          }
          if (recognizedOptions.get(option) == null) {
            throw new XPathException(
              "Command line option -" + option + " is not recognized. Options available: " +
                displayPermittedOptions())
          }
          if (namedOptions.getProperty(option) != null) {
            throw new XPathException(
              "Command line option -" + option + " appears more than once")
          } else if ("?" == value) {
            displayOptionHelp(option)
            throw new XPathException("No processing requested")
          } else {
            if ("" == value) {
              val prop: Int = recognizedOptions.get(option)
              if ((prop & VALUE_REQUIRED) != 0) {
                var msg: String = "Command line option -" + option + " requires a value"
                if (permittedValues.get(option) != null) {
                  msg += ": permitted values are " +
                    displayPermittedValues(permittedValues.get(option))
                }
                throw new XPathException(msg)
              }
              val defaultValue: String = defaultValues.get(option)
              if (defaultValue != null) {
                value = defaultValue
              }
            } else {
              val prop: Int = recognizedOptions.get(option)
              if ((prop & VALUE_PROHIBITED) != 0) {
                val msg = "Command line option -" + option + " does not expect a value"
                throw new XPathException(msg)
              }
            }
            val permitted: Set[String] = permittedValues.get(option)
            if (permitted != null && !permitted.contains(value)) {
              throw new XPathException(
                "Bad option value " + arg + ": permitted values are " +
                  displayPermittedValues(permitted))
            }
            namedOptions.setProperty(option, value)
          }
        }
      } else {
        val eq: Int = arg.indexOf('=')
        if (eq >= 1) {
          val keyword: String = arg.substring(0, eq)
          var value: String = ""
          if (eq < arg.length - 1) {
            value = arg.substring(eq + 1)
          }
          val ch: Char = arg.charAt(0)
          if (ch == '!' && eq >= 2) {
            serializationParams.setProperty(keyword.substring(1), value)
          } else if (ch == '?' && eq >= 2) {
            paramExpressions.setProperty(keyword.substring(1), value)
          } else if (ch == '+' && eq >= 2) {
            paramFiles.setProperty(keyword.substring(1), value)
          } else {
            paramValues.setProperty(keyword, value)
          }
        } else {
          positionalOptions.add(arg)
        }
      }
    }
  }

  def definesParameterValues(): Boolean =
    !serializationParams.isEmpty || !paramExpressions.isEmpty ||
      !paramFiles.isEmpty ||
      !paramValues.isEmpty

  def testIfSchemaAware(): Boolean =
    getOptionValue("sa") != null || getOptionValue("outval") != null ||
      getOptionValue("val") != null ||
      getOptionValue("vlax") != null ||
      getOptionValue("xsd") != null ||
      getOptionValue("xsdversion") != null

  def applyToConfiguration(processor: Processor): Unit = {
    val config: Configuration = processor.getUnderlyingConfiguration
    val e: Enumeration[_] = configOptions.propertyNames()
    while (e.hasMoreElements()) {
      val name: String = e.nextElement().asInstanceOf[String]
      val value: String = configOptions.getProperty(name)
      val fullName = "http://saxon.sf.net/feature/" + name
      if (!name.startsWith("parserFeature?") && !name.startsWith(
        "parserProperty?")) {
        val f: Feature[_] = Feature.byName(fullName)
        if (f == null) {
          throw new XPathException("Unknown configuration feature " + name)
        }
        if (f.typeVar == classOf[Boolean]) {
          Configuration.requireBoolean(name, value)
        } else if (f.typeVar == classOf[Integer]) {
          java.lang.Integer.valueOf(value)
        } else if (f.typeVar != classOf[String]) {
          throw new XPathException(
            "Property --" + name + " cannot be supplied as a string")
        }
      }
      processor.getUnderlyingConfiguration
        .setConfigurationProperty(fullName, value)
    }
    var value: String = getOptionValue("catalog")
    if (value != null) {
      if (getOptionValue("r") != null) {
        throw new XPathException("Cannot use -catalog and -r together")
      }
      if (getOptionValue("x") != null) {
        throw new XPathException("Cannot use -catalog and -x together")
      }
      if (getOptionValue("y") != null) {
        throw new XPathException("Cannot use -catalog and -y together")
      }
      val sb: StringBuilder = new StringBuilder()
      if ((getOptionValue("u") != null) || isImplicitURI(value)) {
        for (s <- value.split(";")) {
          var sourceInput: Source = null
          try sourceInput = config.getURIResolver.resolve(s, null)
          catch {
            case e: TransformerException => {}

          }
          if (sourceInput == null) {
            sourceInput = config.getSystemURIResolver.resolve(s, null)
          }
          sb.append(sourceInput.getSystemId).append(';')
        }
      } else {
        ???
//        for (s <- value.split(";")) {
//          val catalogFile: File = new File(s)
//          if (!catalogFile.exists()) {
//            throw new XPathException("Catalog file not found: " + s)
//          }
//          sb.append(catalogFile.toURI().toASCIIString()).append(';')
//        }
      }
      value = sb.toString
      config.getConfClass("org.apache.xml.resolver.CatalogManager", tracing = false, null)
      XmlCatalogResolver.setCatalog(value, config, getOptionValue("t") != null)
    }
    value = getOptionValue("dtd")
    if (value != null) {
      value match {
        case "on" =>
          config.setBooleanProperty(Feature.DTD_VALIDATION, value = true)
          config.getParseOptions.setDTDValidationMode(Validation.STRICT)
        case "off" =>
          config.setBooleanProperty(Feature.DTD_VALIDATION, value = false)
          config.getParseOptions.setDTDValidationMode(Validation.SKIP)
        case "recover" =>
          config.setBooleanProperty(Feature.DTD_VALIDATION, value = true)
          config.setBooleanProperty(Feature.DTD_VALIDATION_RECOVERABLE, value = true)
          config.getParseOptions.setDTDValidationMode(Validation.LAX)

      }
    }
    value = getOptionValue("ea")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("ea", value)
//      config.getDefaultXsltCompilerInfo.setAssertionsEnabled(on)
    }
    value = getOptionValue("expand")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("expand", value)
      config.getParseOptions.setExpandAttributeDefaults(on)
    }
    value = getOptionValue("ext")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("ext", value)
      config.setBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS, on)
    }
    value = getOptionValue("l")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("l", value)
      config.setBooleanProperty(Feature.LINE_NUMBERING, on)
    }
    value = getOptionValue("m")
    if (value != null) {
      config.setConfigurationProperty(Feature.MESSAGE_EMITTER_CLASS, value)
    }
    value = getOptionValue("opt")
    if (value != null) {
      config.setConfigurationProperty(Feature.OPTIMIZATION_LEVEL, value)
    }
    value = getOptionValue("or")
    if (value != null) {
      val resolver: AnyRef = config.getInstance(value, null).asInstanceOf[AnyRef]
      if (resolver.isInstanceOf[OutputURIResolver]) {
        config.setConfigurationProperty(
          Feature.OUTPUT_URI_RESOLVER,
          resolver.asInstanceOf[OutputURIResolver])
      } else {
        throw new XPathException(
          "Class " + value + " is not an OutputURIResolver")
      }
    }
    value = getOptionValue("outval")
    if (value != null) {
      val isRecover: java.lang.Boolean = "recover" == value
      config.setConfigurationProperty[Boolean](Feature.VALIDATION_WARNINGS, isRecover)
      config.setConfigurationProperty[Boolean](Feature.VALIDATION_COMMENTS, isRecover)
    }
    value = getOptionValue("r")
    if (value != null) {
      config.setURIResolver(config.makeURIResolver(value))
    }
    value = getOptionValue("strip")
    if (value != null) {
      config.setConfigurationProperty(Feature.STRIP_WHITESPACE, value)
    }
    value = getOptionValue("T")
    if (value != null) {
      config.setCompileWithTracing(true)
    }
    value = getOptionValue("TJ")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("TJ", value)
      config.setBooleanProperty(Feature.TRACE_EXTERNAL_FUNCTIONS, on)
    }
    value = getOptionValue("tree")
    if (value != null) {
      value match {
        case "linked" => config.setTreeModel(Builder.LINKED_TREE)
        case "tiny" => config.setTreeModel(Builder.TINY_TREE)
        case "tinyc" => config.setTreeModel(Builder.TINY_TREE_CONDENSED)

      }
    }
    value = getOptionValue("val")
    if (value != null) {
      if ("strict" == value) {
        processor.setConfigurationProperty[Integer](Feature.SCHEMA_VALIDATION,
          Validation.STRICT)
      } else if ("lax" == value) {
        processor.setConfigurationProperty[Integer](Feature.SCHEMA_VALIDATION,
          Validation.LAX)
      }
    }
    value = getOptionValue("x")
    if (value != null) {
      processor.setConfigurationProperty(Feature.SOURCE_PARSER_CLASS, value)
    }
    value = getOptionValue("xi")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("xi", value)
      processor.setConfigurationProperty(Feature.XINCLUDE, on)
    }
    value = getOptionValue("xmlversion")
    if (value != null) {
      processor.setConfigurationProperty(Feature.XML_VERSION, value)
    }
    value = getOptionValue("xsdversion")
    if (value != null) {
      processor.setConfigurationProperty(Feature.XSD_VERSION, value)
    }
    value = getOptionValue("xsiloc")
    if (value != null) {
      val on: Boolean = Configuration.requireBoolean("xsiloc", value)
      processor.setConfigurationProperty(Feature.USE_XSI_SCHEMA_LOCATION, on)
    }
    value = getOptionValue("y")
    if (value != null) {
      processor.setConfigurationProperty(Feature.STYLE_PARSER_CLASS, value)
    }
    value = getOptionValue("init")
    if (value != null) {
      val initializer: Initializer =
        config.getInstance(value, null).asInstanceOf[Initializer]
      initializer.initialize(config)
    }
  }

  def displayPermittedOptions(): String = {
    var options: Array[String] = Array.ofDim[String](recognizedOptions.size)
    options = new ArrayList(recognizedOptions.keySet).toArray(options)
    Arrays.sort(options, Collator.getInstance)
    val sb: FastStringBuffer = new FastStringBuffer(100)
    for (opt <- options) {
      sb.append(" -")
      sb.append(opt)
    }
    sb.append(" --?")
    sb.toString
  }

  private def displayOptionHelp(option: String): Unit = {
    System.err.println("Help for -" + option + " option")
    val prop: Int = recognizedOptions.get(option)
    if ((prop & VALUE_PROHIBITED) == 0) {
      prop & 0xff match {
        case TYPE_BOOLEAN => System.err.println("Value: on|off")
        case TYPE_INTEGER => System.err.println("Value: integer")
        case TYPE_FILENAME => System.err.println("Value: file name")
        case TYPE_FILENAME_LIST =>
          System.err.println("Value: list of file names, semicolon-separated")
        case TYPE_CLASSNAME =>
          System.err.println("Value: Java fully-qualified class name")
        case TYPE_QNAME =>
          System.err.println("Value: QName in Clark notation ({uri}local)")
        case TYPE_STRING => System.err.println("Value: string")
        case TYPE_INTEGER_PAIR => System.err.println("Value: int,int")
        case TYPE_ENUMERATION =>
          var message: String = "Value: one of "
          message += displayPermittedValues(permittedValues.get(option))
          System.err.println(message)
        case _ =>

      }
    }
    System.err.println("Meaning: " + optionHelp.get(option))
  }

  def getOptionValue(option: String): String = namedOptions.getProperty(option)

  def setParams(processor: Processor, paramSetter: ParamSetter): Unit = {
    val en: Enumeration[_] = paramValues.propertyNames()
    while (en.hasMoreElements()) {
      val name: String = en.nextElement().asInstanceOf[String]
      val value: String = paramValues.getProperty(name)
      paramSetter.setParam(QName.fromClarkName(name),
        new XdmAtomicValue(value, ItemType.UNTYPED_ATOMIC))
    }
    applyFileParameters(processor, paramSetter)
    val e: Enumeration[_] = paramExpressions.propertyNames()
    while (e.hasMoreElements()) {
      val name: String = e.nextElement().asInstanceOf[String]
      val value: String = paramExpressions.getProperty(name)
      val xpc: XPathCompiler = processor.newXPathCompiler()
      val xpe: XPathExecutable = xpc.compile(value)
      val `val`: XdmValue = xpe.load().evaluate()
      paramSetter.setParam(QName.fromClarkName(name), `val`)
    }
  }

  private def applyFileParameters(processor: Processor,
                                  paramSetter: ParamSetter): Unit = {
    val useURLs: Boolean = "on" == getOptionValue("u")
    val e = paramFiles.propertyNames()
    while (e.hasMoreElements) {
      val name: String = e.nextElement().asInstanceOf[String]
      val value: String = paramFiles.getProperty(name)
      val sourceList: List[Source] = new ArrayList[Source]()
      loadDocuments(value, useURLs, processor, useSAXSource = true, sourceList)
      if (!sourceList.isEmpty) {
        val nodeList: List[XdmNode] = new ArrayList[XdmNode](sourceList.size)
        val builder: DocumentBuilder = processor.newDocumentBuilder()
        for (s <- sourceList.asScala) {
          nodeList.add(builder.build(s))
        }
        val nodes: XdmValue = new XdmValue(nodeList)
        paramSetter.setParam(QName.fromClarkName(name), nodes)
      } else {
        paramSetter.setParam(QName.fromClarkName(name),
          XdmEmptySequence.getInstance)
      }
    }
  }

  def setSerializationProperties(serializer: Serializer): Unit = {
    val e = serializationParams.propertyNames()
    while (e.hasMoreElements) {
      var name: String = e.nextElement().asInstanceOf[String]
      val value: String = serializationParams.getProperty(name)
      if (name.startsWith("saxon:")) {
        name = "{" + NamespaceConstant.SAXON + "}" + name.substring(6)
      }
      serializer.setOutputProperty(QName.fromClarkName(name), value)
    }
  }

  def applyStaticParams(/*compiler: XsltCompiler*/): Unit = { //  XsltCompiler not exist
    val processor: Processor = new Processor()
    val en = paramValues.propertyNames()
    while (en.hasMoreElements) {
      val name = en.nextElement().asInstanceOf[String]
      val value = paramValues.getProperty(name)
     /* compiler.setParameter(QName.fromClarkName(name),
        new XdmAtomicValue(value, ItemType.UNTYPED_ATOMIC))*/
    }
    val e = paramExpressions.propertyNames()
    while (e.hasMoreElements) {
      val name: String = e.nextElement().asInstanceOf[String]
      val value: String = paramExpressions.getProperty(name)
      val xpc: XPathCompiler = processor.newXPathCompiler()
      val xpe: XPathExecutable = xpc.compile(value)
      val `val`: XdmValue = xpe.load().evaluate()
      //compiler.setParameter(QName.fromClarkName(name), `val`)
    }
  }

  def applyFileParams(processor: Processor/*,
                      transformer: Xslt30Transformer*/): Unit = { // Xslt30Transformer not exist
    if (!paramFiles.isEmpty) {
      val params: Map[QName, XdmValue] = new HashMap[QName, XdmValue]()
      applyFileParameters(processor, params.put)
      //transformer.setStylesheetParameters(params)
    }
  }
}
