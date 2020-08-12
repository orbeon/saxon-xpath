package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Version

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib._

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.ma.arrays.ArrayItemType

import net.sf.saxon.ma.map.HashTrieMap

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.ma.map.MapType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om._

import net.sf.saxon.s9api._

import net.sf.saxon.serialize.CharacterMap

import net.sf.saxon.serialize.CharacterMapIndex

import net.sf.saxon.serialize.SerializationProperties

import net.sf.saxon.trans.StylesheetCache

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XsltController

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.tree.wrapper.RebasedDocument

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value._

import org.xml.sax.InputSource

import javax.xml.transform.OutputKeys

import javax.xml.transform.Source

import javax.xml.transform.TransformerException

import javax.xml.transform.sax.SAXSource

import javax.xml.transform.stream.StreamSource

import java.io.File

import java.io.StringReader

import java.io.StringWriter

import java.net.URI

import scala.jdk.CollectionConverters._

import java.util.ArrayList

import java.util.HashMap

import java.util.List

import java.util.Map

import java.util.concurrent.ConcurrentHashMap

import TransformFn._
import scala.jdk.CollectionConverters._

object TransformFn {

  private var transformOptionNames30: Array[String] = Array(
    "package-name",
    "package-version",
    "package-node",
    "package-location",
    "static-params",
    "global-context-item",
    "template-params",
    "tunnel-params",
    "initial-function",
    "function-params"
  )

  private val dummyBaseOutputUriScheme: String = "dummy"

  def makeOptionsParameter(): OptionsParameter = {
    val op: OptionsParameter = new OptionsParameter()
    op.addAllowedOption("xslt-version", SequenceType.SINGLE_DECIMAL)
    op.addAllowedOption("stylesheet-location", SequenceType.SINGLE_STRING)
    op.addAllowedOption("stylesheet-node", SequenceType.SINGLE_NODE)
    op.addAllowedOption("stylesheet-text", SequenceType.SINGLE_STRING)
    op.addAllowedOption("stylesheet-base-uri", SequenceType.SINGLE_STRING)
    op.addAllowedOption("base-output-uri", SequenceType.SINGLE_STRING)
    op.addAllowedOption(
      "stylesheet-params",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption("source-node", SequenceType.SINGLE_NODE)
    op.addAllowedOption("source-location", SequenceType.SINGLE_STRING)
    op.addAllowedOption("initial-mode", SequenceType.SINGLE_QNAME)
    op.addAllowedOption("initial-match-selection", SequenceType.ANY_SEQUENCE)
    op.addAllowedOption("initial-template", SequenceType.SINGLE_QNAME)
    op.addAllowedOption("delivery-format", SequenceType.SINGLE_STRING)
    op.addAllowedOption(
      "serialization-params",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption(
      "vendor-options",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption("cache", SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("package-name", SequenceType.SINGLE_STRING)
    op.addAllowedOption("package-version", SequenceType.SINGLE_STRING)
    op.addAllowedOption("package-node", SequenceType.SINGLE_NODE)
    op.addAllowedOption("package-location", SequenceType.SINGLE_STRING)
    op.addAllowedOption(
      "static-params",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption("global-context-item", SequenceType.SINGLE_ITEM)
    op.addAllowedOption(
      "template-params",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption(
      "tunnel-params",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption("initial-function", SequenceType.SINGLE_QNAME)
    op.addAllowedOption("function-params", ArrayItemType.SINGLE_ARRAY)
    op.addAllowedOption(
      "requested-properties",
      SequenceType.makeSequenceType(MapType.ANY_MAP_TYPE,
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption(
      "post-process",
      SequenceType.makeSequenceType(
        new SpecificFunctionType(Array(SequenceType.SINGLE_STRING,
          SequenceType.ANY_SEQUENCE),
          SequenceType.ANY_SEQUENCE),
        StaticProperty.EXACTLY_ONE)
    )
    op
  }

  private def validate(node: NodeInfo,
                       config: Configuration,
                       validation: Int): NodeInfo = {
    val options: ParseOptions = new ParseOptions(config.getParseOptions)
    options.setSchemaValidationMode(validation)
    config.buildDocumentTree(node, options).getRootNode
  }

  object Deliverer {

    def makeDeliverer(deliveryFormat: String): Deliverer =
      deliveryFormat match {
        case "document" => new DocumentDeliverer()
        case "serialized" => new SerializedDeliverer()
        case "raw" => new RawDeliverer()
        case _ => throw new IllegalArgumentException("delivery-format")

      }

  }

  abstract class Deliverer extends ResultDocumentResolver {

    //  var transformer: Xslt30Transformer = _ // Xslt30Transformer.scala not found

    var baseOutputUri: String = _

    var principalResultKey: String = _

    var postProcessor: Function = _

    var context: XPathContext = _

    var resultMap: HashTrieMap = new HashTrieMap()

    /* def setTransformer(transformer: Xslt30Transformer): Unit = {
       this.transformer = transformer
     }*/

    def setPrincipalResultKey(key: String): Unit = {
      this.principalResultKey = key
    }

    def setBaseOutputUri(uri: String): Unit = {
      this.baseOutputUri = uri
    }

    def setPostProcessor(postProcessor: Function,
                         context: XPathContext): Unit = {
      this.postProcessor = postProcessor
      this.context = context
    }

    def getAbsoluteUri(href: String, baseUri: String): URI = {
      var absolute: URI = null
      absolute = ResolveURI.makeAbsolute(href, baseUri)
      absolute
    }

    def populateResultMap(resultMap: HashTrieMap): HashTrieMap

    def getPrimaryDestination(serializationParamsMap: MapItem): Destination

    def makeSerializer(serializationParamsMap: MapItem): Serializer = {
       val serializer: Serializer = new Serializer(new Processor())
      if (serializationParamsMap != null) {
        val paramIterator: AtomicIterator[_ <: AtomicValue] = serializationParamsMap.keys
        var param: AtomicValue = null
        while ((param = paramIterator.next()) != null) {
          var paramName: QName = null
          if (param.isInstanceOf[StringValue]) {
            paramName = new QName(param.getStringValue)
          } else if (param.isInstanceOf[QNameValue]) {
            paramName = new QName(
              param.head().asInstanceOf[QNameValue].getStructuredQName)
          } else {
            throw new XPathException(
              "Serialization parameters must be strings or QNames",
              "XPTY0004")
          }
          var paramValue: String = null
          val supplied: GroundedValue = serializationParamsMap.get(param)
          if (supplied.getLength > 0) {
            if (supplied.getLength == 1) {
              val `val`: Item = supplied.itemAt(0)
              if (`val`.isInstanceOf[StringValue]) {
                paramValue = `val`.getStringValue
              } else if (`val`.isInstanceOf[BooleanValue]) {
                paramValue =
                  if (`val`.asInstanceOf[BooleanValue].getBooleanValue) "yes"
                  else "no"
              } else if (`val`.isInstanceOf[DecimalValue]) {
                paramValue = `val`.getStringValue
              } else if (`val`.isInstanceOf[QNameValue]) {
                paramValue =
                  `val`.asInstanceOf[QNameValue].getStructuredQName.getEQName
              } else if (`val`.isInstanceOf[MapItem] &&
                paramName.getClarkName == SaxonOutputKeys.USE_CHARACTER_MAPS) {
                val charMap: CharacterMap =
                  Serialize.toCharacterMap(`val`.asInstanceOf[MapItem])
                val charMapIndex: CharacterMapIndex = new CharacterMapIndex()
                charMapIndex.putCharacterMap(charMap.getName, charMap)
                serializer.setCharacterMap(charMapIndex)
                 val existing: String = serializer.getOutputProperty(
                   Serializer.Property.USE_CHARACTER_MAPS)
                 if (existing == null) {
                   serializer.setOutputProperty(
                     Serializer.Property.USE_CHARACTER_MAPS,
                     charMap.getName.getEQName)
                 } else {
                   serializer.setOutputProperty(
                     Serializer.Property.USE_CHARACTER_MAPS,
                     existing + " " + charMap.getName.getEQName)
                 }
                //continue
              }
            }
            if (paramValue == null) {
              val iter: SequenceIterator = supplied.iterate()
              var it: Item = null
              paramValue = ""
              while ((it = iter.next()) != null) if (it.isInstanceOf[
                QNameValue]) {
                paramValue += " " +
                  it.asInstanceOf[QNameValue].getStructuredQName.getEQName
              } else {
                throw new XPathException(
                  "Value of serialization parameter " + paramName.getEQName +
                    " not recognized",
                  "XPTY0004")
              }
            }
            val prop: Serializer.Property.Property = Serializer.getProperty(paramName)
            if (paramName.getClarkName == OutputKeys.CDATA_SECTION_ELEMENTS ||
              paramName.getClarkName == SaxonOutputKeys.SUPPRESS_INDENTATION) {
               val existing: String = serializer.getOutputProperty(paramName)
               if (existing == null) {
                 serializer.setOutputProperty(prop, paramValue)
               } else {
                 serializer.setOutputProperty(prop, existing + paramValue)
               }
             } else {
               serializer.setOutputProperty(prop, paramValue)
             }
            }
          }
        }
        serializer
      }

      def getPrimaryResult(): Sequence

      def postProcess(uri: String, result: Sequence): GroundedValue = {
        var seqRes = result
        if (postProcessor != null) {
          seqRes = postProcessor.call(context.newCleanContext(),
            Array(new StringValue(uri), seqRes))
        }
        seqRes.materialize()
      }

    }

    private class DocumentDeliverer extends Deliverer {

      private var results: Map[String, TreeInfo] = new ConcurrentHashMap()

      private var destination: XdmDestination = new XdmDestination()

      override def getPrimaryDestination(
                                          serializationParamsMap: MapItem): Destination = destination

      override def getPrimaryResult(): Sequence = {
        val node: XdmNode = destination.getXdmNode
        if (node == null) null
        else postProcess(baseOutputUri, node.getUnderlyingNode)
      }

      override def resolve(context: XPathContext,
                           href: String,
                           baseUri: String,
                           properties: SerializationProperties): Receiver = {
        val absolute: URI = getAbsoluteUri(href, baseUri)
        val destination: XdmDestination = new XdmDestination()
        destination.setDestinationBaseURI(absolute)
        destination.onClose(() => {
          val root: XdmNode = destination.getXdmNode
          results.put(absolute.toASCIIString(),
            root.getUnderlyingValue.getTreeInfo)
        })
        val pipe: PipelineConfiguration =
          context.getController.makePipelineConfiguration
        destination.getReceiver(pipe, properties)
      }

      def populateResultMap(resultMap: HashTrieMap): HashTrieMap = {
        var resMap = resultMap
        for ((key, value) <- results.asScala) {
          val uri: String = key
          resMap = resMap.addEntry(new StringValue(uri), value.getRootNode)
        }
        resMap
      }

    }

    private class SerializedDeliverer extends Deliverer {

      private var results: Map[String, String] = new ConcurrentHashMap()

      private var workInProgress: Map[String, StringWriter] =
        new ConcurrentHashMap()

      private var primaryWriter: StringWriter = _

      override def getPrimaryDestination(
                                          serializationParamsMap: MapItem): Destination = {
        val serializer: Serializer = makeSerializer(serializationParamsMap)
        primaryWriter = new StringWriter()
        serializer.setOutputWriter(primaryWriter)
        serializer
      }

      override def getPrimaryResult(): Sequence = {
        val str: String = primaryWriter.toString
        if (str.isEmpty) {
          return null
        }
        postProcess(baseOutputUri, new StringValue(str))
      }

      override def resolve(context: XPathContext,
                           href: String,
                           baseUri: String,
                           properties: SerializationProperties): Receiver = {
        val absolute: URI = getAbsoluteUri(href, baseUri)
        if (absolute.getScheme == dummyBaseOutputUriScheme) {
          throw new XPathException(
            "The location of output documents is undefined: use the transform option base-output-uri",
            "FOXT0002")
        }
        val writer: StringWriter = new StringWriter()
        val serializer: Serializer = makeSerializer(null)
        serializer.setCharacterMap(properties.getCharacterMapIndex)
        serializer.setOutputWriter(writer)
        serializer.onClose(() =>
          results.put(absolute.toASCIIString(), writer.toString))
        val pipe: PipelineConfiguration =
          context.getController.makePipelineConfiguration
        val out: Receiver = serializer.getReceiver(pipe, properties)
        out.setSystemId(absolute.toASCIIString())
        out
      }

      override def populateResultMap(resultMap: HashTrieMap): HashTrieMap = {
        var resMap = resultMap
        for ((key, value) <- results.asScala) {
          val uri: String = key
          resMap =
            resMap.addEntry(new StringValue(uri), new StringValue(value))
        }
        resMap
      }

    }

    private class RawDeliverer extends Deliverer {

      private var results: Map[String, XdmValue] = new ConcurrentHashMap()

      private var primaryDestination: RawDestination = new RawDestination()

      override def getPrimaryDestination(
                                          serializationParamsMap: MapItem): Destination = primaryDestination

      override def getPrimaryResult(): Sequence = {
        val actualResult: Sequence =
          primaryDestination.getXdmValue.getUnderlyingValue
        postProcess(baseOutputUri, actualResult)
      }

      override def resolve(context: XPathContext,
                           href: String,
                           baseUri: String,
                           properties: SerializationProperties): Receiver = {
        val absolute: URI = getAbsoluteUri(href, baseUri)
        val destination: RawDestination = new RawDestination()
        destination.onClose(() =>
          results.put(absolute.toASCIIString(), destination.getXdmValue))
        val pipe: PipelineConfiguration =
          context.getController.makePipelineConfiguration
        destination.getReceiver(pipe, properties)
      }

      override def populateResultMap(resultMap: HashTrieMap): HashTrieMap = {
        var resMap = resultMap
        for ((key, value) <- results.asScala) {
          val uri: String = key
          resMap =
            resMap.addEntry(new StringValue(uri), value.getUnderlyingValue)
        }
        resMap
      }

    }

  }

  class TransformFn extends SystemFunction with Callable {

    private def isTransformOptionName30(string: String): Boolean =
      transformOptionNames30.find(_ == string).map(_ => true).getOrElse(false)

    private def checkTransformOptions(options: Map[String, Sequence],
                                      context: XPathContext,
                                      isXslt30Processor: Boolean): Unit = {
      if (options.isEmpty) {
        throw new XPathException("No transformation options supplied",
          "FOXT0002")
      }
      for (keyName <- options.keySet.asScala
           if isTransformOptionName30(keyName) && !isXslt30Processor) {
        throw new XPathException(
          "The transform option " + keyName +
            " is only available when using an XSLT 3.0 processor",
          "FOXT0002")
      }
    }

    private def checkStylesheetMutualExclusion(
                                                map: Map[String, Sequence]): String =
      exactlyOneOf(map,
        "stylesheet-location",
        "stylesheet-node",
        "stylesheet-text")

    private def checkStylesheetMutualExclusion30(
                                                  map: Map[String, Sequence]): String = {
      val styleOption: String = exactlyOneOf(map,
        "stylesheet-location",
        "stylesheet-node",
        "stylesheet-text",
        "package-name",
        "package-node",
        "package-location")
      if (styleOption.==("package-location")) {
        throw new XPathException(
          "The transform option " + styleOption + " is not implemented in Saxon",
          "FOXT0002")
      }
      styleOption
    }

    private def checkInvocationMutualExclusion(
                                                options: Map[String, Sequence]): String =
      oneOf(options, "initial-mode", "initial-template")

    private def oneOf(map: Map[String, Sequence], keys: String*): String = {
      var found: String = null
      for (s <- keys if map.get(s) != null) {
        if (found != null) {
          throw new XPathException(
            "The following transform options are mutually exclusive: " +
              enumerate(keys: _*),
            "FOXT0002")
        } else {
          found = s
        }
      }
      found
    }

    private def exactlyOneOf(map: Map[String, Sequence], keys: String*): String = {
      val found: String = oneOf(map, keys: _*)
      if (found == null) {
        throw new XPathException(
          "One of the following transform options must be present: " +
            enumerate(keys: _*))
      }
      found
    }

    private def enumerate(keys: String*): String = {
      var first: Boolean = true
      val buffer: FastStringBuffer = new FastStringBuffer(256)
      for (k <- keys) {
        if (first) {
          first = false
        } else {
          buffer.append(" | ")
        }
        buffer.append(k)
      }
      buffer.toString
    }

    private def checkInvocationMutualExclusion30(
                                                  map: Map[String, Sequence]): String =
      oneOf(map, "initial-mode", "initial-template", "initial-function")

    private def unsuitable(option: String, value: String): Unit = {
      throw new XPathException(
        "No XSLT processor is available with xsl:" + option +
          " = " +
          value,
        "FOXT0001")
    }

    private def asBoolean(value: AtomicValue): Boolean = {
      if (value.isInstanceOf[BooleanValue]) {
        value.asInstanceOf[BooleanValue].getBooleanValue
      } else if (value.isInstanceOf[StringValue]) {
        val s: String =
          Whitespace.normalizeWhitespace(value.getStringValue).toString
        if (s.==("yes") || s.==("true") || s.==("1")) {
          true
        } else if (s.==("no") || s.==("false") || s.==("0")) {
          false
        }
      }
      throw new XPathException("Unrecognized boolean value " + value, "FOXT0002")
    }

    private def setRequestedProperties(options: Map[String, Sequence],
                                       processor: Processor): Unit = {
      val requestedProps: MapItem =
        options.get("requested-properties").head().asInstanceOf[MapItem]
      val optionIterator: AtomicIterator[_ <: AtomicValue] = requestedProps.keys
      while (true) {
        val option: AtomicValue = optionIterator.next()
        if (option != null) {
          val optionName: StructuredQName =
            option.head().asInstanceOf[QNameValue].getStructuredQName
          val value: AtomicValue =
            requestedProps.get(option).head().asInstanceOf[AtomicValue]
          if (optionName.hasURI(NamespaceConstant.XSLT)) {
            val localName: String = optionName.getLocalPart
            localName match {
              case "vendor-url" =>
                if (!(value.getStringValue.contains("saxonica.com") || value.getStringValue
                  .==("Saxonica"))) {
                  unsuitable("vendor-url", value.getStringValue)
                }
              case "product-name" =>
                if (value.getStringValue.!=("SAXON")) {
                  unsuitable("vendor-url", value.getStringValue)
                }
              case "product-version" =>
                if (!Version.getProductVersion.startsWith(value.getStringValue)) {
                  unsuitable("product-version", value.getStringValue)
                }
              case "is-schema-aware" => {
                val b: Boolean = asBoolean(value)
                if (b) {
                  if (processor.getUnderlyingConfiguration.isLicensedFeature(
                    Configuration.LicenseFeature.ENTERPRISE_XSLT)) {
                    processor.setConfigurationProperty(Feature.XSLT_SCHEMA_AWARE,
                      true)
                  } else {
                    unsuitable("is-schema-aware", value.getStringValue)
                  }
                } else {
                  if (processor.getUnderlyingConfiguration.isLicensedFeature(
                    Configuration.LicenseFeature.ENTERPRISE_XSLT)) {
                    unsuitable("is-schema-aware", value.getStringValue)
                  }
                }
                //break
              }
              case "supports-serialization" => {
                val b: Boolean = asBoolean(value)
                if (!b) {
                  unsuitable("supports-serialization", value.getStringValue)
                }
                //break
              }
              case "supports-backwards-compatibility" => {
                val b: Boolean = asBoolean(value)
                if (!b) {
                  unsuitable("supports-backwards-compatibility",
                    value.getStringValue)
                }
                //break
              }
              case "supports-namespace-axis" => {
                val b: Boolean = asBoolean(value)
                if (!b) {
                  unsuitable("supports-namespace-axis", value.getStringValue)
                }
                //break
              }
              case "supports-streaming" => {
                val b: Boolean = asBoolean(value)
                if (b) {
                  if (!processor.getUnderlyingConfiguration.isLicensedFeature(
                    Configuration.LicenseFeature.ENTERPRISE_XSLT)) {
                    unsuitable("supports-streaming", value.getStringValue)
                  }
                } else {
                  if (processor.getUnderlyingConfiguration.isLicensedFeature(
                    Configuration.LicenseFeature.ENTERPRISE_XSLT)) {
                    processor.setConfigurationProperty(Feature.STREAMABILITY,
                      "off")
                  }
                }
                //break
              }
              case "supports-dynamic-evaluation" => {
                val b: Boolean = asBoolean(value)
                if (!b) {
                  processor.setConfigurationProperty(
                    Feature.DISABLE_XSL_EVALUATE,
                    true)
                }
                //break
              }
              case "supports-higher-order-functions" => {
                val b: Boolean = asBoolean(value)
                if (!b) {
                  unsuitable("supports-higher-order-functions",
                    value.getStringValue)
                }
                //break
              }
              case "xpath-version" => {
                val v: String = value.getStringValue
                try if (java.lang.Double.parseDouble(v) > 3.1) {
                  unsuitable("xpath-version", value.getStringValue)
                } catch {
                  case nfe: NumberFormatException =>
                    unsuitable("xpath-version", value.getStringValue)

                }
                //break
              }
              case "xsd-version" => {
                val v: String = value.getStringValue
                try if (java.lang.Double.parseDouble(v) > 1.1) {
                  unsuitable("xsd-version", value.getStringValue)
                } catch {
                  case nfe: NumberFormatException =>
                    unsuitable("xsd-version", value.getStringValue)

                }
                //break
              }

            }
          }
        } else {
          //break
        }
      }
    }

    private def setStaticParams(options: Map[String, Sequence],
                                /*xsltCompiler: XsltCompiler,*/
                                // XsltCompiler not found
                                allowTypedNodes: Boolean): Unit = {
      val staticParamsMap: MapItem =
        options.get("static-params").head().asInstanceOf[MapItem]
      val paramIterator: AtomicIterator[_ <: AtomicValue] = staticParamsMap.keys
      while (true) {
        val param: AtomicValue = paramIterator.next()
        if (param != null) {
          if (!(param.isInstanceOf[QNameValue])) {
            throw new XPathException(
              "Parameter names in static-params must be supplied as QNames",
              "FOXT0002")
          }
          val paramName: QName = new QName(
            param.asInstanceOf[QNameValue].getStructuredQName)
          val value: GroundedValue = staticParamsMap.get(param)
          if (!allowTypedNodes) {
            checkSequenceIsUntyped(value)
          }
          val paramVal: XdmValue = XdmValue.wrap(value)
          //xsltCompiler.setParameter(paramName, paramVal)
        } else {
          //break
        }
      }
    }

    /*
      private def getStylesheet(options: Map[String, Sequence],
                               /* xsltCompiler: XsltCompiler,*/
                                styleOptionStr: String,
                                context: XPathContext): XsltExecutable = {
        val styleOptionItem: Item = options.get(styleOptionStr).head()
        var stylesheetBaseUri: URI = null
        var seq: Sequence = null
        if ((seq = options.get("stylesheet-base-uri")) != null) {
          val styleBaseUri: StringValue = seq.head().asInstanceOf[StringValue]
          stylesheetBaseUri = URI.create(styleBaseUri.getStringValue)
          if (!stylesheetBaseUri.isAbsolute) {
            val staticBase: URI = getRetainedStaticContext.getStaticBaseUri
            stylesheetBaseUri = staticBase.resolve(styleBaseUri.getStringValue)
          }
        }
        val compileErrors: List[XmlProcessingError] =
          new ArrayList[XmlProcessingError]()
        val originalReporter: ErrorReporter = xsltCompiler.getErrorReporter
        xsltCompiler.setErrorReporter((error) => {
          if (!error.isWarning) {
            compileErrors.add(error)
          }
          originalReporter.report(error)
        })
        var cacheable: Boolean = options.get("static-params") == null
        if (options.get("cache") != null) {
          cacheable &= options
            .get("cache")
            .head()
            .asInstanceOf[BooleanValue]
            .getBooleanValue
        }
        val cache: StylesheetCache = context.getController.getStylesheetCache
        var executable: XsltExecutable = null
        styleOptionStr match {
          case "stylesheet-location" =>
            var stylesheetLocation: String = styleOptionItem.getStringValue
            if (cacheable) {
              executable = cache.getStylesheetByLocation(stylesheetLocation)
            }
            if (executable == null) {
              var style: Source = null
              val base: String = getStaticBaseUriString
              style = xsltCompiler.getURIResolver.resolve(stylesheetLocation, base)
              if (style == null) {
                style =
                  xsltCompiler.getProcessor.getUnderlyingConfiguration.getSystemURIResolver
                    .resolve(stylesheetLocation, base)
              }
              try executable = xsltCompiler.compile(style)
              catch {
                case e: SaxonApiException => reportCompileError(e, compileErrors)

              }
              if (cacheable) {
                cache.setStylesheetByLocation(stylesheetLocation, executable)
              }
            }
          case "stylesheet-node" | "package-node" =>
            var stylesheetNode: NodeInfo = styleOptionItem.asInstanceOf[NodeInfo]
            if (stylesheetBaseUri != null &&
              stylesheetNode.getBaseURI != stylesheetBaseUri.toASCIIString()) {
              val newBaseUri: String = stylesheetBaseUri.toASCIIString()
              val rebased: RebasedDocument = new RebasedDocument(
                stylesheetNode.getTreeInfo,
                (node) => newBaseUri,
                (node) => newBaseUri)
              stylesheetNode = rebased.getRootNode
            }
            if (cacheable) {
              executable = cache.getStylesheetByNode(stylesheetNode)
            }
            if (executable == null) {
              var source: Source = stylesheetNode
              if (stylesheetBaseUri != null) {
                source = AugmentedSource.makeAugmentedSource(source)
                source.setSystemId(stylesheetBaseUri.toASCIIString())
              }
              try executable = xsltCompiler.compile(source)
              catch {
                case e: SaxonApiException => reportCompileError(e, compileErrors)

              }
              if (cacheable) {
                cache.setStylesheetByNode(stylesheetNode, executable)
              }
            }
          case "stylesheet-text" =>
            var stylesheetText: String = styleOptionItem.getStringValue
            if (cacheable) {
              executable = cache.getStylesheetByText(stylesheetText)
            }
            if (executable == null) {
              val sr: StringReader = new StringReader(stylesheetText)
              val style: SAXSource = new SAXSource(new InputSource(sr))
              if (stylesheetBaseUri != null) {
                style.setSystemId(stylesheetBaseUri.toASCIIString())
              }
              try executable = xsltCompiler.compile(style)
              catch {
                case e: SaxonApiException => reportCompileError(e, compileErrors)

              }
              if (cacheable) {
                cache.setStylesheetByText(stylesheetText, executable)
              }
            }
          case "package-name" =>
            var packageName: String =
              Whitespace.trim(styleOptionItem.getStringValue)
            var packageVersion: String = null
            if (options.get("package-version") != null) {
              packageVersion = options.get("package-version").head().getStringValue
            }
            try {
              val pack: XsltPackage =
                xsltCompiler.obtainPackage(packageName, packageVersion)
              if (pack == null) {
                throw new XPathException(
                  "Cannot locate package " + packageName + " version " +
                    packageVersion,
                  "FOXT0002")
              }
              executable = pack.link()
            } catch {
              case e: SaxonApiException =>
                if (e.getCause.isInstanceOf[XPathException]) {
                  throw e.getCause.asInstanceOf[XPathException]
                } else {
                  throw new XPathException(e)
                }

            }

        }
        executable
      }*/

    /*  private def reportCompileError(
                                      e: SaxonApiException,
                                      compileErrors: List[XmlProcessingError]): XsltExecutable = {
        for (te <- compileErrors.asScala) {
          val xe: XPathException = XPathException.fromXmlProcessingError(te)
          xe.maybeSetErrorCode("FOXT0002")
          throw xe
        }
        if (e.getCause.isInstanceOf[XPathException]) {
          throw e.getCause.asInstanceOf[XPathException]
        } else {
          throw new XPathException(e)
        }
      }*/

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val options: Map[String, Sequence] =
        getDetails.optionDetails.processSuppliedOptions(
          arguments(0).head().asInstanceOf[MapItem],
          context)
      val vendorOptionsValue: Sequence = options.get("vendor-options")
      val vendorOptions: MapItem =
        if (vendorOptionsValue == null) null
        else vendorOptionsValue.head().asInstanceOf[MapItem]
      var targetConfig: Configuration = context.getConfiguration
      var allowTypedNodes: Boolean = true
      var schemaValidation: Int = Validation.DEFAULT
      if (vendorOptions != null) {
        var optionValue: Sequence = vendorOptions.get(
          new QNameValue("", NamespaceConstant.SAXON, "configuration"))
        if (optionValue != null) {
          val configFile: NodeInfo = optionValue.head().asInstanceOf[NodeInfo]
          targetConfig =
            Configuration.readConfiguration(configFile, targetConfig)
          allowTypedNodes = false
          if (!context.getConfiguration.getBooleanProperty(
            Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
            targetConfig.setBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS,
              false)
          }
        }
        optionValue = vendorOptions.get(
          new QNameValue("", NamespaceConstant.SAXON, "schema-validation"))
        if (optionValue != null) {
          val valOption: String = optionValue.head().getStringValue
          schemaValidation = Validation.getCode(valOption)
        }
      }
      val processor: Processor = new Processor(true)
      processor.setConfigurationProperty(Feature.CONFIGURATION, targetConfig)
      val isXslt30Processor: Boolean = true
      checkTransformOptions(options, context, isXslt30Processor)
      var useXslt30Processor: Boolean = isXslt30Processor
      if (options.get("xslt-version") != null) {
        val xsltVersion: BigDecimalValue =
          options.get("xslt-version").head().asInstanceOf[BigDecimalValue]
        if ((xsltVersion.compareTo(BigDecimalValue.THREE) >= 0 && !isXslt30Processor) ||
          (xsltVersion.compareTo(BigDecimalValue.THREE) > 0 && isXslt30Processor)) {
          throw new XPathException(
            "The transform option xslt-version is higher than the XSLT version supported by this processor",
            "FOXT0002")
        }
        useXslt30Processor = xsltVersion.compareTo(BigDecimalValue.THREE) == 0
      }
      val principalInput: String = oneOf(options,
        "source-node",
        "source-location",
        "initial-match-selection")
      var invocationOption: String = null
      var invocationName: String = "invocation"
      var styleOption: String = null
      invocationOption = checkInvocationMutualExclusion30(options)
      if (invocationOption != null) {
        invocationName = invocationOption
      }
      if (invocationName.!=("initial-template") && invocationName.!=(
        "initial-function") &&
        principalInput == null) {
        invocationName = "initial-template"
        options.put(
          "initial-template",
          new QNameValue("", NamespaceConstant.XSLT, "initial-template"))
      }
      if (invocationName.==("initial-function") && options.get("function-params") == null) {
        throw new XPathException(
          "Use of the transform option initial-function requires the function parameters to be supplied using the option function-params",
          "FOXT0002")
      }
      if (invocationName.!=("initial-function") && options.get("function-params") != null) {
        throw new XPathException(
          "The transform option function-params can only be used if the option initial-function is also used",
          "FOXT0002")
      }
      styleOption = checkStylesheetMutualExclusion30(options)
      if (options.get("requested-properties") != null) {
        setRequestedProperties(options, processor)
      }
      /*   val xsltCompiler: XsltCompiler = processor.newXsltCompiler()
         xsltCompiler.setURIResolver(context.getURIResolver)
         xsltCompiler.setJustInTimeCompilation(false)
         if (options.get("static-params") != null) {
           setStaticParams(options, xsltCompiler, allowTypedNodes)
         }
         val sheet: XsltExecutable =
           getStylesheet(options, xsltCompiler, styleOption, context)
         val transformer: Xslt30Transformer = sheet.load30()*/
      var deliveryFormat: String = "document"
      var sourceNode: NodeInfo = null
      var sourceLocation: String = null
      var initialMatchSelection: XdmValue = null
      var initialTemplate: QName = null
      var initialMode: QName = null
      var baseOutputUri: String = null
      val stylesheetParams: Map[QName, XdmValue] = new HashMap[QName, XdmValue]()
      var serializationParamsMap: MapItem = null
      val serializedResult: StringWriter = null
      val serializedResultFile: File = null
      var globalContextItem: XdmItem = null
      val templateParams: Map[QName, XdmValue] = new HashMap[QName, XdmValue]()
      val tunnelParams: Map[QName, XdmValue] = new HashMap[QName, XdmValue]()
      var initialFunction: QName = null
      var functionParams: Array[XdmValue] = null
      var postProcessor: Function = null
      var principalResultKey: String = "output"
      for (name <- options.keySet.asScala) {
        val value: Sequence = options.get(name)
        val head: Item = value.head()
        name match {
          case "source-node" =>
            sourceNode = head.asInstanceOf[NodeInfo]
          /*  if (!allowTypedNodes) {
              checkSequenceIsUntyped(sourceNode)
            }*/
          case "source-location" => sourceLocation = head.getStringValue
          case "initial-template" =>
            initialTemplate = new QName(
              head.asInstanceOf[QNameValue].getStructuredQName)
          case "initial-mode" =>
            initialMode = new QName(
              head.asInstanceOf[QNameValue].getStructuredQName)
          case "initial-match-selection" =>
            initialMatchSelection = XdmValue.wrap(value)
          /* if (!allowTypedNodes) {
             checkSequenceIsUntyped(value)
           }*/
          case "delivery-format" =>
            deliveryFormat = head.getStringValue
            if (deliveryFormat.!=("document") && deliveryFormat.!=("serialized") &&
              deliveryFormat.!=("raw")) {
              throw new XPathException(
                "The transform option delivery-format should be one of: document|serialized|raw ",
                "FOXT0002")
            }
          case "base-output-uri" =>
            baseOutputUri = head.getStringValue
            principalResultKey = baseOutputUri
          case "serialization-params" =>
            serializationParamsMap = head.asInstanceOf[MapItem]
          case "stylesheet-params" => {
            val params: MapItem = head.asInstanceOf[MapItem]
            //processParams(params, stylesheetParams, allowTypedNodes)
            //break
          }
          case "global-context-item" =>
            if (useXslt30Processor) {
              if (!allowTypedNodes && head.isInstanceOf[NodeInfo] && head
                .asInstanceOf[NodeInfo]
                .getTreeInfo
                .isTyped) {
                throw new XPathException(
                  "Schema-validated nodes cannot be passed to fn:transform() when it runs under a different Saxon Configuration",
                  "FOXT0002")
              }
              globalContextItem = XdmValue.wrap(head).asInstanceOf[XdmItem]
            }
          case "template-params" => {
            val params: MapItem = head.asInstanceOf[MapItem]
            //processParams(params, templateParams, allowTypedNodes)
            //break
          }
          case "tunnel-params" => {
            val params: MapItem = head.asInstanceOf[MapItem]
            //processParams(params, tunnelParams, allowTypedNodes)
            //break
          }
          case "initial-function" =>
            initialFunction = new QName(
              head.asInstanceOf[QNameValue].getStructuredQName)
          case "function-params" =>
            var functionParamsArray: ArrayItem = head.asInstanceOf[ArrayItem]
            functionParams =
              Array.ofDim[XdmValue](functionParamsArray.arrayLength())
            for (i <- 0 until functionParams.length) {
              val argVal: Sequence = functionParamsArray.get(i)
              /* if (!allowTypedNodes) {
                 checkSequenceIsUntyped(argVal)
               }*/
              functionParams(i) = XdmValue.wrap(argVal)
            }
          case "post-process" => postProcessor = head.asInstanceOf[Function]

        }
      }
      if (baseOutputUri == null) {
        baseOutputUri = getStaticBaseUriString
      } else {
        var base: URI = new URI(baseOutputUri)
        if (!base.isAbsolute) {
          base = getRetainedStaticContext.getStaticBaseUri.resolve(baseOutputUri)
          baseOutputUri = base.toASCIIString()
        }
      }
      val deliverer: Deliverer = Deliverer.makeDeliverer(deliveryFormat)
      //deliverer.setTransformer(transformer)
      deliverer.setBaseOutputUri(baseOutputUri)
      deliverer.setPrincipalResultKey(principalResultKey)
      deliverer.setPostProcessor(postProcessor, context)
      /*val controller: XsltController = transformer.getUnderlyingController*/
      //controller.setResultDocumentResolver(deliverer)
      val destination: Destination =
      deliverer.getPrimaryDestination(serializationParamsMap)
      var result: Sequence = null
      try {
        /*transformer.setStylesheetParameters(stylesheetParams)
        transformer.setBaseOutputURI(baseOutputUri)
        transformer.setInitialTemplateParameters(templateParams, false)
        transformer.setInitialTemplateParameters(tunnelParams, true)*/
        if (schemaValidation == Validation.STRICT || schemaValidation == Validation.LAX) {
          if (sourceNode != null) {
            sourceNode = validate(sourceNode, targetConfig, schemaValidation)
          } else if (sourceLocation != null) {
            val base: String = getStaticBaseUriString
            /*  var ss: Source =
              if (ss == null) {
                ss = targetConfig.getURIResolver.resolve(sourceLocation, base)
                if (ss == null) {
                  throw new XPathException(
                    "Cannot locate document at sourceLocation " + sourceLocation,
                    "FOXT0003")
                }
              }*/
            val parseOptions: ParseOptions = new ParseOptions(
              targetConfig.getParseOptions)
            parseOptions.setSchemaValidationMode(schemaValidation)
            val tree: TreeInfo = targetConfig.buildDocumentTree(null, parseOptions)
            sourceNode = tree.getRootNode
            sourceLocation = null
          }
          if (globalContextItem.isInstanceOf[XdmNode]) {
            val v: NodeInfo = validate(
              globalContextItem.asInstanceOf[XdmNode].getUnderlyingNode,
              targetConfig,
              schemaValidation)
            globalContextItem = XdmValue.wrap(v).asInstanceOf[XdmNode]
          }
        }
        /*  if (sourceNode != null && globalContextItem == null) {
            transformer.setGlobalContextItem(new XdmNode(sourceNode.getRoot))
          }
          if (globalContextItem != null) {
            transformer.setGlobalContextItem(globalContextItem)
          }*/
        /*    if (initialTemplate != null) {
              transformer.callTemplate(initialTemplate, destination)
              result = deliverer.getPrimaryResult
            } else if (initialFunction != null) {
              transformer.callFunction(initialFunction, functionParams, destination)
              result = deliverer.getPrimaryResult
            } else {
              if (initialMode != null) {
                transformer.setInitialMode(initialMode)
              }*/
        if (initialMatchSelection == null && sourceNode != null) {
          initialMatchSelection = XdmValue.wrap(sourceNode)
        }
        if (initialMatchSelection == null && sourceLocation != null) {
          val stream: StreamSource = new StreamSource(sourceLocation)
          /*    if (transformer.getUnderlyingController.getInitialMode.isDeclaredStreamable) {
                transformer.applyTemplates(stream, destination)
              } else {
                transformer.transform(stream, destination)
              }*/
          result = deliverer.getPrimaryResult
        } else {
          /*transformer.applyTemplates(initialMatchSelection, destination)*/
          result = deliverer.getPrimaryResult
        }
      }
      catch {
        case e: SaxonApiException => {
          var e2: XPathException = null
          if (e.getCause.isInstanceOf[XPathException]) {
            e2 = e.getCause.asInstanceOf[XPathException]
            e2.setIsGlobalError(false)
            throw e2
          } else {
            throw new XPathException(e)
          }
        }

      }
      var resultMap: HashTrieMap = new HashTrieMap()
      resultMap = deliverer.populateResultMap(resultMap)
      if (result != null) {
        val resultKey: AtomicValue = new StringValue(principalResultKey)
        resultMap = resultMap.addEntry(resultKey, result.materialize())
      }
      resultMap
    }

    private def processParams(suppliedParams: MapItem,
                              checkedParams: Map[QName, XdmValue],
                              allowTypedNodes: Boolean): Unit = {
      val paramIterator: AtomicIterator[_ <: AtomicValue] = suppliedParams.keys
      while (true) {
        val param: AtomicValue = paramIterator.next()
        if (param != null) {
          if (!(param.isInstanceOf[QNameValue])) {
            throw new XPathException(
              "The names of parameters must be supplied as QNames",
              "FOXT0002")
          }
          val paramName: QName = new QName(
            param.asInstanceOf[QNameValue].getStructuredQName)
          val value: Sequence = suppliedParams.get(param)
          if (!allowTypedNodes) {
            checkSequenceIsUntyped(value)
          }
          val paramVal: XdmValue = XdmValue.wrap(value)
          checkedParams.put(paramName, paramVal)
        } else {
          //break
        }
      }
    }

    private def checkSequenceIsUntyped(value: Sequence): Unit = {
      val iter: SequenceIterator = value.iterate()
      var item: Item = null
      while ((item = iter.next()) != null) if (item
        .isInstanceOf[NodeInfo] && item
        .asInstanceOf[NodeInfo]
        .getTreeInfo
        .isTyped) {
        throw new XPathException(
          "Schema-validated nodes cannot be passed to fn:transform() when it runs under a different Saxon Configuration",
          "FOXT0002")
      }
    }

  }
