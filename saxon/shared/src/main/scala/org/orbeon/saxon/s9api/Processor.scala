package org.orbeon.saxon.s9api

import java.io.{File, OutputStream, Writer}
import java.text.RuleBasedCollator
import java.util.{Comparator, Objects}

import org.orbeon.saxon.event._
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.expr.sort.{RuleBasedSubstringMatcher, SimpleCollation}
import org.orbeon.saxon.lib._
import org.orbeon.saxon.om.{GroundedValue, Sequence, StructuredQName}
import org.orbeon.saxon.s9api.Processor._
import org.orbeon.saxon.serialize.SerializationProperties
import org.orbeon.saxon.utils.{Configuration, Version}

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object Processor {

  private class ExtensionFunctionDefinitionWrapper(private var function: ExtensionFunction)
    extends ExtensionFunctionDefinition {

    override def getFunctionQName(): StructuredQName =
      function.getName.getStructuredQName

    override def getMinimumNumberOfArguments(): Int =
      function.getArgumentTypes.length

    override def getMaximumNumberOfArguments(): Int =
      function.getArgumentTypes.length

    override def getArgumentTypes: Array[org.orbeon.saxon.value.SequenceType] = {
      val declaredArgs: Array[org.orbeon.saxon.s9api.SequenceType] =
        function.getArgumentTypes
      val types: Array[org.orbeon.saxon.value.SequenceType] =
        Array.ofDim[org.orbeon.saxon.value.SequenceType](declaredArgs.length)
      for (i <- 0 until declaredArgs.length) {
        types(i) = org.orbeon.saxon.value.SequenceType.makeSequenceType(
          declaredArgs(i).getItemType.getUnderlyingItemType,
          declaredArgs(i).getOccurrenceIndicator.getCardinality)
      }
      types
    }

    override def getResultType(suppliedArgumentTypes: Array[org.orbeon.saxon.value.SequenceType]): org.orbeon.saxon.value.SequenceType = {
      val declaredResult: org.orbeon.saxon.s9api.SequenceType = function.getResultType
      org.orbeon.saxon.value.SequenceType.makeSequenceType(
        declaredResult.getItemType.getUnderlyingItemType,
        declaredResult.getOccurrenceIndicator.getCardinality)
    }

    override def trustResultType(): Boolean = false

    override def dependsOnFocus(): Boolean = false

    override def hasSideEffects(): Boolean = false

    override def makeCallExpression(): ExtensionFunctionCall =
      new ExtensionFunctionCall() {
        override def call(context: XPathContext,
                          arguments: Array[Sequence]): Sequence = {
          val args: Array[XdmValue] = Array.ofDim[XdmValue](arguments.length)
          for (i <- 0 until args.length) {
            val `val`: GroundedValue =
              arguments(i).materialize()
            args(i) = XdmValue.wrap(`val`)
          }
          val result: XdmValue = function.call(args)
          result.getUnderlyingValue
        }
      }

  }

}

class Processor extends Configuration.ApiProvider {

  private var config: Configuration = _

  private var licensedEdition: Boolean = false

  @BeanProperty
  var schemaManager: SchemaManager = _

  if (licensedEdition) {
    config = Configuration.newConfiguration
    if (config.getEditionCode.==("EE")) {
      schemaManager = makeSchemaManager()
    }
  } else {
    config = new Configuration()
  }

  config.setProcessor(this)

  def this(licEdition: Boolean) {
    this()
    this.licensedEdition = licEdition
  }

  def this(config: Configuration) = {
    this()
    this.config = config
    if (config.getEditionCode.==("EE")) {
      schemaManager = makeSchemaManager()
    }
  }

  //ORBEON: Remove as `Configuration.readConfiguration` is not available
//  def this(source: Source) = {
//    this()
//    config = Configuration.readConfiguration(source)
//    schemaManager = makeSchemaManager()
//    config.setProcessor(this)
//  }

  def newDocumentBuilder(): DocumentBuilder = new DocumentBuilder(config)

  def newXPathCompiler(): XPathCompiler = new XPathCompiler(this)

  //def newXsltCompiler(): XsltCompiler = new XsltCompiler(this) //XsltCompiler not found

  def newXQueryCompiler(): XQueryCompiler = new XQueryCompiler(this)

  def newSerializer(): Serializer = new Serializer(this)

  def newSerializer(stream: OutputStream): Serializer = {
    val s: Serializer = new Serializer(this)
    s.setOutputStream(stream)
    s
  }

  def newSerializer(writer: Writer): Serializer = {
    val s: Serializer = new Serializer(this)
    s.setOutputWriter(writer)
    s
  }

  // ORBEON: No `File` support.
//  def newSerializer(file: File): Serializer = {
//    val s: Serializer = new Serializer(this)
//    s.setOutputFile(file)
//    s
//  }

  def newPush(destination: Destination): Push = {
    val pipe: PipelineConfiguration =
      getUnderlyingConfiguration.makePipelineConfiguration
    val props: SerializationProperties = new SerializationProperties()
    new PushToReceiver(destination.getReceiver(pipe, props))
  }

  def registerExtensionFunction(function: ExtensionFunction): Unit = {
    val wrapper: ExtensionFunctionDefinitionWrapper =
      new ExtensionFunctionDefinitionWrapper(function)
    registerExtensionFunction(wrapper)
  }

  def registerExtensionFunction(function: ExtensionFunctionDefinition): Unit = {
    config.registerExtensionFunction(function)
  }

  def isSchemaAware: Boolean =
    config.isLicensedFeature(Configuration.LicenseFeature.SCHEMA_VALIDATION)

  def getSaxonProductVersion: String = Version.getProductVersion

  def getSaxonEdition: String = config.getEditionCode

  def setXmlVersion(version: String): Unit = {
    version match {
      case "1.0" => config.setXMLVersion(Configuration.XML10)
      case "1.1" => config.setXMLVersion(Configuration.XML11)
      case _ => throw new IllegalArgumentException("XmlVersion")

    }
  }

  def getXmlVersion: String =
    if (config.getXMLVersion == Configuration.XML10) {
      "1.0"
    } else {
      "1.1"
    }

  def setConfigurationProperty(name: String, value: AnyRef): Unit = {
    if (name == FeatureKeys.CONFIGURATION) {
      config = value.asInstanceOf[Configuration]
    } else {
      config.setConfigurationProperty(name, value)
    }
  }

  def getConfigurationProperty(name: String): AnyRef =
    config.getConfigurationProperty(name).asInstanceOf[AnyRef]

  def setConfigurationProperty[T](feature: Feature[T], value: T): Unit = {
    if (feature == Feature.CONFIGURATION) {
      config = value.asInstanceOf[Configuration]
    } else {
      config.setConfigurationProperty(feature, value)
    }
  }

  def getConfigurationProperty[T](feature: Feature[T]): T =
    config.getConfigurationProperty(feature)

  def declareCollation(uri: String, collation: Comparator[CharSequence]): Unit = {
    if (uri == NamespaceConstant.CODEPOINT_COLLATION_URI) {
      throw new IllegalArgumentException(
        "Cannot redeclare the Unicode codepoint collation URI")
    }
    if (uri == NamespaceConstant.HTML5_CASE_BLIND_COLLATION_URI) {
      throw new IllegalArgumentException(
        "Cannot redeclare the HTML5 caseblind collation URI")
    }
    var saxonCollation: StringCollator = null
    saxonCollation =
      if (collation.isInstanceOf[RuleBasedCollator])
        new RuleBasedSubstringMatcher(
          uri,
          collation.asInstanceOf[RuleBasedCollator])
      else new SimpleCollation(uri, collation)
    config.registerCollation(uri, saxonCollation)
  }

  def getUnderlyingConfiguration: Configuration = config

  def writeXdmValue(value: XdmValue, destination: Destination): Unit = {
    Objects.requireNonNull(value)
    Objects.requireNonNull(destination)
    if (destination.isInstanceOf[Serializer]) {
      destination.asInstanceOf[Serializer].serializeXdmValue(value)
    } else {
      val out: Receiver = destination.getReceiver(
        config.makePipelineConfiguration,
        config.obtainDefaultSerializationProperties)
      val tree: ComplexContentOutputter = new ComplexContentOutputter(out)
      tree.open()
      tree.startDocument(ReceiverOption.NONE)
      for (item <- value.asScala) {
        tree.append(item.getUnderlyingValue,
          Loc.NONE,
          ReceiverOption.ALL_NAMESPACES)
      }
      tree.endDocument()
      tree.close()
      destination.closeAndNotify()
    }
  }

  private def makeSchemaManager(): SchemaManager = {
    val manager: SchemaManager = null
    manager
  }
}
