package org.orbeon.saxon.s9api

import org.orbeon.saxon.event._
import org.orbeon.saxon.expr.EarlyEvaluationContext
import org.orbeon.saxon.expr.JPConverter
import org.orbeon.saxon.lib.ParseOptions
import org.orbeon.saxon.lib.Validation
import org.orbeon.saxon.om._
import org.orbeon.saxon.query.XQueryExpression
import org.orbeon.saxon.value.Whitespace
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import java.io.File
import java.net.URI

import DocumentBuilder._
import org.orbeon.saxon.utils.Configuration

import scala.beans.{BeanProperty, BooleanBeanProperty}

object DocumentBuilder {

  private class BuildingContentHandlerImpl(r: Receiver, b: Builder)
    extends ReceivingContentHandler
      with BuildingContentHandler {

    private var builder: Builder = b

    this.receiver = r

    this.setPipelineConfiguration(r.getPipelineConfiguration)

    def getDocumentNode(): XdmNode = new XdmNode(builder.getCurrentRoot)

  }

}

class DocumentBuilder (var config: Configuration) {

  @BeanProperty
  var schemaValidator: SchemaValidator = _

  private var dtdValidation: Boolean = _

  @BooleanBeanProperty
  var lineNumbering: Boolean = _

  @BeanProperty
  var treeModel: TreeModel = TreeModel.TINY_TREE

  private var whitespacePolicy: WhitespaceStrippingPolicy =
    WhitespaceStrippingPolicy.UNSPECIFIED

  @BeanProperty
  var base_URI: URI = _

  private var projectionQuery: XQueryExecutable = _

  def setDTDValidation(option: Boolean): Unit = {
    dtdValidation = option
  }

  def isDTDValidation: Boolean = dtdValidation

  def setWhitespaceStrippingPolicy(policy: WhitespaceStrippingPolicy): Unit = {
    whitespacePolicy = policy
  }

  def getWhitespaceStrippingPolicy: WhitespaceStrippingPolicy =
    whitespacePolicy

  def setBaseURI(uri: URI): Unit = {
    if (!uri.isAbsolute) {
      throw new IllegalArgumentException("Supplied base URI must be absolute")
    }
    base_URI = uri
  }

  def setDocumentProjectionQuery(query: XQueryExecutable): Unit = {
    this.projectionQuery = query
  }

  def getDocumentProjectionQuery: XQueryExecutable = this.projectionQuery

  def build(source: Source): XdmNode = {
    if (source == null) {
      throw new NullPointerException("source")
    }
    if (!(whitespacePolicy == WhitespaceStrippingPolicy.UNSPECIFIED ||
      whitespacePolicy == WhitespaceStrippingPolicy.IGNORABLE ||
      whitespacePolicy.ordinal() == Whitespace.XSLT)) {
      if (dtdValidation) {
        throw new SaxonApiException(
          "When DTD validation is used, the whitespace stripping policy must be IGNORABLE")
      }
      if (schemaValidator != null) {
        throw new SaxonApiException(
          "When schema validation is used, the whitespace stripping policy must be IGNORABLE")
      }
    }
    val options: ParseOptions = new ParseOptions(config.getParseOptions)
    options.setDTDValidationMode(
      if (dtdValidation) Validation.STRICT else Validation.STRIP)
    if (schemaValidator != null) {
      options.setSchemaValidationMode(
        if (schemaValidator.isLax) Validation.LAX else Validation.STRICT)
      if (schemaValidator.getDocumentElementName != null) {
        val qn: QName = schemaValidator.getDocumentElementName
        options.setTopLevelElement(
          new StructuredQName(qn.getPrefix,
            qn.getNamespaceURI,
            qn.getLocalName))
      }
      if (schemaValidator.getDocumentElementType != null) {
        options.setTopLevelType(schemaValidator.getDocumentElementType)
      }
    }
    if (treeModel != null) {
      options.setModel(treeModel)
    }
    if (whitespacePolicy != null &&
      whitespacePolicy != WhitespaceStrippingPolicy.UNSPECIFIED) {
      val option: Int = whitespacePolicy.ordinal()
      if (option == Whitespace.XSLT) {
        options.setSpaceStrippingRule(NoElementsSpaceStrippingRule.getInstance)
        options.addFilter(whitespacePolicy.makeStripper())
      } else {
        options.setSpaceStrippingRule(whitespacePolicy.getSpaceStrippingRule)
      }
    }
    options.setLineNumbering(lineNumbering)
    if (source.getSystemId == null && base_URI != null) {
      source.setSystemId(base_URI.toString)
    }
    if (projectionQuery != null) {
      val exp: XQueryExpression = projectionQuery.getUnderlyingCompiledQuery
      val ff: FilterFactory = config.makeDocumentProjector(exp)
      if (ff != null) {
        options.addFilter(ff)
      }
    }
    val doc: TreeInfo = config.buildDocumentTree(source, options)
    new XdmNode(doc.getRootNode)
  }

  // ORBEON: No `File` support.
//  def build(file: File): XdmNode = build(new StreamSource(file))

  def newBuildingContentHandler(): BuildingContentHandler = {
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    val builder: Builder = treeModel.makeBuilder(pipe)
    if (base_URI != null) {
      builder.setSystemId(base_URI.toASCIIString())
    }
    builder.setLineNumbering(lineNumbering)
    var r: Receiver = builder
    r = new NamespaceReducer(r)
    r = injectValidator(r, builder)
    new BuildingContentHandlerImpl(r, builder)
  }

  private def injectValidator(r: Receiver, builder: Builder): Receiver = {
    if (schemaValidator != null) {
      val pipe: PipelineConfiguration = builder.getPipelineConfiguration
      val `val`: Receiver = schemaValidator.getReceiver(
        pipe,
        config.obtainDefaultSerializationProperties)
      `val`.setPipelineConfiguration(pipe)
      if (`val`.isInstanceOf[ProxyReceiver]) {
        `val`.asInstanceOf[ProxyReceiver].setUnderlyingReceiver(r)
      }
      return `val`
    }
    r
  }

  def newBuildingStreamWriter(): BuildingStreamWriterImpl = {
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    val builder: Builder = treeModel.makeBuilder(pipe)
    builder.setLineNumbering(lineNumbering)
    var r: Receiver = builder
    r = new NamespaceReducer(r)
    r = injectValidator(r, builder)
    new BuildingStreamWriterImpl(r, builder)
  }

  def wrap(node: AnyRef): XdmNode =
    if (node.isInstanceOf[NodeInfo]) {
      val nodeInfo: NodeInfo = node.asInstanceOf[NodeInfo]
      if (nodeInfo.getConfiguration.isCompatible(config)) {
        new XdmNode(nodeInfo)
      } else {
        throw new IllegalArgumentException(
          "Supplied NodeInfo was created using a different Configuration")
      }
    } else {
      val converter: JPConverter =
        JPConverter.allocate(node.getClass, null, config)
      val nodeInfo: NodeInfo = converter
        .convert(node, new EarlyEvaluationContext(config))
        .asInstanceOf[NodeInfo]
      XdmItem.wrapItem(nodeInfo)
    }

}
