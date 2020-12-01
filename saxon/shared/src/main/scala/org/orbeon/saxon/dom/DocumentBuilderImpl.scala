//package org.orbeon.saxon.dom
//
//import java.io.File
//import java.util.Objects
//
//import javax.xml.parsers.DocumentBuilder
//import javax.xml.transform.sax.SAXSource
//import org.orbeon.saxon.event.Sender
//import org.orbeon.saxon.lib.{ParseOptions, Validation}
//import org.orbeon.saxon.om.{AllElementsSpaceStrippingRule, IgnorableSpaceStrippingRule, NoElementsSpaceStrippingRule, SpaceStrippingRule}
//import org.orbeon.saxon.tree.tiny.{TinyBuilder, TinyDocumentImpl}
//import org.orbeon.saxon.utils.Configuration
//import org.orbeon.saxon.value.Whitespace
//import org.w3c.dom.{DOMImplementation, Document}
//import org.xml.sax.{EntityResolver, ErrorHandler, InputSource}
//
//import scala.beans.BeanProperty
//
//class DocumentBuilderImpl extends DocumentBuilder {
//
//  private var config: Configuration = _
//
//  @BeanProperty
//  var parseOptions: ParseOptions = new ParseOptions()
//
//  def setConfiguration(config: Configuration): Unit = {
//    this.config = config
//  }
//
//  def getConfiguration: Configuration = {
//    if (config == null) {
//      config = new Configuration()
//    }
//    config
//  }
//
//  def isNamespaceAware(): Boolean = true
//
//  def setValidating(state: Boolean): Unit = {
//    parseOptions.setDTDValidationMode(
//      if (state) Validation.STRICT else Validation.SKIP)
//  }
//
//  def isValidating(): Boolean =
//    parseOptions.getDTDValidationMode == Validation.STRICT
//
//  def newDocument(): Document =
//    throw new UnsupportedOperationException(
//      "The only way to build a document using this DocumentBuilder is with the parse() method")
//
//  def parse(in: InputSource): Document = {
//    if (config == null) {
//      config = new Configuration()
//    }
//    val builder: TinyBuilder = new TinyBuilder(
//      config.makePipelineConfiguration)
//    builder.setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
//    val source: SAXSource = new SAXSource(in)
//    source.setSystemId(in.getSystemId)
//    Sender.send(source, builder, parseOptions)
//    val doc: TinyDocumentImpl =
//      builder.getCurrentRoot.asInstanceOf[TinyDocumentImpl]
//    builder.reset()
//    NodeOverNodeInfo.wrap(doc).asInstanceOf[Document]
//  }
//
//  override def parse(f: File): Document = {
//    Objects.requireNonNull(f)
//    val uri: String = f.toURI().toString
//    val in: InputSource = new InputSource(uri)
//    parse(in)
//  }
//
//  def setEntityResolver(er: EntityResolver): Unit = {
//    parseOptions.setEntityResolver(er)
//  }
//
//  def setErrorHandler(eh: ErrorHandler): Unit = {
//    parseOptions.setErrorHandler(eh)
//  }
//
//  def getDOMImplementation(): DOMImplementation =
//    newDocument().getImplementation
//
//  def setXIncludeAware(state: Boolean): Unit = {
//    parseOptions.setXIncludeAware(state)
//  }
//
//  override def isXIncludeAware(): Boolean = parseOptions.isXIncludeAware
//
//  def setStripSpace(stripAction: Int): Unit = {
//    stripAction match {
//      case Whitespace.ALL =>
//        parseOptions.setSpaceStrippingRule(
//          AllElementsSpaceStrippingRule.getInstance)
//      case Whitespace.NONE =>
//        parseOptions.setSpaceStrippingRule(
//          NoElementsSpaceStrippingRule.getInstance)
//      case Whitespace.IGNORABLE =>
//        parseOptions.setSpaceStrippingRule(
//          IgnorableSpaceStrippingRule.getInstance)
//      case Whitespace.UNSPECIFIED => parseOptions.setSpaceStrippingRule(null)
//      case _ => throw new IllegalArgumentException
//
//    }
//  }
//
//  def getStripSpace: Int = {
//    val rule: SpaceStrippingRule = parseOptions.getSpaceStrippingRule
//    if (rule == AllElementsSpaceStrippingRule.getInstance) {
//      Whitespace.ALL
//    } else if (rule == NoElementsSpaceStrippingRule.getInstance) {
//      Whitespace.NONE
//    } else if (rule == IgnorableSpaceStrippingRule.getInstance) {
//      Whitespace.IGNORABLE
//    } else if (rule == null) {
//      Whitespace.IGNORABLE
//    } else {
//      Whitespace.XSLT
//    }
//  }
//
//}
