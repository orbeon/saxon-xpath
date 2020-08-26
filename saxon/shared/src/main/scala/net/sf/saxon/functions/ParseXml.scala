package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Builder

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.Sender

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.PackageData

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.om._

import net.sf.saxon.style.StylesheetPackage

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.TinyDocumentImpl

import net.sf.saxon.value.ObjectValue

import net.sf.saxon.value.SequenceExtent

import net.sf.saxon.value.StringValue

import org.xml.sax.ErrorHandler

import org.xml.sax.InputSource

import org.xml.sax.SAXParseException

import javax.xml.transform.Source

import javax.xml.transform.sax.SAXSource

import java.io.StringReader

import java.util.ArrayList

import java.util.List

import ParseXml._

import scala.jdk.CollectionConverters._

object ParseXml {

  class RetentiveErrorHandler extends ErrorHandler {

    var errors: List[SAXParseException] = new ArrayList()

    var failed: Boolean = false

    def error(exception: SAXParseException): Unit = {
      errors.add(exception)
    }

    def warning(exception: SAXParseException): Unit = ()

    def fatalError(exception: SAXParseException): Unit = {
      errors.add(exception)
      failed = true
    }

    def captureRetainedErrors(xe: XPathException): Unit = {
      val retainedErrors: List[SAXParseException] = errors
      if (!retainedErrors.isEmpty) {
        val wrappedErrors: List[ObjectValue[SAXParseException]] =
          new ArrayList[ObjectValue[SAXParseException]]()
        for (e <- retainedErrors.asScala) {
          wrappedErrors.add(new ObjectValue(e))
        }
        xe.setErrorObject(SequenceExtent.makeSequenceExtent(wrappedErrors))
      }
    }

  }

}

class ParseXml extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val input: StringValue = arguments(0).head().asInstanceOf[StringValue]
    if (input == null) ZeroOrOne.empty()
    else new ZeroOrOne(evalParseXml(input, context))
  }

  private def evalParseXml(inputArg: StringValue,
                           context: XPathContext): NodeInfo = {
    val baseURI: String = getRetainedStaticContext.getStaticBaseUriString
    val errorHandler: RetentiveErrorHandler = new RetentiveErrorHandler()
    try {
      val controller: Controller = context.getController
      if (controller == null) {
        throw new XPathException(
          "parse-xml() function is not available in this environment")
      }
      val config: Configuration = controller.getConfiguration
      val sr: StringReader = new StringReader(inputArg.getStringValue)
      val is: InputSource = new InputSource(sr)
      is.setSystemId(baseURI)
      val source: Source = new SAXSource(is)
      source.setSystemId(baseURI)
      val b: Builder =
        TreeModel.TINY_TREE.makeBuilder(controller.makePipelineConfiguration)
      var s: Receiver = b
      val options: ParseOptions = new ParseOptions(config.getParseOptions)
      val pd: PackageData = getRetainedStaticContext.getPackageData
      if (pd.isInstanceOf[StylesheetPackage]) {
        options.setSpaceStrippingRule(
          pd.asInstanceOf[StylesheetPackage].getSpaceStrippingRule)
        if (pd.asInstanceOf[StylesheetPackage].isStripsTypeAnnotations) {
          s = config.getAnnotationStripper(s)
        }
      } else {
        options.setSpaceStrippingRule(IgnorableSpaceStrippingRule.getInstance)
      }
      options.setErrorHandler(errorHandler)
      s.setPipelineConfiguration(b.getPipelineConfiguration)
      Sender.send(source, s, options)
      val node: TinyDocumentImpl =
        b.getCurrentRoot.asInstanceOf[TinyDocumentImpl]
      node.setBaseURI(baseURI)
      node.getTreeInfo.setUserData("saxon:document-uri", "")
      b.reset()
      node
    } catch {
      case err: XPathException => {
        val xe: XPathException = new XPathException(
          "First argument to parse-xml() is not a well-formed and namespace-well-formed XML document. XML parser reported: " +
            err.getMessage,
          "FODC0006")
        errorHandler.captureRetainedErrors(xe)
        xe.maybeSetContext(context)
        throw xe
      }

    }
  }

}
