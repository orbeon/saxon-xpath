
package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Version
import org.orbeon.saxon.event.Builder
import org.orbeon.saxon.event.ProxyReceiver
import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.event.Sender
import org.orbeon.saxon.expr.Callable

import scala.util.control.Breaks._
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.ParseOptions
import org.orbeon.saxon.lib.Validation
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.tiny.TinyBuilder
import org.orbeon.saxon.value.StringValue
import org.xml.sax.InputSource
import org.xml.sax.XMLReader
import javax.xml.transform.sax.SAXSource
import java.io.StringReader
import java.util

import org.orbeon.saxon.functions.ParseXmlFragment.OuterElementStripper

object ParseXmlFragment {

  /**
   * Filter to remove the element wrapper added to the document to satisfy the XML parser
   */
  private class OuterElementStripper(val next: Receiver) extends ProxyReceiver(next) {
    private var level = 0
    private val suppressStartContent = false

    /**
     * Notify the start of an element
     */
    @throws[XPathException]
    override def startElement(elemName: NodeName, `type`: SchemaType, attributes: AttributeMap, namespaces: NamespaceMap, location: Location, properties: Int): Unit = {
      level += 1
      if (level > 0) super.startElement(elemName, `type`, attributes, namespaces, location, properties)
    }

    /**
     * End of element
     */
    @throws[XPathException]
    override def endElement() = {
      level -= 1
      if (level > 0) super.endElement()
    }
  }

}

class ParseXmlFragment extends SystemFunction with Callable {

  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val input = arguments(0).head.asInstanceOf[StringValue]
    if (input == null)
      ZeroOrOne.empty
    else
      new ZeroOrOne[NodeInfo](evalParseXml(input, context))
  }

  @throws[XPathException]
  private def evalParseXml(inputArg: StringValue, context: XPathContext): NodeInfo = {
    var node: NodeInfo = null
    val baseURI = getStaticBaseUriString
    val errorHandler = new ParseXml.RetentiveErrorHandler
    var attempt = 0
    while (attempt < 3) try {
      attempt += 1
      val controller = context.getController
      if (controller == null) throw new XPathException("parse-xml-fragment() function is not available in this environment")
      val configuration = controller.getConfiguration
      val fragmentReader = new StringReader(inputArg.getStringValue)
      val skeleton = "<!DOCTYPE z [<!ENTITY e SYSTEM \"http://www.saxonica.com/parse-xml-fragment/actual.xml\">]>\n<z>&e;</z>"
      val skeletonReader = new StringReader(skeleton)
      val is = new InputSource(skeletonReader)
      is.setSystemId(baseURI)
      val source = new SAXSource(is)
      var reader: XMLReader = null
      if (attempt == 1) {
        reader = configuration.getSourceParser
        breakable {
          if (reader.getEntityResolver != null) break()
          else reader = Version.platform.loadParserForXmlFragments
        }
        source.setXMLReader(reader)
        source.setSystemId(baseURI)
        val b = controller.makeBuilder
        if (b.isInstanceOf[TinyBuilder]) b.asInstanceOf[TinyBuilder].setStatistics(controller.getConfiguration.getTreeStatistics.FN_PARSE_STATISTICS)
        var s = b
        val options = new ParseOptions
        options.setSchemaValidationMode(Validation.SKIP)
        options.setDTDValidationMode(Validation.SKIP)
        val safetyCheck = new util.ArrayList[Boolean]
        reader.setEntityResolver((publicId: String, systemId: String) => {
          def foo(publicId: String, systemId: String) =
            if ("http://www.saxonica.com/parse-xml-fragment/actual.xml" == systemId) {
              safetyCheck.add(true)
              val is1 = new InputSource(fragmentReader)
              is1.setSystemId(baseURI)
              is1
            }
            else null

          foo(publicId, systemId)
        })
        val pd = getRetainedStaticContext.getPackageData
        if (pd.isInstanceOf[StylesheetPackage]) {
          options.setSpaceStrippingRule(pd.asInstanceOf[StylesheetPackage].getSpaceStrippingRule)
          if (pd.asInstanceOf[StylesheetPackage].isStripsTypeAnnotations) s = configuration.getAnnotationStripper(s).asInstanceOf[Builder]
        }
        else options.setSpaceStrippingRule(IgnorableSpaceStrippingRule.getInstance)
        options.setErrorHandler(errorHandler)
        s.setPipelineConfiguration(b.getPipelineConfiguration)
        options.addFilter(res => new OuterElementStripper(res))
        try Sender.send(source, s, options)
        catch {
          case e: XPathException =>
            // this might be because the EntityResolver wasn't called - see bug 4127

            if (safetyCheck.isEmpty) { // This means our entity resolver wasn't called. Make one more try, using the
              // built-in platform default parser; then give up.
              breakable {
                if (attempt == 2) {
                  val xe = new XPathException("The configured XML parser cannot be used by fn:parse-xml-fragment(), because it ignores the supplied EntityResolver", "FODC0006")
                  errorHandler.captureRetainedErrors(xe)
                  xe.maybeSetContext(context)
                  throw xe
                }
                else break()
              }
            }
            else throw e
        }
        node = b.getCurrentRoot
        b.reset()
      }
    }
    catch {
      case err: XPathException =>
        val xe = new XPathException("First argument to parse-xml-fragment() is not a well-formed and namespace-well-formed XML fragment. XML parser reported: " + err.getMessage, "FODC0006")
        errorHandler.captureRetainedErrors(xe)
        xe.maybeSetContext(context)
        throw xe
    }
    node
  }
}