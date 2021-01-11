package org.orbeon.saxon.functions

import org.orbeon.saxon.event.{ComplexContentOutputter, ReceiverOption}
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, SimpleType, Untyped}
import org.orbeon.saxon.om._
import org.orbeon.saxon.regex.{RegexIterator, RegularExpression}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.xml.sax.InputSource

import java.{util => ju}
import javax.xml.transform.sax.SAXSource


class AnalyzeStringFn extends RegexFunction {

  private var resultName   : NodeName   = _
  private var nonMatchName : NodeName   = _
  private var matchName    : NodeName   = _
  private var groupName    : NodeName   = _
  private var groupNrName  : NodeName   = _
  private var resultType   : SchemaType = Untyped.getInstance
  private var nonMatchType : SchemaType = Untyped.getInstance
  private var matchType    : SchemaType = Untyped.getInstance
  private var groupType    : SchemaType = Untyped.getInstance
  private var groupNrType  : SimpleType = BuiltInAtomicType.UNTYPED_ATOMIC

  override def allowRegexMatchingEmptyString(): Boolean = false

  private def init(config: Configuration, schemaAware: Boolean): Unit = {
    synchronized {
      resultName = new FingerprintedQName("",
        NamespaceConstant.FN,
        "analyze-string-result")
      nonMatchName =
        new FingerprintedQName("", NamespaceConstant.FN, "non-match")
      matchName = new FingerprintedQName("", NamespaceConstant.FN, "match")
      groupName = new FingerprintedQName("", NamespaceConstant.FN, "group")
      groupNrName = new NoNamespaceName("nr")
      if (schemaAware) {
        resultType = config.getSchemaType(
          new StructuredQName("",
            NamespaceConstant.FN,
            "analyze-string-result-type"))
        nonMatchType = BuiltInAtomicType.STRING
        matchType = config.getSchemaType(
          new StructuredQName("", NamespaceConstant.FN, "match-type"))
        groupType = config.getSchemaType(
          new StructuredQName("", NamespaceConstant.FN, "group-type"))
        groupNrType = BuiltInAtomicType.POSITIVE_INTEGER
        if (resultType == null || matchType == null || groupType == null) {
          throw new XPathException(
            "Schema for analyze-string has not been successfully loaded")
        }
      }
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): NodeInfo = {
    val inputItem: Item = arguments(0).head
    var input: CharSequence = null
    input = if (inputItem == null) "" else inputItem.getStringValueCS
    val re: RegularExpression = getRegularExpression(arguments)
    val iter: RegexIterator = re.analyze(input)
    if (resultName == null) {
      val schemaAware = context.getController.getExecutable.isSchemaAware
      val config = context.getConfiguration
      config.synchronized {
        // ORBEON: No actual Schema support and `config.isSchemaAvailable() == false`.
        if (schemaAware && ! config.isSchemaAvailable(NamespaceConstant.FN)) {
          val inputStream = Configuration.locateResource("xpath-functions.scm", new ju.ArrayList)
          if (inputStream == null)
            throw new XPathException("Failed to load xpath-functions.scm from the classpath")
          val is = new InputSource(inputStream)
          if (config.isTiming)
            config.getLogger.info("Loading schema from resources for: " + NamespaceConstant.FN)
          config.addSchemaSource(new SAXSource(is))
        }
      }
      init(context.getConfiguration, schemaAware)
    }
    val builder = context.getController.makeBuilder
    val out     = new ComplexContentOutputter(builder)
    builder.setBaseURI(getStaticBaseUriString)
    out.open()
    out.startElement(resultName, resultType, Loc.NONE, ReceiverOption.NONE)
    out.startContent()
    var item: Item = null
    while ({
      item = iter.next()
      item
    } != null)
      if (iter.isMatching) {
        out.startElement(matchName, matchType, Loc.NONE, ReceiverOption.NONE)
        out.startContent()
        iter.processMatchingSubstring(new RegexIterator.MatchHandler() {
          def characters(s: CharSequence): Unit =
            out.characters(s, Loc.NONE, ReceiverOption.NONE)

          def onGroupStart(groupNumber: Int): Unit = {
            out.startElement(groupName, groupType, Loc.NONE, ReceiverOption.NONE)
            out.attribute(groupNrName,
              groupNrType,
              "" + groupNumber,
              Loc.NONE,
              ReceiverOption.NONE)
            out.startContent()
          }

          def onGroupEnd(groupNumber: Int): Unit =
            out.endElement()
        })
        out.endElement()
      } else {
        out.startElement(nonMatchName,
          nonMatchType,
          Loc.NONE,
          ReceiverOption.NONE)
        out.startContent()
        out.characters(item.getStringValueCS, Loc.NONE, ReceiverOption.NONE)
        out.endElement()
      }
    out.endElement()
    out.close()
    builder.getCurrentRoot
  }
}
