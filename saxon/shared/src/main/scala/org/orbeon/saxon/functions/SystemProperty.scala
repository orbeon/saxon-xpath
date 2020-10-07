package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Version

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import SystemProperty._

object SystemProperty {

  def yesOrNo(whatever: Boolean): String = if (whatever) "yes" else "no"

  def getProperty(uri: String, local: String, rsc: RetainedStaticContext): String = {
    val config = rsc.getConfiguration
    val edition = rsc.getPackageData.getTargetEdition
    if (uri == NamespaceConstant.XSLT) {
      local match {
        case "version"                          => "3.0"
        case "vendor"                           => Version.getProductVendor
        case "vendor-url"                       => Version.getWebSiteAddress
        case "product-name"                     => Version.getProductName
        case "product-version"                  => Version.getProductVariantAndVersion(edition)
        case "is-schema-aware"                  => yesOrNo(rsc.getPackageData.isSchemaAware)
        case "supports-serialization"           => yesOrNo("JS" != edition)
        case "supports-backwards-compatibility" => "yes"
        case "supports-namespace-axis"          => "yes"
        case "supports-streaming" =>
          yesOrNo(
            "EE" == edition &&
              config.isLicensedFeature(Configuration.LicenseFeature.ENTERPRISE_XSLT) &&
              config.getConfigurationProperty(Feature.STREAMABILITY) != "off"
          )
        case "supports-dynamic-evaluation"      => yesOrNo(!config.getBooleanProperty(Feature.DISABLE_XSL_EVALUATE))
        case "supports-higher-order-functions"  => "yes"
        case "xpath-version"                    => "3.1"
        case "xsd-version"                      => if (rsc.getConfiguration.getXsdVersion == Configuration.XSD10) "1.0" else "1.1"
        case _                                  => ""
      }
    } else if (uri.isEmpty && config.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      val `val` = System.getProperty(local)
      if (`val` == null) "" else `val`
    } else {
      ""
    }
  }
}

class SystemProperty extends SystemFunction with Callable {

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    if (arguments(0).isInstanceOf[Literal]) {
      try {
        val name: StringValue = arguments(0)
          .asInstanceOf[Literal]
          .value
          .asInstanceOf[StringValue]
        val qName: StructuredQName = StructuredQName.fromLexicalQName(
          name.getStringValue,
          useDefault = false,
          allowEQName = true,
          getRetainedStaticContext)
        val uri: String = qName.getURI
        val local: String = qName.getLocalPart
        if (uri == NamespaceConstant.XSLT &&
          (local == "version" || local == "vendor" || local == "vendor-url" ||
            local == "product-name" ||
            local == "product-version" ||
            local == "supports-backwards-compatibility" ||
            local == "xpath-version" ||
            local == "xsd-version")) {
          val result: String =
            getProperty(uri, local, getRetainedStaticContext)
          new StringLiteral(result)
        }
      } catch {
        case _: XPathException => // no action
      }
    }
    null
  }

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val name: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val qName: StructuredQName = StructuredQName.fromLexicalQName(
      name.getStringValue,
      useDefault = false,
      allowEQName = true,
      getRetainedStaticContext)
    new StringValue(
      getProperty(qName.getURI, qName.getLocalPart, getRetainedStaticContext))
  }

  private def allowsEarlyEvaluation(arguments: Array[Sequence],
                                    context: XPathContext): Boolean = {
    val name: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val qName: StructuredQName = StructuredQName.fromLexicalQName(
      name.getStringValue,
      useDefault = false,
      allowEQName = true,
      getRetainedStaticContext)
    val uri: String = qName.getURI
    val local: String = qName.getLocalPart
    uri == NamespaceConstant.XSLT &&
      (local == "version" || local == "vendor" || local == "vendor-url" ||
        local == "product-name" ||
        local == "product-version" ||
        local == "supports-backwards-compatibility" ||
        local == "xpath-version" ||
        local == "xsd-version")
  }

}
