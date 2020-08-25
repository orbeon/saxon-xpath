package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Version

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RetainedStaticContext

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.StringValue

import SystemProperty._

object SystemProperty {

  def yesOrNo(whatever: Boolean): String = if (whatever) "yes" else "no"

  def getProperty(uri: String,
                  local: String,
                  rsc: RetainedStaticContext): String = {
    val config: Configuration = rsc.getConfiguration
    val edition: String = rsc.getPackageData.getTargetEdition
    if (uri == NamespaceConstant.XSLT) {
      local match {
        case "version" => return "3.0"
        case "vendor" => Version.getProductVendor
        case "vendor-url" => Version.getWebSiteAddress
        case "product-name" => Version.getProductName
        case "product-version" => Version.getProductVariantAndVersion(edition)
        case "is-schema-aware" =>
          var schemaAware: Boolean = rsc.getPackageData.isSchemaAware
          yesOrNo(schemaAware)
        case "supports-serialization" => yesOrNo("JS" != edition)
        case "supports-backwards-compatibility" => return "yes"
        case "supports-namespace-axis" => return "yes"
        case "supports-streaming" =>
          yesOrNo(
            "EE" == edition &&
              config.isLicensedFeature(
                Configuration.LicenseFeature.ENTERPRISE_XSLT) &&
              config.getConfigurationProperty(Feature.STREAMABILITY).!=("off"))
        case "supports-dynamic-evaluation" =>
          yesOrNo(!config.getBooleanProperty(Feature.DISABLE_XSL_EVALUATE))
        case "supports-higher-order-functions" => return "yes"
        case "xpath-version" => return "3.1"
        case "xsd-version" =>
          if (rsc.getConfiguration.getXsdVersion == Configuration.XSD10) return "1.0"
          else return "1.1"

      }
      ""
    } else if (uri.isEmpty &&
      config.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      val `val`: String = System.getProperty(local)
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
          false,
          true,
          getRetainedStaticContext)
        val uri: String = qName.getURI
        val local: String = qName.getLocalPart
        if (uri == NamespaceConstant.XSLT &&
          (local.==("version") || local.==("vendor") || local.==(
            "vendor-url") ||
            local.==("product-name") ||
            local.==("product-version") ||
            local.==("supports-backwards-compatibility") ||
            local.==("xpath-version") ||
            local.==("xsd-version"))) {
          val result: String =
            getProperty(uri, local, getRetainedStaticContext)
          new StringLiteral(result)
        }
      } catch {
        case e: XPathException => {}

      }
    }
    null
  }

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val name: StringValue = arguments(0).head().asInstanceOf[StringValue]
    val qName: StructuredQName = StructuredQName.fromLexicalQName(
      name.getStringValue,
      false,
      true,
      getRetainedStaticContext)
    new StringValue(
      getProperty(qName.getURI, qName.getLocalPart, getRetainedStaticContext))
  }

  private def allowsEarlyEvaluation(arguments: Array[Sequence],
                                    context: XPathContext): Boolean = {
    val name: StringValue = arguments(0).head().asInstanceOf[StringValue]
    val qName: StructuredQName = StructuredQName.fromLexicalQName(
      name.getStringValue,
      false,
      true,
      getRetainedStaticContext)
    val uri: String = qName.getURI
    val local: String = qName.getLocalPart
    uri == NamespaceConstant.XSLT &&
      (local.==("version") || local.==("vendor") || local.==("vendor-url") ||
        local.==("product-name") ||
        local.==("product-version") ||
        local.==("supports-backwards-compatibility") ||
        local.==("xpath-version") ||
        local.==("xsd-version"))
  }

}
