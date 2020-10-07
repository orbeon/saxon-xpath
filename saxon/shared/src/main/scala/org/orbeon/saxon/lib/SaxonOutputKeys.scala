////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.model.ValidationException

import org.orbeon.saxon.om.NameChecker

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.QNameException

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BigDecimalValue

import javax.xml.transform.OutputKeys

import java.math.BigDecimal

import java.util.Properties

import java.util.StringTokenizer




object SaxonOutputKeys {

  /*@NotNull*/

  val SAXON_XQUERY_METHOD: String = "{http://saxon.sf.net/}xquery"

  /*@NotNull*/

  val SAXON_BASE64_BINARY_METHOD: String = "{http://saxon.sf.net/}base64Binary"

  /*@NotNull*/

  val SAXON_HEX_BINARY_METHOD: String = "{http://saxon.sf.net/}hexBinary"

  /*@NotNull*/

  val SAXON_XML_TO_JSON_METHOD: String = "{http://saxon.sf.net/}xml-to-json"

  /*@NotNull*/

  val ALLOW_DUPLICATE_NAMES: String = "allow-duplicate-names"

  /*@NotNull*/

  val BUILD_TREE: String = "build-tree"

  /*@NotNull*/

  val INDENT_SPACES: String = "{http://saxon.sf.net/}indent-spaces"

  /*@NotNull*/

  val LINE_LENGTH: String = "{http://saxon.sf.net/}line-length"

  /*@NotNull*/

  val SINGLE_QUOTES: String = "{http://saxon.sf.net/}single-quotes"

  /*@NotNull*/

  val SUPPRESS_INDENTATION: String = "suppress-indentation"

  val HTML_VERSION: String = "html-version"

  val ITEM_SEPARATOR: String = "item-separator"

  /*@NotNull*/

  val JSON_NODE_OUTPUT_METHOD: String = "json-node-output-method"

  /*@NotNull*/

  val ATTRIBUTE_ORDER: String = "{http://saxon.sf.net/}attribute-order"

  /*@NotNull*/

  val CANONICAL: String = "{http://saxon.sf.net/}canonical"

  /*@NotNull*/

  val PROPERTY_ORDER: String = "{http://saxon.sf.net/}property-order"

  /*@NotNull*/

  val DOUBLE_SPACE: String = "{http://saxon.sf.net/}double-space"

  /*@NotNull*/

  val NEWLINE: String = "{http://saxon.sf.net/}newline"

  /*@NotNull*/

  val STYLESHEET_VERSION: String = "{http://saxon.sf.net/}stylesheet-version"

  /*@NotNull*/

  val USE_CHARACTER_MAPS: String = "use-character-maps"

  /*@NotNull*/

  val INCLUDE_CONTENT_TYPE: String = "include-content-type"

  /*@NotNull*/

  val UNDECLARE_PREFIXES: String = "undeclare-prefixes"

  /*@NotNull*/

  val ESCAPE_URI_ATTRIBUTES: String = "escape-uri-attributes"

  /*@NotNull*/

  val CHARACTER_REPRESENTATION: String =
    "{http://saxon.sf.net/}character-representation"

  /*@NotNull*/

  val NEXT_IN_CHAIN: String = "{http://saxon.sf.net/}next-in-chain"

  /*@NotNull*/

  val NEXT_IN_CHAIN_BASE_URI: String =
    "{http://saxon.sf.net/}next-in-chain-base-uri"

  /*@NotNull*/

  val PARAMETER_DOCUMENT: String = "parameter-document"

  /*@NotNull*/

  val PARAMETER_DOCUMENT_BASE_URI: String =
    "{http://saxon.sf.net/}parameter-document-base-uri"

  /*@NotNull*/

  val BYTE_ORDER_MARK: String = "byte-order-mark"

  /*@NotNull*/

  val NORMALIZATION_FORM: String = "normalization-form"

  /*@NotNull*/

  val RECOGNIZE_BINARY: String = "{http://saxon.sf.net/}recognize-binary"

  /*@NotNull*/

  val REQUIRE_WELL_FORMED: String = "{http://saxon.sf.net/}require-well-formed"

  /*@NotNull*/

  val SUPPLY_SOURCE_LOCATOR: String =
    "{http://saxon.sf.net/}supply-source-locator"

  /*@NotNull*/

  val WRAP: String = "{http://saxon.sf.net/}wrap-result-sequence"

  val UNFAILING: String = "{http://saxon.sf.net/}unfailing"

  /*@NotNull*/

  def parseListOfNodeNames(value: String,
                           nsResolver: NamespaceResolver,
                           useDefaultNS: Boolean,
                           prevalidated: Boolean,
                           allowStar: Boolean,
                           errorCode: String): String = {
    val s: StringBuilder = new StringBuilder()
    val st: StringTokenizer = new StringTokenizer(value, " \t\n\r", false)
    while (st.hasMoreTokens()) {
      val displayname: String = st.nextToken()
      if (allowStar && "*" == displayname) {
        s.append(' ').append(displayname)
      } else if (prevalidated || (nsResolver == null)) {
        s.append(' ').append(displayname)
      } else if (displayname.startsWith("Q{")) {
        s.append(' ').append(displayname)
      } else {
        val parts: Array[String] = NameChecker.getQNameParts(displayname)
        val muri: String = nsResolver.getURIForPrefix(parts(0), useDefaultNS)
        if (muri == null) {
          throw new XPathException(
            "Namespace prefix '" + parts(0) + "' has not been declared",
            errorCode)
        }
        s.append(" Q{").append(muri).append('}').append(parts(1))
      }
    }
    s.toString
  }

  /**
    * Ask whether a particular serialization property is to be considered as string-valued,
    * in which case the value is used exactly as specified without any whitespace stripping.
    *
    * <p>The logic here is a little pragmatic.</p>
    * <p>For XSLT (xsl:output and xsl:result-document) the properties doctype-system, doctype-public,
    *    item-separator, and media-type have string type and whitespace should therefore be retained.</p>
    * <p>For values in a parameter document, doctype-system is not whitespace stripped (and is restricted
    *    by a pattern); doctype-public is whitespace-stripped; media-type is whitespace-stripped</p>
    * <p>For values in an fn:serialize parameter map, doctype-system, doctype-public, encoding,
    *    item-separator, media-type, and version are non-stripped strings</p>
    * <p>XQuery output declarations follow the rules for parameter documents</p>
    * <p>Pragmatically, it makes sense to be consistent. Including whitespace in doctype-system,
    *    doctype-public, encoding, version, or media-type is never useful. So we apply
    *    whitespace-stripping to all properties other than item-separator.</p>
    *
    * @param key the property name, in Clark notation
    * @return true if the property is retained as written without whitespace stripping
    */
  def isUnstrippedProperty(key: String): Boolean =
    ITEM_SEPARATOR == key || NEWLINE == key

  /**
    * Examine the already-validated properties to see whether the html-version property is present
    * with the decimal value 5.; used to decide whether to produce XHTML 5.0 in the XHTML output
    * method.
    *
    * @param properties the properties to be examined
    * @return true if the properties include html-version="5.0". The property is a decimal value, so
    *         it can also be written, for example, "5" or "+5.00".
    */
  def isXhtmlHtmlVersion5(properties: Properties): Boolean = {
    val htmlVersion: String =
      properties.getProperty(SaxonOutputKeys.HTML_VERSION)
    try htmlVersion != null &&
      BigDecimalValue
        .makeDecimalValue(htmlVersion, validate = false)
        .asAtomic()
        .asInstanceOf[BigDecimalValue]
        .getDecimalValue == BigDecimal.valueOf(5)
    catch {
      case e: ValidationException => false

    }
  }

  /**
    * Examine the already-validated properties to see whether the html-version property is present
    * with the decimal value 5.0, or if absent, the version property is present with the value 5.0.
    * Used to decide whether to produce HTML5 output in the HTML output method.
    *
    * @param properties the properties to be examined
    * @return true if the properties include html-version="5.0". The property is a decimal value, so
    *         it can also be written, for example, "5" or "+5.00".
    */
  def isHtmlVersion5(properties: Properties): Boolean = {
    var htmlVersion: String =
      properties.getProperty(SaxonOutputKeys.HTML_VERSION)
    if (htmlVersion == null) {
      htmlVersion = properties.getProperty(OutputKeys.VERSION)
    }
    if (htmlVersion != null) {
      try BigDecimalValue
        .makeDecimalValue(htmlVersion, validate = false)
        .asAtomic()
        .asInstanceOf[BigDecimalValue]
        .getDecimalValue == BigDecimal.valueOf(5)
      catch {
        case e: ValidationException => false

      }
    } else {
// Change in 10.0 to make HTML5 the default
      true
    }
  }

  def isBuildTree(properties: Properties): Boolean = {
    val buildTreeProperty: String = properties.getProperty("build-tree")
    if (buildTreeProperty != null) {
      return "yes" == buildTreeProperty
    }
    val method: String = properties.getProperty("method")
    !("json" == method || "adaptive" == method)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Provides string constants that can be used to set
  * output properties for a Transformer, or to retrieve
  * output properties from a Transformer or Templates object.
  * <p>These keys are private Saxon keys that supplement the standard keys
  * defined in javax.xml.transform.OutputKeys. As well as Saxon extension
  * attributes, the list includes new attributes defined in XSLT 2.0 which
  * are not yet supported in JAXP</p>
  *
  * <p>Note that for JAXP compatibility, the names of properties use Clark format,
  * that is <code>{uri}local</code>. However, from 10.0, the values of properties
  * containing QNames, such as <code>cdata-section-elements</code>, use EQName
  * format, that is <code>Q{uri}local</code>.</p>
  */
