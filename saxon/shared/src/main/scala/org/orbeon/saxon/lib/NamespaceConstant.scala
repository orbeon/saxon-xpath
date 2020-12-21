////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib




object NamespaceConstant {

  val NULL: String = ""

  /**
    * Fixed namespace name for XML: "http://www.w3.org/XML/1998/namespace".
    */ /**
    * Fixed namespace name for XML: "http://www.w3.org/XML/1998/namespace".
    */
  val XML: String = "http://www.w3.org/XML/1998/namespace"

  /**
    * Fixed namespace name for XSLT: "http://www.w3.org/1999/XSL/Transform"
    */ /**
    * Fixed namespace name for XSLT: "http://www.w3.org/1999/XSL/Transform"
    */
  val XSLT: String = "http://www.w3.org/1999/XSL/Transform"

  /**
    * Current namespace name for SAXON (from 7.0 onwards): "http://saxon.sf.net/"
    */ /**
    * Current namespace name for SAXON (from 7.0 onwards): "http://saxon.sf.net/"
    */
  val SAXON: String = "http://saxon.sf.net/"

  /**
    * Old namespace name for SAXON6: "http://icl.com/saxon"
    */ /**
    * Old namespace name for SAXON6: "http://icl.com/saxon"
    */
  val SAXON6: String = "http://icl.com/saxon"

  /**
    * Fixed namespace name for the export of a Saxon stylesheet package
    */ /**
    * Fixed namespace name for the export of a Saxon stylesheet package
    */
  val SAXON_XSLT_EXPORT: String = "http://ns.saxonica.com/xslt/export"

  /**
    * Namespace name for XML Schema: "http://www.w3.org/2001/XMLSchema"
    */ /**
    * Namespace name for XML Schema: "http://www.w3.org/2001/XMLSchema"
    */
  val SCHEMA: String = "http://www.w3.org/2001/XMLSchema"

  /**
    * XML-schema-defined namespace for use in instance documents ("xsi")
    */ /**
    * XML-schema-defined namespace for use in instance documents ("xsi")
    */
  val SCHEMA_INSTANCE: String = "http://www.w3.org/2001/XMLSchema-instance"

  /**
    * Namespace defined in XSD 1.1 for schema versioning
    */ /**
    * Namespace defined in XSD 1.1 for schema versioning
    */
  val SCHEMA_VERSIONING: String = "http://www.w3.org/2007/XMLSchema-versioning"

  /**
    * Fixed namespace name for SAXON SQL extension: "http://saxon.sf.net/sql"
    */ /**
    * Fixed namespace name for SAXON SQL extension: "http://saxon.sf.net/sql"
    */
  val SQL: String = "http://saxon.sf.net/sql"

  /**
    * Fixed namespace name for EXSLT/Common: "http://exslt.org/common"
    */ /**
    * Fixed namespace name for EXSLT/Common: "http://exslt.org/common"
    */
  val EXSLT_COMMON: String = "http://exslt.org/common"

  /**
    * Fixed namespace name for EXSLT/math: "http://exslt.org/math"
    */ /**
    * Fixed namespace name for EXSLT/math: "http://exslt.org/math"
    */
  val EXSLT_MATH: String = "http://exslt.org/math"

  /**
    * Fixed namespace name for EXSLT/sets: "http://exslt.org/sets"
    */ /**
    * Fixed namespace name for EXSLT/sets: "http://exslt.org/sets"
    */
  val EXSLT_SETS: String = "http://exslt.org/sets"

  /**
    * Fixed namespace name for EXSLT/date: "http://exslt.org/dates-and-times"
    */ /**
    * Fixed namespace name for EXSLT/date: "http://exslt.org/dates-and-times"
    */
  val EXSLT_DATES_AND_TIMES: String = "http://exslt.org/dates-and-times"

  /**
    * Fixed namespace name for EXSLT/random: "http://exslt.org/random"
    */ /**
    * Fixed namespace name for EXSLT/random: "http://exslt.org/random"
    */
  val EXSLT_RANDOM: String = "http://exslt.org/random"

  /**
    * The standard namespace for functions and operators
    */ /**
    * The standard namespace for functions and operators
    */
  val FN: String = "http://www.w3.org/2005/xpath-functions"

  /**
    * The standard namespace for XQuery output declarations
    */ /**
    * The standard namespace for XQuery output declarations
    */
  val OUTPUT: String = "http://www.w3.org/2010/xslt-xquery-serialization"

  /**
    * The standard namespace for system error codes
    */ /**
    * The standard namespace for system error codes
    */
  val ERR: String = "http://www.w3.org/2005/xqt-errors"

  /**
    * Predefined XQuery namespace for local functions
    */ /**
    * Predefined XQuery namespace for local functions
    */
  val LOCAL: String = "http://www.w3.org/2005/xquery-local-functions"

  val MATH: String = "http://www.w3.org/2005/xpath-functions/math"

  /**
    * Namespace URI for XPath 3.0 functions associated with maps
    */ /**
    * Namespace URI for XPath 3.0 functions associated with maps
    */
  val MAP_FUNCTIONS: String = "http://www.w3.org/2005/xpath-functions/map"

  /**
    * Namespace URI for XPath 3.1 functions associated with arrays
    */ /**
    * Namespace URI for XPath 3.1 functions associated with arrays
    */
  val ARRAY_FUNCTIONS: String = "http://www.w3.org/2005/xpath-functions/array"

  val XHTML: String = "http://www.w3.org/1999/xhtml"

  val SVG: String = "http://www.w3.org/2000/svg"

  val MATHML: String = "http://www.w3.org/1998/Math/MathML"

  val XMLNS: String = "http://www.w3.org/2000/xmlns/"

  val XLINK: String = "http://www.w3.org/1999/xlink"

  val XQUERY: String = "http://www.w3.org/2012/xquery"

  val JAVA_TYPE: String = "http://saxon.sf.net/java-type"

  val DOT_NET_TYPE: String = "http://saxon.sf.net/clitype"

  val ANONYMOUS: String = "http://ns.saxonica.com/anonymous-type"

  val SCM: String = "http://ns.saxonica.com/schema-component-model"

  val OBJECT_MODEL_SAXON: String = "http://saxon.sf.net/jaxp/xpath/om"

  val OBJECT_MODEL_XOM: String = "http://www.xom.nu/jaxp/xpath/xom"

  val OBJECT_MODEL_JDOM: String = "http://jdom.org/jaxp/xpath/jdom"

// Note: this URI is a Saxon invention
  val OBJECT_MODEL_AXIOM: String = "http://ws.apache.org/jaxp/xpath/axiom"

  val OBJECT_MODEL_DOM4J: String = "http://www.dom4j.org/jaxp/xpath/dom4j"

  val OBJECT_MODEL_DOT_NET_DOM: String =
    "http://saxon.sf.net/object-model/dotnet/dom"

  val OBJECT_MODEL_DOMINO: String = "http://saxon.sf.net/object-model/domino"

  val CODEPOINT_COLLATION_URI: String =
    "http://www.w3.org/2005/xpath-functions/collation/codepoint"

  val HTML5_CASE_BLIND_COLLATION_URI: String =
    "http://www.w3.org/2005/xpath-functions/collation/html-ascii-case-insensitive"

  val SAXON_GENERATED_VARIABLE: String = SAXON + "generated-variable"

  val SAXON_CONFIGURATION: String = "http://saxon.sf.net/ns/configuration"

  val EXPATH_ZIP: String = "http://expath.org/ns/zip"

  /**
    * URI for the user extension calls in Saxon.js
    */ /**
    * URI for the user extension calls in Saxon.js
    */
  val GLOBAL_JS: String = "http://saxonica.com/ns/globalJS"

  /**
    * URI for the user extension calls in Saxon/C for C++ and PHP
    */ /**
    * URI for the user extension calls in Saxon/C for C++ and PHP
    */
  val PHP: String = "http://php.net/xsl"

  /**
    * URI for interactive XSLT extensions in Saxon-CE and Saxon-JS
    */ /**
    * URI for interactive XSLT extensions in Saxon-CE and Saxon-JS
    */
  val IXSL: String = "http://saxonica.com/ns/interactiveXSLT"

  def getConventionalPrefix(uri: String): String = uri match {
    case XSLT => "xsl"
    case FN => "fn"
    case XML => "xml"
    case SCHEMA => "xs"
    case SCHEMA_INSTANCE => "xsi"
    case IXSL => "ixsl"
    case GLOBAL_JS => "js"
    case SAXON => "saxon"
    case SAXON_GENERATED_VARIABLE => "vv"
    case MATH => "math"
    case MAP_FUNCTIONS => "map"
    case ARRAY_FUNCTIONS => "array"
    case ERR => "err"
    case _ => null

  }

  def getUriForConventionalPrefix(prefix: String): String = prefix match {
    case "xsl" => XSLT
    case "fn" => FN
    case "xml" => XML
    case "xs" => SCHEMA
    case "xsi" => SCHEMA_INSTANCE
    case "err" => ERR
    case "ixsl" => IXSL
    case "js" => GLOBAL_JS
    case "saxon" => SAXON
    case "vv" => SAXON_GENERATED_VARIABLE
    case "math" => MATH
    case "map" => MAP_FUNCTIONS
    case "array" => ARRAY_FUNCTIONS
    case _ => null

  }

  def isReserved(uri: String): Boolean =
    uri != null &&
      (uri == XSLT || uri == FN || uri == MATH || uri == MAP_FUNCTIONS ||
        uri == ARRAY_FUNCTIONS ||
        uri == XML ||
        uri == SCHEMA ||
        uri == SCHEMA_INSTANCE ||
        uri == ERR ||
        uri == XMLNS)

  def isReservedInQuery31(uri: String): Boolean =
    uri == FN || uri == XML || uri == SCHEMA || uri == SCHEMA_INSTANCE ||
      uri == MATH ||
      uri == XQUERY ||
      uri == MAP_FUNCTIONS ||
      uri == ARRAY_FUNCTIONS

  def findSimilarNamespace(candidate: String): String =
    if (isSimilar(candidate, XML)) {
      XML
    } else if (isSimilar(candidate, SCHEMA)) {
      SCHEMA
    } else if (isSimilar(candidate, XSLT)) {
      XSLT
    } else if (isSimilar(candidate, SCHEMA_INSTANCE)) {
      SCHEMA_INSTANCE
    } else if (isSimilar(candidate, FN)) {
      FN
    } else if (isSimilar(candidate, SAXON)) {
      SAXON
    } else if (isSimilar(candidate, EXSLT_COMMON)) {
      EXSLT_COMMON
    } else if (isSimilar(candidate, EXSLT_MATH)) {
      EXSLT_MATH
    } else if (isSimilar(candidate, EXSLT_DATES_AND_TIMES)) {
      EXSLT_DATES_AND_TIMES
    } else if (isSimilar(candidate, EXSLT_RANDOM)) {
      EXSLT_RANDOM
    } else if (isSimilar(candidate, XHTML)) {
      XHTML
    } else if (isSimilar(candidate, ERR)) {
      ERR
    } else if (isSimilar(candidate, JAVA_TYPE)) {
      JAVA_TYPE
    } else if (isSimilar(candidate, DOT_NET_TYPE)) {
      DOT_NET_TYPE
    } else {
      null
    }

  private def isSimilar(s1: String, s2: String): Boolean =
    if (s1.equalsIgnoreCase(s2)) {
      true
    } else if (s1.startsWith(s2) && s1.length - s2.length < 3) {
      true
    } else if (s2.startsWith(s1) && s2.length - s1.length < 3) {
      true
    } else if (s1.length > 8 && Math.abs(s2.length - s1.length) < 3) {
      var diff: Int = 0
      for (i <- 0 until s1.length) {
        val c1: Char = s1.charAt(i)
        if (!((i < s2.length && c1 == s2.charAt(i)) ||
              (i > 0 && i < s2.length - 1 && c1 == s2.charAt(i - 1)) ||
              (i + 1 < s2.length && c1 == s2.charAt(i + 1)))) {
          { diff += 1; diff - 1 }
        }
      }
      diff < 3
    } else {
      false
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is not instantiated, it exists to hold a set of constants representing known
  * namespaces. For each of these, there is a constant for the namespace URI and for many of
  * them, there is a numeric constant used as the code for this namespace in the name pool.
  * <p>This class also defines constant URIs for some objects other than namespaces -
  * for example, URIs that identify the various object models used in the JAXP XPath API,
  * and the Unicode codepoint collation URI.</p>
  */
