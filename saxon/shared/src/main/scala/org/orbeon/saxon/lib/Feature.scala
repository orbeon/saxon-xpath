////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AUTO-GENERATED FROM FeatureKeys.xml - DO NOT EDIT THIS FILE
package org.orbeon.saxon.lib


import java.{util => ju}

import org.orbeon.saxon.lib.Feature._
import org.orbeon.saxon.utils.Configuration

import scala.collection.mutable
//import scala.collection.compat._
import scala.jdk.CollectionConverters._


/**
 * Feature defines a set of constants, representing the details of Saxon configuration
 * options which can be supplied to the Saxon method Configuration.setProperty(),
 * and to other interfaces
 */
object Feature {

  private val HE: Int = 0
  private val PE: Int = 1
  private val EE: Int = 2

  private val index: mutable.Map[String, Feature[_]] = mutable.TreeMap[String, Feature[_]]()

  def byName(name: String): Feature[_] = index.get(name).orNull

  def getNames: ju.Iterator[String] = index.keySet.iterator.asJava

  val ALLOW_EXTERNAL_FUNCTIONS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/allow-external-functions",
    1,
    HE,
    classOf[Boolean],
    false)

  val ALLOW_MULTITHREADING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/allow-multithreading",
    2,
    EE,
    classOf[Boolean],
    false)

  val ALLOW_OLD_JAVA_URI_FORMAT: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/allow-old-java-uri-format",
    3,
    PE,
    classOf[Boolean],
    false)

  val ALLOW_SYNTAX_EXTENSIONS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/allowSyntaxExtensions",
    4,
    PE,
    classOf[Boolean],
    false)

  val ASSERTIONS_CAN_SEE_COMMENTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/assertionsCanSeeComments",
    5,
    EE,
    classOf[Boolean],
    false)

  val COLLATION_URI_RESOLVER: Feature[org.orbeon.saxon.lib.CollationURIResolver] =
    new Feature[org.orbeon.saxon.lib.CollationURIResolver](
      "http://saxon.sf.net/feature/collation-uri-resolver",
      6,
      HE,
      classOf[org.orbeon.saxon.lib.CollationURIResolver],
      null)

  val COLLATION_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/collation-uri-resolver-class",
    7,
    HE,
    classOf[String],
    null)

  val COLLECTION_FINDER: Feature[org.orbeon.saxon.lib.CollectionFinder] =
    new Feature[org.orbeon.saxon.lib.CollectionFinder](
      "http://saxon.sf.net/feature/collection-finder",
      8,
      HE,
      classOf[org.orbeon.saxon.lib.CollectionFinder],
      null)

  val COLLECTION_FINDER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/collection-finder-class",
    9,
    HE,
    classOf[String],
    null)

  val COMPILE_WITH_TRACING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/compile-with-tracing",
    12,
    HE,
    classOf[Boolean],
    false)

  val CONFIGURATION: Feature[Configuration] =
    new Feature[Configuration](
      "http://saxon.sf.net/feature/configuration",
      13,
      HE,
      classOf[Configuration],
      null)

  val CONFIGURATION_FILE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/configuration-file",
    14,
    PE,
    classOf[String],
    null)

  val DEBUG_BYTE_CODE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/debugByteCode",
    15,
    EE,
    classOf[Boolean],
    false)

  val DEBUG_BYTE_CODE_DIR: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/debugByteCodeDir",
    16,
    EE,
    classOf[String],
    null)

  val DEFAULT_COLLATION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/defaultCollation",
    17,
    HE,
    classOf[String],
    null)

  val DEFAULT_COLLECTION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/defaultCollection",
    18,
    HE,
    classOf[String],
    null)

  val DEFAULT_COUNTRY: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/defaultCountry",
    19,
    HE,
    classOf[String],
    null)

  val DEFAULT_LANGUAGE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/defaultLanguage",
    20,
    HE,
    classOf[String],
    null)

  val DEFAULT_REGEX_ENGINE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/defaultRegexEngine",
    21,
    HE,
    classOf[String],
    null)

  val DISABLE_XSL_EVALUATE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/disableXslEvaluate",
    22,
    EE,
    classOf[Boolean],
    false)

  val DISPLAY_BYTE_CODE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/displayByteCode",
    23,
    EE,
    classOf[Boolean],
    false)

  val DTD_VALIDATION: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/validation",
    24,
    HE,
    classOf[Boolean],
    false)

  val DTD_VALIDATION_RECOVERABLE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/dtd-validation-recoverable",
    25,
    HE,
    classOf[Boolean],
    false)

  val EAGER_EVALUATION: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/eagerEvaluation",
    26,
    HE,
    classOf[Boolean],
    false)

  val ENTITY_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/entityResolverClass",
    27,
    HE,
    classOf[String],
    null)

  val ENVIRONMENT_VARIABLE_RESOLVER: Feature[
    org.orbeon.saxon.lib.EnvironmentVariableResolver] =
    new Feature[org.orbeon.saxon.lib.EnvironmentVariableResolver](
      "http://saxon.sf.net/feature/environmentVariableResolver",
      28,
      HE,
      classOf[org.orbeon.saxon.lib.EnvironmentVariableResolver],
      null)

  val ENVIRONMENT_VARIABLE_RESOLVER_CLASS: Feature[String] =
    new Feature[String](
      "http://saxon.sf.net/feature/environmentVariableResolverClass",
      29,
      HE,
      classOf[String],
      null)

  val ERROR_LISTENER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/errorListenerClass",
    30,
    HE,
    classOf[String],
    null)

  val EXPAND_ATTRIBUTE_DEFAULTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/expandAttributeDefaults",
    31,
    HE,
    classOf[Boolean],
    false)

  val EXPATH_FILE_DELETE_TEMPORARY_FILES: Feature[Boolean] =
    new Feature[Boolean](
      "http://saxon.sf.net/feature/expathFileDeleteTemporaryFiles",
      32,
      PE,
      classOf[Boolean],
      false)

  val GENERATE_BYTE_CODE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/generateByteCode",
    33,
    EE,
    classOf[Boolean],
    false)

  val IGNORE_SAX_SOURCE_PARSER: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/ignoreSAXSourceParser",
    34,
    HE,
    classOf[Boolean],
    false)

  val IMPLICIT_SCHEMA_IMPORTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/implicitSchemaImports",
    35,
    EE,
    classOf[Boolean],
    false)

  val LAZY_CONSTRUCTION_MODE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/lazyConstructionMode",
    36,
    HE,
    classOf[Boolean],
    false)

  val LICENSE_FILE_LOCATION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/licenseFileLocation",
    37,
    PE,
    classOf[String],
    null)

  val LINE_NUMBERING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/linenumbering",
    38,
    HE,
    classOf[Boolean],
    false)

  val MARK_DEFAULTED_ATTRIBUTES: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/markDefaultedAttributes",
    39,
    HE,
    classOf[Boolean],
    false)

  val MAX_COMPILED_CLASSES: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/maxCompiledClasses",
    40,
    EE,
    classOf[Integer],
    null)

  val MESSAGE_EMITTER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/messageEmitterClass",
    41,
    HE,
    classOf[String],
    null)

  val MODULE_URI_RESOLVER: Feature[org.orbeon.saxon.lib.ModuleURIResolver] =
    new Feature[org.orbeon.saxon.lib.ModuleURIResolver](
      "http://saxon.sf.net/feature/moduleURIResolver",
      42,
      HE,
      classOf[org.orbeon.saxon.lib.ModuleURIResolver],
      null)

  val MODULE_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/moduleURIResolverClass",
    43,
    HE,
    classOf[String],
    null)

  val MONITOR_HOT_SPOT_BYTE_CODE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/monitorHotSpotByteCode",
    44,
    EE,
    classOf[Boolean],
    false)

  val MULTIPLE_SCHEMA_IMPORTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/multipleSchemaImports",
    45,
    EE,
    classOf[Boolean],
    false)

  val NAME_POOL: Feature[org.orbeon.saxon.om.NamePool] =
    new Feature[org.orbeon.saxon.om.NamePool](
      "http://saxon.sf.net/feature/namePool",
      46,
      HE,
      classOf[org.orbeon.saxon.om.NamePool],
      null)

  val OCCURRENCE_LIMITS: Feature[Any] = new Feature[Any](
    "http://saxon.sf.net/feature/occurrenceLimits",
    47,
    EE,
    classOf[AnyRef],
    null)

  val OPTIMIZATION_LEVEL: Feature[Any] = new Feature[Any](
    "http://saxon.sf.net/feature/optimizationLevel",
    48,
    HE,
    classOf[AnyRef],
    null)

  val OUTPUT_URI_RESOLVER: Feature[org.orbeon.saxon.lib.OutputURIResolver] =
    new Feature[org.orbeon.saxon.lib.OutputURIResolver](
      "http://saxon.sf.net/feature/outputURIResolver",
      49,
      HE,
      classOf[org.orbeon.saxon.lib.OutputURIResolver],
      null)

  val OUTPUT_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/outputURIResolverClass",
    50,
    HE,
    classOf[String],
    null)

  val PRE_EVALUATE_DOC_FUNCTION: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/preEvaluateDocFunction",
    51,
    HE,
    classOf[Boolean],
    false)

  val PREFER_JAXP_PARSER: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/preferJaxpParser",
    52,
    HE,
    classOf[Boolean],
    false)

  val RECOGNIZE_URI_QUERY_PARAMETERS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/recognize-uri-query-parameters",
    53,
    HE,
    classOf[Boolean],
    false)

  val RECOVERY_POLICY: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/recoveryPolicy",
    54,
    HE,
    classOf[Integer],
    null)

  val RECOVERY_POLICY_NAME: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/recoveryPolicyName",
    55,
    HE,
    classOf[String],
    null)

  val RESULT_DOCUMENT_THREADS: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/resultDocumentThreads",
    56,
    EE,
    classOf[Integer],
    null)

  val RETAIN_DTD_ATTRIBUTE_TYPES: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/retain-dtd-attribute-types",
    57,
    HE,
    classOf[Boolean],
    false)

  val SCHEMA_URI_RESOLVER: Feature[org.orbeon.saxon.lib.SchemaURIResolver] =
    new Feature[org.orbeon.saxon.lib.SchemaURIResolver](
      "http://saxon.sf.net/feature/schemaURIResolver",
      58,
      EE,
      classOf[org.orbeon.saxon.lib.SchemaURIResolver],
      null)

  val SCHEMA_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/schemaURIResolverClass",
    59,
    EE,
    classOf[String],
    null)

  val SCHEMA_VALIDATION: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/schema-validation",
    60,
    EE,
    classOf[Integer],
    null)

  val SCHEMA_VALIDATION_MODE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/schema-validation-mode",
    61,
    EE,
    classOf[String],
    null)

  val SERIALIZER_FACTORY_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/serializerFactoryClass",
    62,
    HE,
    classOf[String],
    null)

  val SOURCE_PARSER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/sourceParserClass",
    63,
    HE,
    classOf[String],
    null)

  val SOURCE_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/sourceResolverClass",
    64,
    HE,
    classOf[String],
    null)

  val STABLE_COLLECTION_URI: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/stableCollectionUri",
    65,
    HE,
    classOf[Boolean],
    false)

  val STABLE_UNPARSED_TEXT: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/stableUnparsedText",
    66,
    HE,
    classOf[Boolean],
    false)

  val STANDARD_ERROR_OUTPUT_FILE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/standardErrorOutputFile",
    67,
    HE,
    classOf[String],
    null)

  val STREAMABILITY: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/streamability",
    68,
    EE,
    classOf[String],
    null)

  val STRICT_STREAMABILITY: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/strictStreamability",
    69,
    EE,
    classOf[Boolean],
    false)

  val STREAMING_FALLBACK: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/streamingFallback",
    70,
    HE,
    classOf[Boolean],
    false)

  val STRIP_WHITESPACE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/strip-whitespace",
    71,
    HE,
    classOf[String],
    null)

  val STYLE_PARSER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/styleParserClass",
    72,
    HE,
    classOf[String],
    null)

  val SUPPRESS_EVALUATION_EXPIRY_WARNING: Feature[Boolean] =
    new Feature[Boolean](
      "http://saxon.sf.net/feature/suppressEvaluationExpiryWarning",
      73,
      PE,
      classOf[Boolean],
      false)

  val SUPPRESS_XPATH_WARNINGS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/suppressXPathWarnings",
    74,
    HE,
    classOf[Boolean],
    false)

  val SUPPRESS_XSLT_NAMESPACE_CHECK: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/suppressXsltNamespaceCheck",
    75,
    HE,
    classOf[Boolean],
    false)

  val THRESHOLD_FOR_COMPILING_TYPES: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/thresholdForCompilingTypes",
    76,
    HE,
    classOf[Integer],
    null)

  val TIMING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/timing",
    77,
    HE,
    classOf[Boolean],
    false)

  val TRACE_EXTERNAL_FUNCTIONS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/trace-external-functions",
    78,
    PE,
    classOf[Boolean],
    false)

  val TRACE_LISTENER: Feature[org.orbeon.saxon.lib.TraceListener] =
    new Feature[org.orbeon.saxon.lib.TraceListener](
      "http://saxon.sf.net/feature/traceListener",
      79,
      HE,
      classOf[org.orbeon.saxon.lib.TraceListener],
      null)

  val TRACE_LISTENER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/traceListenerClass",
    80,
    HE,
    classOf[String],
    null)

  val TRACE_LISTENER_OUTPUT_FILE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/traceListenerOutputFile",
    81,
    HE,
    classOf[String],
    null)

  val TRACE_OPTIMIZER_DECISIONS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/trace-optimizer-decisions",
    82,
    PE,
    classOf[Boolean],
    false)

  val TREE_MODEL: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/treeModel",
    83,
    HE,
    classOf[Integer],
    null)

  val TREE_MODEL_NAME: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/treeModelName",
    84,
    HE,
    classOf[String],
    null)

  val UNPARSED_TEXT_URI_RESOLVER: Feature[
    org.orbeon.saxon.lib.UnparsedTextURIResolver] =
    new Feature[org.orbeon.saxon.lib.UnparsedTextURIResolver](
      "http://saxon.sf.net/feature/unparsedTextURIResolver",
      85,
      HE,
      classOf[org.orbeon.saxon.lib.UnparsedTextURIResolver],
      null)

  val UNPARSED_TEXT_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/unparsedTextURIResolverClass",
    86,
    HE,
    classOf[String],
    null)

  val URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/uriResolverClass",
    87,
    HE,
    classOf[String],
    null)

  val USE_PI_DISABLE_OUTPUT_ESCAPING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/use-pi-disable-output-escaping",
    88,
    HE,
    classOf[Boolean],
    false)

  val USE_TYPED_VALUE_CACHE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/use-typed-value-cache",
    89,
    EE,
    classOf[Boolean],
    false)

  val USE_XSI_SCHEMA_LOCATION: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/useXsiSchemaLocation",
    90,
    EE,
    classOf[Boolean],
    false)

  val VALIDATION_COMMENTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/validation-comments",
    91,
    EE,
    classOf[Boolean],
    false)

  val VALIDATION_WARNINGS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/validation-warnings",
    92,
    EE,
    classOf[Boolean],
    false)

  val VERSION_WARNING: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/version-warning",
    93,
    HE,
    classOf[Boolean],
    false)

  val XINCLUDE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xinclude-aware",
    94,
    HE,
    classOf[Boolean],
    false)

  val XML_VERSION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xml-version",
    95,
    HE,
    classOf[String],
    null)

  val XML_PARSER_FEATURE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/parserFeature?uri=",
    96,
    HE,
    classOf[Boolean],
    false)

  val XML_PARSER_PROPERTY: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/parserProperty?uri=",
    97,
    HE,
    classOf[Boolean],
    false)

  val XQUERY_ALLOW_UPDATE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryAllowUpdate",
    98,
    EE,
    classOf[Boolean],
    false)

  val XQUERY_CONSTRUCTION_MODE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xqueryConstructionMode",
    99,
    HE,
    classOf[String],
    null)

  val XQUERY_DEFAULT_ELEMENT_NAMESPACE: Feature[Any] = new Feature[Any](
    "http://saxon.sf.net/feature/xqueryDefaultElementNamespace",
    100,
    HE,
    classOf[AnyRef],
    null)

  val XQUERY_DEFAULT_FUNCTION_NAMESPACE: Feature[Any] = new Feature[Any](
    "http://saxon.sf.net/feature/xqueryDefaultFunctionNamespace",
    101,
    HE,
    classOf[AnyRef],
    null)

  val XQUERY_EMPTY_LEAST: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryEmptyLeast",
    102,
    HE,
    classOf[Boolean],
    false)

  val XQUERY_INHERIT_NAMESPACES: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryInheritNamespaces",
    103,
    HE,
    classOf[Boolean],
    false)

  val XQUERY_MULTIPLE_MODULE_IMPORTS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryMultipleModuleImports",
    104,
    EE,
    classOf[Boolean],
    false)

  val XQUERY_PRESERVE_BOUNDARY_SPACE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryPreserveBoundarySpace",
    105,
    HE,
    classOf[Boolean],
    false)

  val XQUERY_PRESERVE_NAMESPACES: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xqueryPreserveNamespaces",
    106,
    HE,
    classOf[Boolean],
    false)

  val XQUERY_REQUIRED_CONTEXT_ITEM_TYPE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xqueryRequiredContextItemType",
    107,
    HE,
    classOf[String],
    null)

  val XQUERY_SCHEMA_AWARE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xquerySchemaAware",
    108,
    EE,
    classOf[Boolean],
    false)

  val XQUERY_STATIC_ERROR_LISTENER_CLASS: Feature[String] =
    new Feature[String](
      "http://saxon.sf.net/feature/xqueryStaticErrorListenerClass",
      109,
      HE,
      classOf[String],
      null)

  val XQUERY_VERSION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xqueryVersion",
    110,
    HE,
    classOf[String],
    null)

  val XSD_VERSION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xsd-version",
    111,
    EE,
    classOf[String],
    null)

  val XSLT_ENABLE_ASSERTIONS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/enableAssertions",
    112,
    PE,
    classOf[Boolean],
    false)

  val XSLT_INITIAL_MODE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/initialMode",
    113,
    HE,
    classOf[String],
    null)

  val XSLT_INITIAL_TEMPLATE: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/initialTemplate",
    114,
    HE,
    classOf[String],
    null)

  val XSLT_SCHEMA_AWARE: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/xsltSchemaAware",
    115,
    EE,
    classOf[Boolean],
    false)

  val XSLT_STATIC_ERROR_LISTENER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/stylesheetErrorListener",
    116,
    HE,
    classOf[String],
    null)

  val XSLT_STATIC_URI_RESOLVER_CLASS: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/stylesheetURIResolver",
    117,
    HE,
    classOf[String],
    null)

  val XSLT_VERSION: Feature[String] = new Feature[String](
    "http://saxon.sf.net/feature/xsltVersion",
    118,
    HE,
    classOf[String],
    null)

  val REGEX_BACKTRACKING_LIMIT: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/regexBacktrackingLimit",
    119,
    HE,
    classOf[Integer],
    null)

  val XPATH_VERSION_FOR_XSD: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/xpathVersionForXsd",
    120,
    EE,
    classOf[Integer],
    null)

  val XPATH_VERSION_FOR_XSLT: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/xpathVersionForXslt",
    121,
    HE,
    classOf[Integer],
    null)

  val THRESHOLD_FOR_FUNCTION_INLINING: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/thresholdForFunctionInlining",
    122,
    EE,
    classOf[Integer],
    null)

  val THRESHOLD_FOR_HOTSPOT_BYTE_CODE: Feature[Integer] = new Feature[Integer](
    "http://saxon.sf.net/feature/thresholdForHotspotByteCode",
    123,
    EE,
    classOf[Integer],
    null)

  val ALLOWED_PROTOCOLS: Feature[Any] = new Feature[Any](
    "http://saxon.sf.net/feature/allowedProtocols",
    124,
    EE,
    classOf[AnyRef],
    null)

  val RETAIN_NODE_FOR_DIAGNOSTICS: Feature[Boolean] = new Feature[Boolean](
    "http://saxon.sf.net/feature/retainNodeForDiagnostics",
    125,
    EE,
    classOf[Boolean],
    false)

  val ALLOW_UNRESOLVED_SCHEMA_COMPONENTS: Feature[Boolean] =
    new Feature[Boolean](
      "http://saxon.sf.net/feature/allowUnresolvedSchemaComponents",
      126,
      EE,
      classOf[Boolean],
      false)

}

class Feature[T] private(var name: String,
                         val code: Int,
                         var requiredEdition: Int,
                         var typeVar: Class[_ <: T],
                         var defaultValue: T) {

  index.put(name, this)
}
