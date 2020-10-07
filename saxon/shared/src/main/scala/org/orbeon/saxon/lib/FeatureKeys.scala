////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib




object FeatureKeys {

  val ALLOW_EXTERNAL_FUNCTIONS: String =
    "http://saxon.sf.net/feature/allow-external-functions"

  val ALLOW_MULTITHREADING: String =
    "http://saxon.sf.net/feature/allow-multithreading"

  val ALLOW_OLD_JAVA_URI_FORMAT: String =
    "http://saxon.sf.net/feature/allow-old-java-uri-format"

  val ALLOW_SYNTAX_EXTENSIONS: String =
    "http://saxon.sf.net/feature/allowSyntaxExtensions"

  val ASSERTIONS_CAN_SEE_COMMENTS: String =
    "http://saxon.sf.net/feature/assertionsCanSeeComments"

  val COLLATION_URI_RESOLVER: String =
    "http://saxon.sf.net/feature/collation-uri-resolver"

  val COLLATION_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/collation-uri-resolver-class"

  val COLLECTION_FINDER: String =
    "http://saxon.sf.net/feature/collection-finder"

  val COLLECTION_FINDER_CLASS: String =
    "http://saxon.sf.net/feature/collection-finder-class"

  val COMPILE_WITH_TRACING: String =
    "http://saxon.sf.net/feature/compile-with-tracing"

  val CONFIGURATION: String = "http://saxon.sf.net/feature/configuration"

  val CONFIGURATION_FILE: String =
    "http://saxon.sf.net/feature/configuration-file"

  val DEBUG_BYTE_CODE: String = "http://saxon.sf.net/feature/debugByteCode"

  val DEBUG_BYTE_CODE_DIR: String =
    "http://saxon.sf.net/feature/debugByteCodeDir"

  val DEFAULT_COLLATION: String =
    "http://saxon.sf.net/feature/defaultCollation"

  val DEFAULT_COLLECTION: String =
    "http://saxon.sf.net/feature/defaultCollection"

  val DEFAULT_COUNTRY: String = "http://saxon.sf.net/feature/defaultCountry"

  val DEFAULT_LANGUAGE: String = "http://saxon.sf.net/feature/defaultLanguage"

  val DEFAULT_REGEX_ENGINE: String =
    "http://saxon.sf.net/feature/defaultRegexEngine"

  val DISABLE_XSL_EVALUATE: String =
    "http://saxon.sf.net/feature/disableXslEvaluate"

  val DISPLAY_BYTE_CODE: String = "http://saxon.sf.net/feature/displayByteCode"

  val DTD_VALIDATION: String = "http://saxon.sf.net/feature/validation"

  val DTD_VALIDATION_RECOVERABLE: String =
    "http://saxon.sf.net/feature/dtd-validation-recoverable"

  val EAGER_EVALUATION: String = "http://saxon.sf.net/feature/eagerEvaluation"

  val ENTITY_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/entityResolverClass"

  val ENVIRONMENT_VARIABLE_RESOLVER: String =
    "http://saxon.sf.net/feature/environmentVariableResolver"

  val ENVIRONMENT_VARIABLE_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/environmentVariableResolverClass"

  val ERROR_LISTENER_CLASS: String =
    "http://saxon.sf.net/feature/errorListenerClass"

  val EXPAND_ATTRIBUTE_DEFAULTS: String =
    "http://saxon.sf.net/feature/expandAttributeDefaults"

  val EXPATH_FILE_DELETE_TEMPORARY_FILES: String =
    "http://saxon.sf.net/feature/expathFileDeleteTemporaryFiles"

  val GENERATE_BYTE_CODE: String =
    "http://saxon.sf.net/feature/generateByteCode"

  val IGNORE_SAX_SOURCE_PARSER: String =
    "http://saxon.sf.net/feature/ignoreSAXSourceParser"

  val IMPLICIT_SCHEMA_IMPORTS: String =
    "http://saxon.sf.net/feature/implicitSchemaImports"

  val LAZY_CONSTRUCTION_MODE: String =
    "http://saxon.sf.net/feature/lazyConstructionMode"

  val LICENSE_FILE_LOCATION: String =
    "http://saxon.sf.net/feature/licenseFileLocation"

  val LINE_NUMBERING: String = "http://saxon.sf.net/feature/linenumbering"

  val MARK_DEFAULTED_ATTRIBUTES: String =
    "http://saxon.sf.net/feature/markDefaultedAttributes"

  val MAX_COMPILED_CLASSES: String =
    "http://saxon.sf.net/feature/maxCompiledClasses"

  val MESSAGE_EMITTER_CLASS: String =
    "http://saxon.sf.net/feature/messageEmitterClass"

  val MODULE_URI_RESOLVER: String =
    "http://saxon.sf.net/feature/moduleURIResolver"

  val MODULE_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/moduleURIResolverClass"

  val MONITOR_HOT_SPOT_BYTE_CODE: String =
    "http://saxon.sf.net/feature/monitorHotSpotByteCode"

  val MULTIPLE_SCHEMA_IMPORTS: String =
    "http://saxon.sf.net/feature/multipleSchemaImports"

  val NAME_POOL: String = "http://saxon.sf.net/feature/namePool"

  val OCCURRENCE_LIMITS: String =
    "http://saxon.sf.net/feature/occurrenceLimits"

  val OPTIMIZATION_LEVEL: String =
    "http://saxon.sf.net/feature/optimizationLevel"

  val OUTPUT_URI_RESOLVER: String =
    "http://saxon.sf.net/feature/outputURIResolver"

  val OUTPUT_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/outputURIResolverClass"

  val PRE_EVALUATE_DOC_FUNCTION: String =
    "http://saxon.sf.net/feature/preEvaluateDocFunction"

  val PREFER_JAXP_PARSER: String =
    "http://saxon.sf.net/feature/preferJaxpParser"

  val RECOGNIZE_URI_QUERY_PARAMETERS: String =
    "http://saxon.sf.net/feature/recognize-uri-query-parameters"

  val RECOVERY_POLICY: String = "http://saxon.sf.net/feature/recoveryPolicy"

  val RECOVERY_POLICY_NAME: String =
    "http://saxon.sf.net/feature/recoveryPolicyName"

  val RESULT_DOCUMENT_THREADS: String =
    "http://saxon.sf.net/feature/resultDocumentThreads"

  val RETAIN_DTD_ATTRIBUTE_TYPES: String =
    "http://saxon.sf.net/feature/retain-dtd-attribute-types"

  val SCHEMA_URI_RESOLVER: String =
    "http://saxon.sf.net/feature/schemaURIResolver"

  val SCHEMA_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/schemaURIResolverClass"

  val SCHEMA_VALIDATION: String =
    "http://saxon.sf.net/feature/schema-validation"

  val SCHEMA_VALIDATION_MODE: String =
    "http://saxon.sf.net/feature/schema-validation-mode"

  val SERIALIZER_FACTORY_CLASS: String =
    "http://saxon.sf.net/feature/serializerFactoryClass"

  val SOURCE_PARSER_CLASS: String =
    "http://saxon.sf.net/feature/sourceParserClass"

  val SOURCE_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/sourceResolverClass"

  val STABLE_COLLECTION_URI: String =
    "http://saxon.sf.net/feature/stableCollectionUri"

  val STABLE_UNPARSED_TEXT: String =
    "http://saxon.sf.net/feature/stableUnparsedText"

  val STANDARD_ERROR_OUTPUT_FILE: String =
    "http://saxon.sf.net/feature/standardErrorOutputFile"

  val STREAMABILITY: String = "http://saxon.sf.net/feature/streamability"

  val STRICT_STREAMABILITY: String =
    "http://saxon.sf.net/feature/strictStreamability"

  val STREAMING_FALLBACK: String =
    "http://saxon.sf.net/feature/streamingFallback"

  val STRIP_WHITESPACE: String = "http://saxon.sf.net/feature/strip-whitespace"

  val STYLE_PARSER_CLASS: String =
    "http://saxon.sf.net/feature/styleParserClass"

  val SUPPRESS_EVALUATION_EXPIRY_WARNING: String =
    "http://saxon.sf.net/feature/suppressEvaluationExpiryWarning"

  val SUPPRESS_XPATH_WARNINGS: String =
    "http://saxon.sf.net/feature/suppressXPathWarnings"

  val SUPPRESS_XSLT_NAMESPACE_CHECK: String =
    "http://saxon.sf.net/feature/suppressXsltNamespaceCheck"

  val THRESHOLD_FOR_COMPILING_TYPES: String =
    "http://saxon.sf.net/feature/thresholdForCompilingTypes"

  val TIMING: String = "http://saxon.sf.net/feature/timing"

  val TRACE_EXTERNAL_FUNCTIONS: String =
    "http://saxon.sf.net/feature/trace-external-functions"

  val TRACE_LISTENER: String = "http://saxon.sf.net/feature/traceListener"

  val TRACE_LISTENER_CLASS: String =
    "http://saxon.sf.net/feature/traceListenerClass"

  val TRACE_LISTENER_OUTPUT_FILE: String =
    "http://saxon.sf.net/feature/traceListenerOutputFile"

  val TRACE_OPTIMIZER_DECISIONS: String =
    "http://saxon.sf.net/feature/trace-optimizer-decisions"

  val TREE_MODEL: String = "http://saxon.sf.net/feature/treeModel"

  val TREE_MODEL_NAME: String = "http://saxon.sf.net/feature/treeModelName"

  val UNPARSED_TEXT_URI_RESOLVER: String =
    "http://saxon.sf.net/feature/unparsedTextURIResolver"

  val UNPARSED_TEXT_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/unparsedTextURIResolverClass"

  val URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/uriResolverClass"

  val USE_PI_DISABLE_OUTPUT_ESCAPING: String =
    "http://saxon.sf.net/feature/use-pi-disable-output-escaping"

  val USE_TYPED_VALUE_CACHE: String =
    "http://saxon.sf.net/feature/use-typed-value-cache"

  val USE_XSI_SCHEMA_LOCATION: String =
    "http://saxon.sf.net/feature/useXsiSchemaLocation"

  val VALIDATION_COMMENTS: String =
    "http://saxon.sf.net/feature/validation-comments"

  val VALIDATION_WARNINGS: String =
    "http://saxon.sf.net/feature/validation-warnings"

  val VERSION_WARNING: String = "http://saxon.sf.net/feature/version-warning"

  val XINCLUDE: String = "http://saxon.sf.net/feature/xinclude-aware"

  val XML_VERSION: String = "http://saxon.sf.net/feature/xml-version"

  val XML_PARSER_FEATURE: String =
    "http://saxon.sf.net/feature/parserFeature?uri="

  val XML_PARSER_PROPERTY: String =
    "http://saxon.sf.net/feature/parserProperty?uri="

  val XQUERY_ALLOW_UPDATE: String =
    "http://saxon.sf.net/feature/xqueryAllowUpdate"

  val XQUERY_CONSTRUCTION_MODE: String =
    "http://saxon.sf.net/feature/xqueryConstructionMode"

  val XQUERY_DEFAULT_ELEMENT_NAMESPACE: String =
    "http://saxon.sf.net/feature/xqueryDefaultElementNamespace"

  val XQUERY_DEFAULT_FUNCTION_NAMESPACE: String =
    "http://saxon.sf.net/feature/xqueryDefaultFunctionNamespace"

  val XQUERY_EMPTY_LEAST: String =
    "http://saxon.sf.net/feature/xqueryEmptyLeast"

  val XQUERY_INHERIT_NAMESPACES: String =
    "http://saxon.sf.net/feature/xqueryInheritNamespaces"

  val XQUERY_MULTIPLE_MODULE_IMPORTS: String =
    "http://saxon.sf.net/feature/xqueryMultipleModuleImports"

  val XQUERY_PRESERVE_BOUNDARY_SPACE: String =
    "http://saxon.sf.net/feature/xqueryPreserveBoundarySpace"

  val XQUERY_PRESERVE_NAMESPACES: String =
    "http://saxon.sf.net/feature/xqueryPreserveNamespaces"

  val XQUERY_REQUIRED_CONTEXT_ITEM_TYPE: String =
    "http://saxon.sf.net/feature/xqueryRequiredContextItemType"

  val XQUERY_SCHEMA_AWARE: String =
    "http://saxon.sf.net/feature/xquerySchemaAware"

  val XQUERY_STATIC_ERROR_LISTENER_CLASS: String =
    "http://saxon.sf.net/feature/xqueryStaticErrorListenerClass"

  val XQUERY_VERSION: String = "http://saxon.sf.net/feature/xqueryVersion"

  val XSD_VERSION: String = "http://saxon.sf.net/feature/xsd-version"

  val XSLT_ENABLE_ASSERTIONS: String =
    "http://saxon.sf.net/feature/enableAssertions"

  val XSLT_INITIAL_MODE: String = "http://saxon.sf.net/feature/initialMode"

  val XSLT_INITIAL_TEMPLATE: String =
    "http://saxon.sf.net/feature/initialTemplate"

  val XSLT_SCHEMA_AWARE: String = "http://saxon.sf.net/feature/xsltSchemaAware"

  val XSLT_STATIC_ERROR_LISTENER_CLASS: String =
    "http://saxon.sf.net/feature/stylesheetErrorListener"

  val XSLT_STATIC_URI_RESOLVER_CLASS: String =
    "http://saxon.sf.net/feature/stylesheetURIResolver"

  val XSLT_VERSION: String = "http://saxon.sf.net/feature/xsltVersion"

  val REGEX_BACKTRACKING_LIMIT: String =
    "http://saxon.sf.net/feature/regexBacktrackingLimit"

  val XPATH_VERSION_FOR_XSD: String =
    "http://saxon.sf.net/feature/xpathVersionForXsd"

  val XPATH_VERSION_FOR_XSLT: String =
    "http://saxon.sf.net/feature/xpathVersionForXslt"

  val THRESHOLD_FOR_FUNCTION_INLINING: String =
    "http://saxon.sf.net/feature/thresholdForFunctionInlining"

  val THRESHOLD_FOR_HOTSPOT_BYTE_CODE: String =
    "http://saxon.sf.net/feature/thresholdForHotspotByteCode"

  val ALLOWED_PROTOCOLS: String =
    "http://saxon.sf.net/feature/allowedProtocols"

  val RETAIN_NODE_FOR_DIAGNOSTICS: String =
    "http://saxon.sf.net/feature/retainNodeForDiagnostics"

  val ALLOW_UNRESOLVED_SCHEMA_COMPONENTS: String =
    "http://saxon.sf.net/feature/allowUnresolvedSchemaComponents"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AUTO-GENERATED FROM FeatureKeys.xml - DO NOT EDIT THIS FILE
/**
  * FeatureKeys defines a set of constants, representing the names of Saxon configuration
  * options which can be supplied to the Saxon implementations of the JAXP
  * interfaces TransformerFactory, SchemaFactory, Validator, and ValidationHandler,
  * and to other interfaces such as the s9api {@link org.orbeon.saxon.s9api.Processor}
  *
  * @author Michael H. Kay
  */
