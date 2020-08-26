////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.lib.NamespaceConstant

import java.util.HashMap




object StandardNames {

  private val DFLT_NS: Int = 0

  private val XSL_NS: Int = 1

  private val SAXON_NS: Int = 2

  private val XML_NS: Int = 3

  private val XS_NS: Int = 4

  private val XSI_NS: Int = 5

//   0
  val DFLT: Int = 0

// 128
  val XSL: Int = 128

// 256
  val SAXON: Int = 128 * 2

// 384
  val XML: Int = 128 * 3

// 512
  val XS: Int = 128 * 4

// 640
  val XSI: Int = 128 * 5

  val XSL_ACCEPT: Int = XSL

  val XSL_ACCUMULATOR: Int = XSL + 1

  val XSL_ACCUMULATOR_RULE: Int = XSL + 2

  val XSL_ANALYZE_STRING: Int = XSL + 3

  val XSL_APPLY_IMPORTS: Int = XSL + 4

  val XSL_APPLY_TEMPLATES: Int = XSL + 5

  val XSL_ASSERT: Int = XSL + 6

  val XSL_ATTRIBUTE: Int = XSL + 7

  val XSL_ATTRIBUTE_SET: Int = XSL + 8

  val XSL_BREAK: Int = XSL + 9

  val XSL_CALL_TEMPLATE: Int = XSL + 10

  val XSL_CATCH: Int = XSL + 11

  val XSL_CHARACTER_MAP: Int = XSL + 13

  val XSL_CHOOSE: Int = XSL + 14

  val XSL_COMMENT: Int = XSL + 15

  val XSL_CONTEXT_ITEM: Int = XSL + 16

  val XSL_COPY: Int = XSL + 17

  val XSL_COPY_OF: Int = XSL + 18

  val XSL_DECIMAL_FORMAT: Int = XSL + 19

  val XSL_DOCUMENT: Int = XSL + 22

  val XSL_ELEMENT: Int = XSL + 23

  val XSL_EXPOSE: Int = XSL + 24

  val XSL_EVALUATE: Int = XSL + 25

  val XSL_FALLBACK: Int = XSL + 26

  val XSL_FOR_EACH: Int = XSL + 27

  val XSL_FORK: Int = XSL + 28

  val XSL_FOR_EACH_GROUP: Int = XSL + 29

  val XSL_FUNCTION: Int = XSL + 30

  val XSL_GLOBAL_CONTEXT_ITEM: Int = XSL + 31

  val XSL_IF: Int = XSL + 32

  val XSL_IMPORT: Int = XSL + 33

  val XSL_IMPORT_SCHEMA: Int = XSL + 34

  val XSL_INCLUDE: Int = XSL + 35

  val XSL_ITERATE: Int = XSL + 36

  val XSL_KEY: Int = XSL + 37

  val XSL_MAP: Int = XSL + 38

  val XSL_MAP_ENTRY: Int = XSL + 39

  val XSL_MATCHING_SUBSTRING: Int = XSL + 40

  val XSL_MERGE: Int = XSL + 41

  val XSL_MERGE_ACTION: Int = XSL + 42

  val XSL_MERGE_KEY: Int = XSL + 43

  val XSL_MERGE_SOURCE: Int = XSL + 44

  val XSL_MESSAGE: Int = XSL + 45

  val XSL_MODE: Int = XSL + 46

  val XSL_NAMESPACE: Int = XSL + 47

  val XSL_NAMESPACE_ALIAS: Int = XSL + 48

  val XSL_NEXT_ITERATION: Int = XSL + 49

  val XSL_NEXT_MATCH: Int = XSL + 50

  val XSL_NON_MATCHING_SUBSTRING: Int = XSL + 51

  val XSL_NUMBER: Int = XSL + 52

  val XSL_OTHERWISE: Int = XSL + 53

  val XSL_ON_COMPLETION: Int = XSL + 54

  val XSL_ON_EMPTY: Int = XSL + 55

  val XSL_ON_NON_EMPTY: Int = XSL + 56

  val XSL_OUTPUT: Int = XSL + 57

  val XSL_OVERRIDE: Int = XSL + 58

  val XSL_OUTPUT_CHARACTER: Int = XSL + 59

  val XSL_PACKAGE: Int = XSL + 60

  val XSL_PARAM: Int = XSL + 61

  val XSL_PERFORM_SORT: Int = XSL + 62

  val XSL_PRESERVE_SPACE: Int = XSL + 63

  val XSL_PROCESSING_INSTRUCTION: Int = XSL + 64

  val XSL_RESULT_DOCUMENT: Int = XSL + 65

  val XSL_SEQUENCE: Int = XSL + 66

  val XSL_SORT: Int = XSL + 67

  val XSL_SOURCE_DOCUMENT: Int = XSL + 68

  val XSL_STRIP_SPACE: Int = XSL + 70

  val XSL_STYLESHEET: Int = XSL + 71

  val XSL_TEMPLATE: Int = XSL + 72

  val XSL_TEXT: Int = XSL + 73

  val XSL_TRANSFORM: Int = XSL + 74

  val XSL_TRY: Int = XSL + 75

  val XSL_USE_PACKAGE: Int = XSL + 76

  val XSL_VALUE_OF: Int = XSL + 77

  val XSL_VARIABLE: Int = XSL + 78

  val XSL_WHEN: Int = XSL + 79

  val XSL_WHERE_POPULATED: Int = XSL + 80

  val XSL_WITH_PARAM: Int = XSL + 81

  val XSL_DEFAULT_COLLATION: Int = XSL + 100

  val XSL_DEFAULT_MODE: Int = XSL + 101

  val XSL_DEFAULT_VALIDATION: Int = XSL + 102

  val XSL_EXCLUDE_RESULT_PREFIXES: Int = XSL + 103

  val XSL_EXPAND_TEXT: Int = XSL + 104

  val XSL_EXTENSION_ELEMENT_PREFIXES: Int = XSL + 105

  val XSL_INHERIT_NAMESPACES: Int = XSL + 106

  val XSL_TYPE: Int = XSL + 107

  val XSL_USE_ATTRIBUTE_SETS: Int = XSL + 108

  val XSL_USE_WHEN: Int = XSL + 109

  val XSL_VALIDATION: Int = XSL + 110

  val XSL_VERSION: Int = XSL + 111

  val XSL_XPATH_DEFAULT_NAMESPACE: Int = XSL + 112

  val SAXON_ASSIGN: Int = SAXON + 1

  val SAXON_DEEP_UPDATE: Int = SAXON + 3

  val SAXON_DO: Int = SAXON + 6

  val SAXON_DOCTYPE: Int = SAXON + 7

  val SAXON_ENTITY_REF: Int = SAXON + 8

  val SAXON_TABULATE_MAPS: Int = SAXON + 9

  val SAXON_WHILE: Int = SAXON + 15

// Schema extension elements
  val SAXON_PARAM: Int = SAXON + 20

  val SAXON_PREPROCESS: Int = SAXON + 21

  val SAXON_DISTINCT: Int = SAXON + 22

  val SAXON_ORDER: Int = SAXON + 23

  private val SAXON_B: String = "{" + NamespaceConstant.SAXON + "}"

  val SAXON_ASYCHRONOUS: String = SAXON_B + "asynchronous"

  val SAXON_EXPLAIN: String = SAXON_B + "explain"

  val XML_BASE: Int = XML + 1

  val XML_SPACE: Int = XML + 2

  val XML_LANG: Int = XML + 3

  val XML_ID: Int = XML + 4

  val XML_LANG_TYPE: Int = XML + 5

  val XML_SPACE_TYPE: Int = 6

  val XML_ID_NAME: NodeName =
    new FingerprintedQName("xml", NamespaceConstant.XML, "id", XML_ID)

  val XS_STRING: Int = XS + 1

  val XS_BOOLEAN: Int = XS + 2

  val XS_DECIMAL: Int = XS + 3

  val XS_FLOAT: Int = XS + 4

  val XS_DOUBLE: Int = XS + 5

  val XS_DURATION: Int = XS + 6

  val XS_DATE_TIME: Int = XS + 7

  val XS_TIME: Int = XS + 8

  val XS_DATE: Int = XS + 9

  val XS_G_YEAR_MONTH: Int = XS + 10

  val XS_G_YEAR: Int = XS + 11

  val XS_G_MONTH_DAY: Int = XS + 12

  val XS_G_DAY: Int = XS + 13

  val XS_G_MONTH: Int = XS + 14

  val XS_HEX_BINARY: Int = XS + 15

  val XS_BASE64_BINARY: Int = XS + 16

  val XS_ANY_URI: Int = XS + 17

  val XS_QNAME: Int = XS + 18

  val XS_NOTATION: Int = XS + 19

//public static final int XS_PRECISION_DECIMAL = XS + 20;
  val XS_INTEGER: Int = XS + 21

  val XS_NON_POSITIVE_INTEGER: Int = XS + 22

  val XS_NEGATIVE_INTEGER: Int = XS + 23

  val XS_LONG: Int = XS + 24

  val XS_INT: Int = XS + 25

  val XS_SHORT: Int = XS + 26

  val XS_BYTE: Int = XS + 27

  val XS_NON_NEGATIVE_INTEGER: Int = XS + 28

  val XS_POSITIVE_INTEGER: Int = XS + 29

  val XS_UNSIGNED_LONG: Int = XS + 30

  val XS_UNSIGNED_INT: Int = XS + 31

  val XS_UNSIGNED_SHORT: Int = XS + 32

  val XS_UNSIGNED_BYTE: Int = XS + 33

  val XS_NORMALIZED_STRING: Int = XS + 41

  val XS_TOKEN: Int = XS + 42

  val XS_LANGUAGE: Int = XS + 43

  val XS_NMTOKEN: Int = XS + 44

// NB: list type
  val XS_NMTOKENS: Int = XS + 45

  val XS_NAME: Int = XS + 46

  val XS_NCNAME: Int = XS + 47

  val XS_ID: Int = XS + 48

  val XS_IDREF: Int = XS + 49

// NB: list type
  val XS_IDREFS: Int = XS + 50

  val XS_ENTITY: Int = XS + 51

// NB: list type
  val XS_ENTITIES: Int = XS + 52

  val XS_DATE_TIME_STAMP: Int = XS + 53

  val XS_ANY_TYPE: Int = XS + 60

  val XS_ANY_SIMPLE_TYPE: Int = XS + 61

  val XS_INVALID_NAME: Int = XS + 62

  val XS_ERROR: Int = XS + 63

  val XS_ALL: Int = XS + 64

  val XS_ALTERNATIVE: Int = XS + 65

  val XS_ANNOTATION: Int = XS + 66

  val XS_ANY: Int = XS + 67

  val XS_ANY_ATTRIBUTE: Int = XS + 68

  val XS_APPINFO: Int = XS + 69

  val XS_ASSERT: Int = XS + 70

  val XS_ASSERTION: Int = XS + 71

  val XS_ATTRIBUTE: Int = XS + 72

  val XS_ATTRIBUTE_GROUP: Int = XS + 73

  val XS_CHOICE: Int = XS + 74

  val XS_COMPLEX_CONTENT: Int = XS + 75

  val XS_COMPLEX_TYPE: Int = XS + 76

  val XS_DEFAULT_OPEN_CONTENT: Int = XS + 77

  val XS_DOCUMENTATION: Int = XS + 78

  val XS_ELEMENT: Int = XS + 79

  val XS_ENUMERATION: Int = XS + 80

  val XS_EXTENSION: Int = XS + 81

  val XS_FIELD: Int = XS + 82

  val XS_FRACTION_DIGITS: Int = XS + 83

  val XS_GROUP: Int = XS + 84

  val XS_IMPORT: Int = XS + 85

  val XS_INCLUDE: Int = XS + 86

  val XS_KEY: Int = XS + 87

  val XS_KEYREF: Int = XS + 88

  val XS_LENGTH: Int = XS + 89

  val XS_LIST: Int = XS + 90

  val XS_MAX_EXCLUSIVE: Int = XS + 91

  val XS_MAX_INCLUSIVE: Int = XS + 92

  val XS_MAX_LENGTH: Int = XS + 93

  val XS_MAX_SCALE: Int = XS + 94

  val XS_MIN_EXCLUSIVE: Int = XS + 95

  val XS_MIN_INCLUSIVE: Int = XS + 96

  val XS_MIN_LENGTH: Int = XS + 97

  val XS_MIN_SCALE: Int = XS + 98

  val XS_notation: Int = XS + 99

  val XS_OPEN_CONTENT: Int = XS + 100

  val XS_OVERRIDE: Int = XS + 101

  val XS_PATTERN: Int = XS + 102

  val XS_REDEFINE: Int = XS + 103

  val XS_RESTRICTION: Int = XS + 104

  val XS_SCHEMA: Int = XS + 105

  val XS_SELECTOR: Int = XS + 106

  val XS_SEQUENCE: Int = XS + 107

  val XS_SIMPLE_CONTENT: Int = XS + 108

  val XS_SIMPLE_TYPE: Int = XS + 109

  val XS_EXPLICIT_TIMEZONE: Int = XS + 110

  val XS_TOTAL_DIGITS: Int = XS + 111

  val XS_UNION: Int = XS + 112

  val XS_UNIQUE: Int = XS + 113

  val XS_WHITE_SPACE: Int = XS + 114

  val XS_UNTYPED: Int = XS + 118

  val XS_UNTYPED_ATOMIC: Int = XS + 119

  val XS_ANY_ATOMIC_TYPE: Int = XS + 120

  val XS_YEAR_MONTH_DURATION: Int = XS + 121

  val XS_DAY_TIME_DURATION: Int = XS + 122

  val XS_NUMERIC: Int = XS + 123

  val XSI_TYPE: Int = XSI + 1

  val XSI_NIL: Int = XSI + 2

  val XSI_SCHEMA_LOCATION: Int = XSI + 3

  val XSI_NO_NAMESPACE_SCHEMA_LOCATION: Int = XSI + 4

  val XSI_SCHEMA_LOCATION_TYPE: Int = XSI + 5

  private var localNames: Array[String] = new Array[String](1023)

  private var lookup: HashMap[String, Integer] = new HashMap(1023)

  var errorVariables: Array[StructuredQName] = Array(
    new StructuredQName("err", NamespaceConstant.ERR, "code"),
    new StructuredQName("err", NamespaceConstant.ERR, "description"),
    new StructuredQName("err", NamespaceConstant.ERR, "value"),
    new StructuredQName("err", NamespaceConstant.ERR, "module"),
    new StructuredQName("err", NamespaceConstant.ERR, "line-number"),
    new StructuredQName("err", NamespaceConstant.ERR, "column-number"),
    new StructuredQName("err", NamespaceConstant.ERR, "additional")
  )

  private def bindXSLTName(constant: Int, localName: String): Unit = {
    localNames(constant) = localName
    lookup.put("{" + NamespaceConstant.XSLT + "}" + localName, constant)
  }

  private def bindSaxonName(constant: Int, localName: String): Unit = {
    localNames(constant) = localName
    lookup.put("{" + NamespaceConstant.SAXON + "}" + localName, constant)
  }

  private def bindXMLName(constant: Int, localName: String): Unit = {
    localNames(constant) = localName
    lookup.put("{" + NamespaceConstant.XML + "}" + localName, constant)
  }

  private def bindXSName(constant: Int, localName: String): Unit = {
    localNames(constant) = localName
    lookup.put("{" + NamespaceConstant.SCHEMA + "}" + localName, constant)
  }

  private def bindXSIName(constant: Int, localName: String): Unit = {
    localNames(constant) = localName
    lookup.put("{" + NamespaceConstant.SCHEMA_INSTANCE + "}" + localName,
               constant)
  }

  bindXSLTName(XSL_ACCEPT, "accept")

  bindXSLTName(XSL_ACCUMULATOR, "accumulator")

  bindXSLTName(XSL_ACCUMULATOR_RULE, "accumulator-rule")

  bindXSLTName(XSL_ANALYZE_STRING, "analyze-string")

  bindXSLTName(XSL_APPLY_IMPORTS, "apply-imports")

  bindXSLTName(XSL_APPLY_TEMPLATES, "apply-templates")

  bindXSLTName(XSL_ASSERT, "assert")

  bindXSLTName(XSL_ATTRIBUTE, "attribute")

  bindXSLTName(XSL_ATTRIBUTE_SET, "attribute-set")

  bindXSLTName(XSL_BREAK, "break")

  bindXSLTName(XSL_CALL_TEMPLATE, "call-template")

  bindXSLTName(XSL_CATCH, "catch")

  bindXSLTName(XSL_CHARACTER_MAP, "character-map")

  bindXSLTName(XSL_CHOOSE, "choose")

  bindXSLTName(XSL_COMMENT, "comment")

  bindXSLTName(XSL_CONTEXT_ITEM, "context-item")

  bindXSLTName(XSL_COPY, "copy")

  bindXSLTName(XSL_COPY_OF, "copy-of")

  bindXSLTName(XSL_DECIMAL_FORMAT, "decimal-format")

  bindXSLTName(XSL_DOCUMENT, "document")

  bindXSLTName(XSL_ELEMENT, "element")

  bindXSLTName(XSL_EVALUATE, "evaluate")

  bindXSLTName(XSL_EXPOSE, "expose")

  bindXSLTName(XSL_FALLBACK, "fallback")

  bindXSLTName(XSL_FOR_EACH, "for-each")

  bindXSLTName(XSL_FOR_EACH_GROUP, "for-each-group")

  bindXSLTName(XSL_FORK, "fork")

  bindXSLTName(XSL_FUNCTION, "function")

  bindXSLTName(XSL_GLOBAL_CONTEXT_ITEM, "global-context-item")

  bindXSLTName(XSL_IF, "if")

  bindXSLTName(XSL_IMPORT, "import")

  bindXSLTName(XSL_IMPORT_SCHEMA, "import-schema")

  bindXSLTName(XSL_INCLUDE, "include")

  bindXSLTName(XSL_ITERATE, "iterate")

  bindXSLTName(XSL_KEY, "key")

  bindXSLTName(XSL_MAP, "map")

  bindXSLTName(XSL_MAP_ENTRY, "map-entry")

  bindXSLTName(XSL_MATCHING_SUBSTRING, "matching-substring")

  bindXSLTName(XSL_MERGE, "merge")

  bindXSLTName(XSL_MERGE_SOURCE, "merge-source")

  bindXSLTName(XSL_MERGE_ACTION, "merge-action")

  bindXSLTName(XSL_MERGE_KEY, "merge-key")

  bindXSLTName(XSL_MESSAGE, "message")

  bindXSLTName(XSL_MODE, "mode")

  bindXSLTName(XSL_NEXT_MATCH, "next-match")

  bindXSLTName(XSL_NUMBER, "number")

  bindXSLTName(XSL_NAMESPACE, "namespace")

  bindXSLTName(XSL_NAMESPACE_ALIAS, "namespace-alias")

  bindXSLTName(XSL_NEXT_ITERATION, "next-iteration")

  bindXSLTName(XSL_NON_MATCHING_SUBSTRING, "non-matching-substring")

  bindXSLTName(XSL_ON_COMPLETION, "on-completion")

  bindXSLTName(XSL_ON_EMPTY, "on-empty")

  bindXSLTName(XSL_ON_NON_EMPTY, "on-non-empty")

  bindXSLTName(XSL_OTHERWISE, "otherwise")

  bindXSLTName(XSL_OUTPUT, "output")

  bindXSLTName(XSL_OUTPUT_CHARACTER, "output-character")

  bindXSLTName(XSL_OVERRIDE, "override")

  bindXSLTName(XSL_PACKAGE, "package")

  bindXSLTName(XSL_PARAM, "param")

  bindXSLTName(XSL_PERFORM_SORT, "perform-sort")

  bindXSLTName(XSL_PRESERVE_SPACE, "preserve-space")

  bindXSLTName(XSL_PROCESSING_INSTRUCTION, "processing-instruction")

  bindXSLTName(XSL_RESULT_DOCUMENT, "result-document")

  bindXSLTName(XSL_SEQUENCE, "sequence")

  bindXSLTName(XSL_SORT, "sort")

  bindXSLTName(XSL_SOURCE_DOCUMENT, "source-document")

  bindXSLTName(XSL_STRIP_SPACE, "strip-space")

  bindXSLTName(XSL_STYLESHEET, "stylesheet")

  bindXSLTName(XSL_TEMPLATE, "template")

  bindXSLTName(XSL_TEXT, "text")

  bindXSLTName(XSL_TRANSFORM, "transform")

  bindXSLTName(XSL_TRY, "try")

  bindXSLTName(XSL_USE_PACKAGE, "use-package")

  bindXSLTName(XSL_VALUE_OF, "value-of")

  bindXSLTName(XSL_VARIABLE, "variable")

  bindXSLTName(XSL_WITH_PARAM, "with-param")

  bindXSLTName(XSL_WHEN, "when")

  bindXSLTName(XSL_WHERE_POPULATED, "where-populated")

  bindXSLTName(XSL_DEFAULT_COLLATION, "default-collation")

  bindXSLTName(XSL_DEFAULT_MODE, "default-mode")

  bindXSLTName(XSL_DEFAULT_VALIDATION, "default-validation")

  bindXSLTName(XSL_EXPAND_TEXT, "expand-text")

  bindXSLTName(XSL_EXCLUDE_RESULT_PREFIXES, "exclude-result-prefixes")

  bindXSLTName(XSL_EXTENSION_ELEMENT_PREFIXES, "extension-element-prefixes")

  bindXSLTName(XSL_INHERIT_NAMESPACES, "inherit-namespaces")

  bindXSLTName(XSL_TYPE, "type")

  bindXSLTName(XSL_USE_ATTRIBUTE_SETS, "use-attribute-sets")

  bindXSLTName(XSL_USE_WHEN, "use-when")

  bindXSLTName(XSL_VALIDATION, "validation")

  bindXSLTName(XSL_VERSION, "version")

  bindXSLTName(XSL_XPATH_DEFAULT_NAMESPACE, "xpath-default-namespace")

  bindSaxonName(SAXON_ASSIGN, "assign")

  bindSaxonName(SAXON_DEEP_UPDATE, "deep-update")

  bindSaxonName(SAXON_DISTINCT, "distinct")

  bindSaxonName(SAXON_DO, "do")

  bindSaxonName(SAXON_DOCTYPE, "doctype")

  bindSaxonName(SAXON_ENTITY_REF, "entity-ref")

  bindSaxonName(SAXON_ORDER, "order")

  bindSaxonName(SAXON_WHILE, "while")

  bindSaxonName(SAXON_PARAM, "param")

  bindSaxonName(SAXON_PREPROCESS, "preprocess")

  bindXMLName(XML_BASE, "base")

  bindXMLName(XML_SPACE, "space")

  bindXMLName(XML_LANG, "lang")

  bindXMLName(XML_ID, "id")

  bindXMLName(XML_LANG_TYPE, "_langType")

  bindXMLName(XML_SPACE_TYPE, "_spaceType")

  bindXSName(XS_STRING, "string")

  bindXSName(XS_BOOLEAN, "boolean")

  bindXSName(XS_DECIMAL, "decimal")

  bindXSName(XS_FLOAT, "float")

  bindXSName(XS_DOUBLE, "double")

  bindXSName(XS_DURATION, "duration")

  bindXSName(XS_DATE_TIME, "dateTime")

  bindXSName(XS_TIME, "time")

  bindXSName(XS_DATE, "date")

  bindXSName(XS_G_YEAR_MONTH, "gYearMonth")

  bindXSName(XS_G_YEAR, "gYear")

  bindXSName(XS_G_MONTH_DAY, "gMonthDay")

  bindXSName(XS_G_DAY, "gDay")

  bindXSName(XS_G_MONTH, "gMonth")

  bindXSName(XS_HEX_BINARY, "hexBinary")

  bindXSName(XS_BASE64_BINARY, "base64Binary")

  bindXSName(XS_ANY_URI, "anyURI")

  bindXSName(XS_QNAME, "QName")

  bindXSName(XS_NOTATION, "NOTATION")

  bindXSName(XS_NUMERIC, "numeric")

  bindXSName(XS_INTEGER, "integer")

  bindXSName(XS_NON_POSITIVE_INTEGER, "nonPositiveInteger")

  bindXSName(XS_NEGATIVE_INTEGER, "negativeInteger")

  bindXSName(XS_LONG, "long")

  bindXSName(XS_INT, "int")

  bindXSName(XS_SHORT, "short")

  bindXSName(XS_BYTE, "byte")

  bindXSName(XS_NON_NEGATIVE_INTEGER, "nonNegativeInteger")

  bindXSName(XS_POSITIVE_INTEGER, "positiveInteger")

  bindXSName(XS_UNSIGNED_LONG, "unsignedLong")

  bindXSName(XS_UNSIGNED_INT, "unsignedInt")

  bindXSName(XS_UNSIGNED_SHORT, "unsignedShort")

  bindXSName(XS_UNSIGNED_BYTE, "unsignedByte")

  bindXSName(XS_NORMALIZED_STRING, "normalizedString")

  bindXSName(XS_TOKEN, "token")

  bindXSName(XS_LANGUAGE, "language")

  bindXSName(XS_NMTOKEN, "NMTOKEN")

// NB: list type
  bindXSName(XS_NMTOKENS, "NMTOKENS")

  bindXSName(XS_NAME, "Name")

  bindXSName(XS_NCNAME, "NCName")

  bindXSName(XS_ID, "ID")

  bindXSName(XS_IDREF, "IDREF")

// NB: list type
  bindXSName(XS_IDREFS, "IDREFS")

  bindXSName(XS_ENTITY, "ENTITY")

// NB: list type
  bindXSName(XS_ENTITIES, "ENTITIES")

  bindXSName(XS_DATE_TIME_STAMP, "dateTimeStamp")

  bindXSName(XS_ANY_TYPE, "anyType")

  bindXSName(XS_ANY_SIMPLE_TYPE, "anySimpleType")

  bindXSName(XS_INVALID_NAME, "invalidName")

  bindXSName(XS_ERROR, "error")

  bindXSName(XS_ALL, "all")

  bindXSName(XS_ALTERNATIVE, "alternative")

  bindXSName(XS_ANNOTATION, "annotation")

  bindXSName(XS_ANY, "any")

  bindXSName(XS_ANY_ATTRIBUTE, "anyAttribute")

  bindXSName(XS_APPINFO, "appinfo")

  bindXSName(XS_ASSERT, "assert")

  bindXSName(XS_ASSERTION, "assertion")

  bindXSName(XS_ATTRIBUTE, "attribute")

  bindXSName(XS_ATTRIBUTE_GROUP, "attributeGroup")

  bindXSName(XS_CHOICE, "choice")

  bindXSName(XS_COMPLEX_CONTENT, "complexContent")

  bindXSName(XS_COMPLEX_TYPE, "complexType")

  bindXSName(XS_DEFAULT_OPEN_CONTENT, "defaultOpenContent")

  bindXSName(XS_DOCUMENTATION, "documentation")

  bindXSName(XS_ELEMENT, "element")

  bindXSName(XS_ENUMERATION, "enumeration")

  bindXSName(XS_EXPLICIT_TIMEZONE, "explicitTimezone")

  bindXSName(XS_EXTENSION, "extension")

  bindXSName(XS_FIELD, "field")

  bindXSName(XS_FRACTION_DIGITS, "fractionDigits")

  bindXSName(XS_GROUP, "group")

  bindXSName(XS_IMPORT, "import")

  bindXSName(XS_INCLUDE, "include")

  bindXSName(XS_KEY, "key")

  bindXSName(XS_KEYREF, "keyref")

  bindXSName(XS_LENGTH, "length")

  bindXSName(XS_LIST, "list")

  bindXSName(XS_MAX_EXCLUSIVE, "maxExclusive")

  bindXSName(XS_MAX_INCLUSIVE, "maxInclusive")

  bindXSName(XS_MAX_LENGTH, "maxLength")

  bindXSName(XS_MAX_SCALE, "maxScale")

  bindXSName(XS_MIN_EXCLUSIVE, "minExclusive")

  bindXSName(XS_MIN_INCLUSIVE, "minInclusive")

  bindXSName(XS_MIN_LENGTH, "minLength")

  bindXSName(XS_MIN_SCALE, "minScale")

  bindXSName(XS_notation, "notation")

  bindXSName(XS_OPEN_CONTENT, "openContent")

  bindXSName(XS_OVERRIDE, "override")

  bindXSName(XS_PATTERN, "pattern")

  bindXSName(XS_REDEFINE, "redefine")

  bindXSName(XS_RESTRICTION, "restriction")

  bindXSName(XS_SCHEMA, "schema")

  bindXSName(XS_SELECTOR, "selector")

  bindXSName(XS_SEQUENCE, "sequence")

  bindXSName(XS_SIMPLE_CONTENT, "simpleContent")

  bindXSName(XS_SIMPLE_TYPE, "simpleType")

  bindXSName(XS_TOTAL_DIGITS, "totalDigits")

  bindXSName(XS_UNION, "union")

  bindXSName(XS_UNIQUE, "unique")

  bindXSName(XS_WHITE_SPACE, "whiteSpace")

  bindXSName(XS_UNTYPED, "untyped")

  bindXSName(XS_UNTYPED_ATOMIC, "untypedAtomic")

  bindXSName(XS_ANY_ATOMIC_TYPE, "anyAtomicType")

  bindXSName(XS_YEAR_MONTH_DURATION, "yearMonthDuration")

  bindXSName(XS_DAY_TIME_DURATION, "dayTimeDuration")

  bindXSIName(XSI_TYPE, "type")

  bindXSIName(XSI_NIL, "nil")

  bindXSIName(XSI_SCHEMA_LOCATION, "schemaLocation")

  bindXSIName(XSI_NO_NAMESPACE_SCHEMA_LOCATION, "noNamespaceSchemaLocation")

  bindXSIName(XSI_SCHEMA_LOCATION_TYPE, "anonymous_schemaLocationType")

  def getFingerprint(uri: String, localName: String): Int = {
    val fp: java.lang.Integer = lookup.get("{" + uri + "}" + localName)
    if (fp == null) {
      -1
    } else {
      fp
    }
  }

  def getLocalName(fingerprint: Int): String = localNames(fingerprint)

  /*@NotNull*/

  def getURI(fingerprint: Int): String = {
    val c: Int = fingerprint >> 7
    c match {
      case DFLT_NS => ""
      case XSL_NS => NamespaceConstant.XSLT
      case SAXON_NS => NamespaceConstant.SAXON
      case XML_NS => NamespaceConstant.XML
      case XS_NS => NamespaceConstant.SCHEMA
      case XSI_NS => NamespaceConstant.SCHEMA_INSTANCE
      case _ =>
        throw new IllegalArgumentException(
          "Unknown system fingerprint " + fingerprint)

    }
  }

  def getClarkName(fingerprint: Int): String = {
    val uri: String = getURI(fingerprint)
    if (uri.isEmpty) {
      getLocalName(fingerprint)
    } else {
      "{" + uri + "}" + getLocalName(fingerprint)
    }
  }

  def getPrefix(fingerprint: Int): String = {
    val c: Int = fingerprint >> 7
    c match {
      case DFLT_NS => ""
      case XSL_NS => "xsl"
      case SAXON_NS => "saxon"
      case XML_NS => "xml"
      case XS_NS => "xs"
      case XSI_NS => "xsi"
      case _ => null

    }
  }

  def getDisplayName(fingerprint: Int): String = {
    if (fingerprint == -1) {
      "(anonymous type)"
    }
    if (fingerprint > 1023) {
      "(" + fingerprint + ')'
    }
    if ((fingerprint >> 7) == DFLT) {
      getLocalName(fingerprint)
    }
    getPrefix(fingerprint) + ':' + getLocalName(fingerprint)
  }

  def getStructuredQName(fingerprint: Int): StructuredQName =
    new StructuredQName(getPrefix(fingerprint),
                        getURI(fingerprint),
                        getLocalName(fingerprint))

  def getUnprefixedQName(fingerprint: Int): StructuredQName =
    new StructuredQName("", getURI(fingerprint), getLocalName(fingerprint))

  val SQ_XS_INVALID_NAME: StructuredQName = getStructuredQName(XS_INVALID_NAME)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Well-known names used in XSLT processing. These names must all have
  * fingerprints in the range 0-1023, to avoid clashing with codes allocated
  * in a NamePool. We use the top three bits for the namespace, and the bottom
  * seven bits for the local name.
  * <p>Codes in the range 0-100 are used for standard node kinds such as ELEMENT,
  * DOCUMENT, etc, and for built-in types such as ITEM and EMPTY.</p>
  */
