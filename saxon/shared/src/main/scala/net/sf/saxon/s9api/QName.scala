package net.sf.saxon.s9api

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import java.util.Objects


object QName {

  def fromClarkName(expandedName: String): QName = {
    var namespaceURI: String = null
    var localName: String = null
    if (expandedName == null || expandedName.isEmpty) {
      throw new IllegalArgumentException(
        "Supplied Clark name is null or empty")
    }
    if (expandedName.charAt(0) == '{') {
      val closeBrace: Int = expandedName.indexOf('}')
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in Clark name")
      }
      namespaceURI = expandedName.substring(1, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in Clark name")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespaceURI = ""
      localName = expandedName
    }
    new QName("", namespaceURI, localName)
  }

  def fromEQName(expandedName: String): QName = {
    var namespaceURI: String = null
    var localName: String = null
    if (expandedName.charAt(0) == 'Q' && expandedName.charAt(1) == '{') {
      val closeBrace: Int = expandedName.indexOf('}')
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in EQName")
      }
      namespaceURI = expandedName.substring(2, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in EQName")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespaceURI = ""
      localName = expandedName
    }
    new QName("", namespaceURI, localName)
  }

  val XS_STRING: QName = new QName("xs", NamespaceConstant.SCHEMA, "string")

  val XS_BOOLEAN: QName = new QName("xs", NamespaceConstant.SCHEMA, "boolean")

  val XS_DECIMAL: QName = new QName("xs", NamespaceConstant.SCHEMA, "decimal")

  val XS_FLOAT: QName = new QName("xs", NamespaceConstant.SCHEMA, "float")

  val XS_DOUBLE: QName = new QName("xs", NamespaceConstant.SCHEMA, "double")

  val XS_DURATION: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "duration")

  val XS_DATE_TIME: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "dateTime")

  val XS_TIME: QName = new QName("xs", NamespaceConstant.SCHEMA, "time")

  val XS_DATE: QName = new QName("xs", NamespaceConstant.SCHEMA, "date")

  val XS_G_YEAR_MONTH: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "gYearMonth")

  val XS_G_YEAR: QName = new QName("xs", NamespaceConstant.SCHEMA, "gYear")

  val XS_G_MONTH_DAY: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "gMonthDay")

  val XS_G_DAY: QName = new QName("xs", NamespaceConstant.SCHEMA, "gDay")

  val XS_G_MONTH: QName = new QName("xs", NamespaceConstant.SCHEMA, "gMonth")

  val XS_HEX_BINARY: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "hexBinary")

  val XS_BASE64_BINARY: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "base64Binary")

  val XS_ANY_URI: QName = new QName("xs", NamespaceConstant.SCHEMA, "anyURI")

  val XS_QNAME: QName = new QName("xs", NamespaceConstant.SCHEMA, "QName")

  val XS_NOTATION: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "NOTATION")

  val XS_INTEGER: QName = new QName("xs", NamespaceConstant.SCHEMA, "integer")

  val XS_NON_POSITIVE_INTEGER: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "nonPositiveInteger")

  val XS_NEGATIVE_INTEGER: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "negativeInteger")

  val XS_LONG: QName = new QName("xs", NamespaceConstant.SCHEMA, "long")

  val XS_INT: QName = new QName("xs", NamespaceConstant.SCHEMA, "int")

  val XS_SHORT: QName = new QName("xs", NamespaceConstant.SCHEMA, "short")

  val XS_BYTE: QName = new QName("xs", NamespaceConstant.SCHEMA, "byte")

  val XS_NON_NEGATIVE_INTEGER: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "nonNegativeInteger")

  val XS_POSITIVE_INTEGER: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "positiveInteger")

  val XS_UNSIGNED_LONG: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "unsignedLong")

  val XS_UNSIGNED_INT: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "unsignedInt")

  val XS_UNSIGNED_SHORT: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "unsignedShort")

  val XS_UNSIGNED_BYTE: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "unsignedByte")

  val XS_NORMALIZED_STRING: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "normalizedString")

  val XS_TOKEN: QName = new QName("xs", NamespaceConstant.SCHEMA, "token")

  val XS_LANGUAGE: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "language")

  val XS_NMTOKEN: QName = new QName("xs", NamespaceConstant.SCHEMA, "NMTOKEN")

  val XS_NMTOKENS: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "NMTOKENS")

  val XS_NAME: QName = new QName("xs", NamespaceConstant.SCHEMA, "Name")

  val XS_NCNAME: QName = new QName("xs", NamespaceConstant.SCHEMA, "NCName")

  val XS_ID: QName = new QName("xs", NamespaceConstant.SCHEMA, "ID")

  val XS_IDREF: QName = new QName("xs", NamespaceConstant.SCHEMA, "IDREF")

  val XS_IDREFS: QName = new QName("xs", NamespaceConstant.SCHEMA, "IDREFS")

  val XS_ENTITY: QName = new QName("xs", NamespaceConstant.SCHEMA, "ENTITY")

  val XS_ENTITIES: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "ENTITIES")

  val XS_UNTYPED: QName = new QName("xs", NamespaceConstant.SCHEMA, "untyped")

  val XS_UNTYPED_ATOMIC: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "untypedAtomic")

  val XS_ANY_ATOMIC_TYPE: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "anyAtomicType")

  val XS_YEAR_MONTH_DURATION: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "yearMonthDuration")

  val XS_DAY_TIME_DURATION: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "dayTimeDuration")

  val XS_DATE_TIME_STAMP: QName =
    new QName("xs", NamespaceConstant.SCHEMA, "dateTimeStamp")

}

class QName(prefix: String, uri: String, localName: String) {

  private var sqName: StructuredQName =
    new StructuredQName(prefix, uri, localName)

  def this(uri: String, lexical: String) = {
    this("", uri, "")
    var uriStr = uri
    uriStr = (if (uriStr == null) "" else uriStr)
    val colon: Int = lexical.indexOf(':')
    if (colon < 0) {
      sqName = new StructuredQName("", uriStr, lexical)
    } else {
      val prefix: String = lexical.substring(0, colon)
      val local: String = lexical.substring(colon + 1)
      sqName = new StructuredQName(prefix, uriStr, local)
    }
  }

  def this(localName: String) = {
    this("", "", localName)
    val colon: Int = localName.indexOf(':')
    if (colon < 0) {
      sqName = new StructuredQName("", "", localName)
    } else {
      throw new IllegalArgumentException("Local name contains a colon")
    }
  }

  def this(lexicalQName: String, element: XdmNode) = {
    this("", "", "")
    var lexName = lexicalQName
    if (lexName.startsWith("{")) {
      lexName = "Q" + lexName
    }
    val node: NodeInfo = element.getUnderlyingValue.asInstanceOf[NodeInfo]
    sqName = StructuredQName.fromLexicalQName(lexName,
      true,
      true,
      node.getAllNamespaces)
  }

  def this(qName: javax.xml.namespace.QName) = {
    this("", "", "")
    sqName = new StructuredQName(qName.getPrefix,
      qName.getNamespaceURI,
      qName.getLocalPart)
  }

  def this(sqName: StructuredQName) = {
    this("")
    this.sqName = Objects.requireNonNull(sqName)
  }

  def isValid(processor: Processor): Boolean = {
    val prefix: String = getPrefix
    if (prefix.length > 0) {
      if (!NameChecker.isValidNCName(prefix)) {
        false
      }
    }
    NameChecker.isValidNCName(getLocalName)
  }

  def getPrefix(): String = sqName.getPrefix

  def getNamespaceURI(): String = sqName.getURI

  def getLocalName(): String = sqName.getLocalPart

  def getClarkName(): String = {
    val uri: String = getNamespaceURI
    if (uri.isEmpty) {
      getLocalName
    } else {
      "{" + uri + "}" + getLocalName
    }
  }

  def getEQName(): String = {
    val uri: String = getNamespaceURI
    if (uri.length == 0) {
      getLocalName
    } else {
      "Q{" + uri + "}" + getLocalName
    }
  }

  override def toString(): String = sqName.getDisplayName

  override def hashCode(): Int = sqName.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: QName => sqName == other.sqName
    case _ => false

  }

  def getStructuredQName(): StructuredQName = sqName

}
