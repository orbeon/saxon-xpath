package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.utils.Version

import org.orbeon.saxon.expr.PackageData

import org.orbeon.saxon.expr.StaticContext

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.trans.DecimalFormatManager

import org.orbeon.saxon.trans.XPathException

import java.net.URI

import java.net.URISyntaxException

import java.util.Iterator

import scala.beans.{BeanProperty, BooleanBeanProperty}

class RetainedStaticContext extends NamespaceResolver {

  private var config: Configuration = _

  @BeanProperty
  var packageData: PackageData = _

  private var staticBaseUri: URI = _

  var staticBaseUriString: String = _

  @BeanProperty
  var defaultCollationName: String = _

  private var namespaces: NamespaceResolver = _

  @BeanProperty
  var defaultFunctionNamespace: String = NamespaceConstant.FN

  private var defaultElementNamespace: String = _

  @BeanProperty
  var decimalFormatManager: DecimalFormatManager = _

  @BooleanBeanProperty
  var backwardsCompatibility: Boolean = _

  def this(config: Configuration) {
    this()
    this.config = config
    this.packageData = new PackageData(config)
    this.namespaces = NamespaceMap.emptyMap
    this.defaultCollationName = NamespaceConstant.CODEPOINT_COLLATION_URI
  }

  def this(sc: StaticContext) = {
    this()
    this.config = sc.getConfiguration
    this.packageData = sc.getPackageData
    if (sc.getStaticBaseURI != null) {
      staticBaseUriString = sc.getStaticBaseURI
      try this.staticBaseUri = ExpressionTool.getBaseURI(sc, null, fail = true)
      catch {
        case e: XPathException => staticBaseUri = null

      }
    }
    this.defaultCollationName = sc.getDefaultCollationName
    this.decimalFormatManager = sc.getDecimalFormatManager
    this.defaultElementNamespace = sc.getDefaultElementNamespace
    defaultFunctionNamespace = sc.getDefaultFunctionNamespace
    backwardsCompatibility = sc.isInBackwardsCompatibleMode
    if (Version.platform.JAXPStaticContextCheck(this, sc)) {} else {
      val resolver: NamespaceResolver = sc.getNamespaceResolver
      if (resolver.isInstanceOf[NamespaceMap]) {
        namespaces = resolver
      } else {
        var map: NamespaceMap = NamespaceMap.emptyMap
        var it: Iterator[String] = resolver.iteratePrefixes
        while (it.hasNext) {
          val prefix: String = it.next()
          if (prefix.!=("xml")) {
            map = map.put(prefix, resolver.getURIForPrefix(prefix, useDefault = true))
          }
        }
        namespaces = map
      }
    }
  }

  def getConfiguration: Configuration = config

  def getStaticBaseUriString: String = staticBaseUriString

  def setStaticBaseUriString(baseUri: String): Unit = {
    if (baseUri != null) {
      staticBaseUriString = baseUri
      try this.staticBaseUri = new URI(baseUri)
      catch {
        case e: URISyntaxException => staticBaseUri = null

      }
    }
  }

  def getStaticBaseUri: URI = {
    if (staticBaseUri == null) {
      if (staticBaseUriString == null) {
        return null
      } else {
        throw new XPathException(
          "Supplied static base URI " + staticBaseUriString + " is not a valid URI")
      }
    }
    staticBaseUri
  }

  def getDefaultElementNamespace: String =
    if (defaultElementNamespace == null) "" else defaultElementNamespace

  def setDefaultElementNamespace(ns: String): Unit = {
    defaultElementNamespace = ns
  }

  def declareNamespace(prefix: String, uri: String): Unit = {
    if (namespaces.isInstanceOf[NamespaceMap]) {
      namespaces = namespaces.asInstanceOf[NamespaceMap].put(prefix, uri)
    } else {
      throw new UnsupportedOperationException()
    }
  }

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    namespaces.getURIForPrefix(prefix, useDefault)

  def iteratePrefixes: Iterator[String] = namespaces.iteratePrefixes

  def declaresSameNamespaces(other: RetainedStaticContext): Boolean =
    namespaces == other.namespaces

  override def hashCode: Int = {
    var h: Int = 0x8457cbce
    if (staticBaseUriString != null) {
      h ^= staticBaseUriString.hashCode
    }
    h ^= defaultCollationName.hashCode
    h ^= defaultFunctionNamespace.hashCode
    h ^= namespaces.hashCode
    h
  }

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[RetainedStaticContext])) {
      return false
    }
    val r: RetainedStaticContext = other.asInstanceOf[RetainedStaticContext]
    ExpressionTool.equalOrNull(staticBaseUriString, r.staticBaseUriString) &&
      defaultCollationName == r.defaultCollationName &&
      defaultFunctionNamespace == r.defaultFunctionNamespace &&
      namespaces == r.namespaces
  }

  def setNamespaces(namespaces: NamespaceResolver): Unit = {
    this.namespaces = namespaces
  }

}
