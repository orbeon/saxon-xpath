////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr.{PackageData, StaticContext}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.{NamespaceMap, NamespaceResolver}
import org.orbeon.saxon.trans.{DecimalFormatManager, XPathException}
import org.orbeon.saxon.utils.Configuration

import java.net.{URI, URISyntaxException}
import java.{util => ju}
import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
 * This class contains the part of the static context of expressions that (a) can change from one expression
 * to another within a query or stylesheet, and (b) are potentially needed at run-time.
 *
 * From 9.6, the retained static context is available to every expression in the expression tree (previously,
 * expressions only retained that part of the static context needed by the particular expression). For economy,
 * a new RetainedStaticContext object is only created when the context changes: which is fairly rare (for
 * example, it never happens within an XPath expression).
 */
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
      this.staticBaseUriString = sc.getStaticBaseURI
      this.staticBaseUri =
        try
          ExpressionTool.getBaseURI(sc, null, fail = true)
        catch {
          case _: XPathException =>
            null
        }
    }
    this.defaultCollationName = sc.getDefaultCollationName
    this.decimalFormatManager = sc.getDecimalFormatManager
    this.defaultElementNamespace = sc.getDefaultElementNamespace
    this.defaultFunctionNamespace = sc.getDefaultFunctionNamespace
    this.backwardsCompatibility = sc.isInBackwardsCompatibleMode
//    if (Version.platform.JAXPStaticContextCheck(this, sc)) {} else
    locally {
      val resolver = sc.getNamespaceResolver
      this.namespaces =
        if (resolver.isInstanceOf[NamespaceMap])
          resolver
        else {
          var map = NamespaceMap.emptyMap
          val it = resolver.iteratePrefixes
          while (it.hasNext) {
            val prefix: String = it.next()
            if (prefix.!=("xml"))
              map = map.put(prefix, resolver.getURIForPrefix(prefix, useDefault = true))
          }
          map
        }
    }
  }

  def getConfiguration: Configuration = config
  def getStaticBaseUriString: String = staticBaseUriString

  def setStaticBaseUriString(baseUri: String): Unit =
    if (baseUri != null) {
      staticBaseUriString = baseUri
      staticBaseUri =
        try
          new URI(baseUri)
        catch {
          case _: URISyntaxException =>
            null
        }
    }

  def getStaticBaseUri: URI = {
    if (staticBaseUri == null)
      if (staticBaseUriString == null)
        return null
      else
        throw new XPathException("Supplied static base URI " + staticBaseUriString + " is not a valid URI")
    staticBaseUri
  }

  def getDefaultElementNamespace: String =
    if (defaultElementNamespace == null) "" else defaultElementNamespace

  def setDefaultElementNamespace(ns: String): Unit =
    defaultElementNamespace = ns

  def declareNamespace(prefix: String, uri: String): Unit =
    namespaces match {
      case nsMap: NamespaceMap => namespaces = nsMap.put(prefix, uri)
      case _                   => throw new UnsupportedOperationException()
    }

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    namespaces.getURIForPrefix(prefix, useDefault)

  def iteratePrefixes: ju.Iterator[String] =
    namespaces.iteratePrefixes

  def declaresSameNamespaces(other: RetainedStaticContext): Boolean =
    namespaces == other.namespaces

  override def hashCode: Int = {
    var h = 0x8457cbce
    if (staticBaseUriString != null)
      h ^= staticBaseUriString.hashCode
    h ^= defaultCollationName.hashCode
    h ^= defaultFunctionNamespace.hashCode
    h ^= namespaces.hashCode
    h
  }

  override def equals(other: Any): Boolean = {
    if (! other.isInstanceOf[RetainedStaticContext])
      return false
    val r = other.asInstanceOf[RetainedStaticContext]
    ExpressionTool.equalOrNull(staticBaseUriString, r.staticBaseUriString) &&
      defaultCollationName == r.defaultCollationName &&
      defaultFunctionNamespace == r.defaultFunctionNamespace &&
      namespaces == r.namespaces
  }

  def setNamespaces(namespaces: NamespaceResolver): Unit =
    this.namespaces = namespaces
}
