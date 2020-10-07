////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.Err
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.AnyURIValue
import org.orbeon.saxon.value.AtomicValue
import java.io.File
import java.net.MalformedURLException
import java.net.URI
import java.net.URISyntaxException
import java.net.URL

/**
 * This class supports the resolve-uri() function in XPath 2.0
 */
object ResolveURI {
  /**
   * If a system ID can't be parsed as a URL, try to expand it as a relative
   * URI using the current directory as the base URI.
   *
   * @param systemId the supplied systemId. Null is treated as equivalent to ""
   * @return the systemId itself if it is a valid URL; otherwise the result of resolving
   *         the systemId as a relative file name in the current working directory; or if the
   *         current working directory is not available (e.g. in an applet) the supplied systemId
   *         unchanged (except that null is treated as "").
   */
  /*@NotNull*/ def tryToExpand(/*@Nullable*/ systemId: String): String = {
    var sysId = systemId
    if (sysId == null) sysId = ""
    try {
      new URL(sysId)
      sysId // all is well
    } catch {
      case err: MalformedURLException =>
        var dir: String = null
        try dir = System.getProperty("user.dir")
        catch {
          case geterr: Exception =>
            // this doesn't work when running an applet
            return sysId
        }
        if (!(dir.endsWith("/") || sysId.startsWith("/"))) dir = dir + '/'
        try {
          val currentDirectoryURI = new File(dir).toURI
          val baseURI = currentDirectoryURI.resolve(sysId)
          baseURI.toString
        } catch {
          case e: Exception =>
            sysId
        }
    }
  }

  /**
   * Construct an absolute URI from a relative URI and a base URI. The method uses the resolve
   * method of the java.net.URI class, except where the base URI uses the (non-standard) "jar:" scheme,
   * in which case the method used is <code>new URL(baseURL, relativeURL)</code>.
   * <p>Spaces in either URI are converted to %20</p>
   * <p>If no base URI is available, and the relative URI is not an absolute URI, then the current
   * directory is used as a base URI.</p>
   *
   * @param relativeURI the relative URI. Null is permitted provided that the base URI is an absolute URI
   * @param base        the base URI. Null is permitted provided that relativeURI is an absolute URI
   * @return the absolutized URI
   * @throws java.net.URISyntaxException if either of the strings is not a valid URI or
   *                                     if the resolution fails
   */
  @throws[URISyntaxException]
  def makeAbsolute(relativeURI: String, base: String): URI = {
    var baseStr = base
    var absoluteURI: URI = null
    if (relativeURI == null) {
      if (baseStr == null) throw new URISyntaxException("", "Relative and Base URI must not both be null")
      absoluteURI = new URI(ResolveURI.escapeSpaces(baseStr))
      if (!absoluteURI.isAbsolute) throw new URISyntaxException(baseStr, "Relative URI not supplied, so base URI must be absolute")
      else return absoluteURI
    }
    try if (baseStr == null || baseStr.isEmpty) {
      absoluteURI = new URI(relativeURI)
      if (!absoluteURI.isAbsolute) {
        val expandedBase = ResolveURI.tryToExpand(baseStr)
        if (!(expandedBase == baseStr)) { // prevent infinite recursion
          return makeAbsolute(relativeURI, expandedBase)
        }
      }
    }
    else if (baseStr.startsWith("jar:") || baseStr.startsWith("file:////")) {
      try {
        val baseURL = new URL(baseStr)
        val absoluteURL = new URL(baseURL, relativeURI)
        absoluteURI = absoluteURL.toURI
      } catch {
        case err: MalformedURLException =>
          throw new URISyntaxException(baseStr + " " + relativeURI, err.getMessage)
      }
    }
    else if (baseStr.startsWith("classpath:")) {
      absoluteURI = new URI(relativeURI)
      if (!absoluteURI.isAbsolute) absoluteURI = new URI("classpath:" + relativeURI)
    }
    else {
      var baseURI: URI = null
      try baseURI = new URI(baseStr)
      catch {
        case e: URISyntaxException =>
          throw new URISyntaxException(baseStr, "Invalid base URI: " + e.getMessage)
      }
      if (baseURI.getFragment != null) {
        val hash = base.indexOf('#')
        if (hash >= 0) baseStr = baseStr.substring(0, hash)
        try baseURI = new URI(baseStr)
        catch {
          case e: URISyntaxException =>
            throw new URISyntaxException(baseStr, "Invalid base URI: " + e.getMessage)
        }
      }
      try new URI(relativeURI) // for validation only
      catch {
        case e: URISyntaxException =>
          throw new URISyntaxException(baseStr, "Invalid relative URI: " + e.getMessage)
      }
      absoluteURI = if (relativeURI.isEmpty) baseURI
      else baseURI.resolve(relativeURI)
    }
    catch {
      case err0: IllegalArgumentException =>
        // can be thrown by resolve() when given a bad URI
        throw new URISyntaxException(relativeURI, "Cannot resolve URI against base " + Err.wrap(baseStr))
    }
    absoluteURI
  }

  /**
   * Replace spaces by %20
   *
   * @param s the input string
   * @return the input string with each space replaced by %20
   */
  def escapeSpaces(s: String): String = { // It's not entirely clear why we have to escape spaces by hand, and not other special characters;
    // it's just that tests with a variety of filenames show that this approach seems to work.
    val i = s.indexOf(' ')
    if (i < 0) return s
    (if (i == 0) ""
    else s.substring(0, i)) + "%20" + (if (i == s.length - 1) ""
    else escapeSpaces(s.substring(i + 1)))
  }

  /**
   * Replace %20 by space
   *
   * @param uri the input uri
   * @return the input URI with each %20 replaced by space
   */
  def unescapeSpaces(uri: String) = uri.replace("%20", " ")
}

class ResolveURI extends SystemFunction {
  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val arg0 = arguments(0).head.asInstanceOf[AtomicValue]
    if (arg0 == null) return ZeroOrOne.empty
    val relative = arg0.getStringValue
    var base: String = null
    if (getArity == 2) { //noinspection ConstantConditions
      base = arguments(1).head.getStringValue
    }
    else {
      base = getStaticBaseUriString
      if (base == null) throw new XPathException("Base URI in static context of resolve-uri() is unknown", "FONS0005", context)
    }
    new ZeroOrOne[Item](resolve(base, relative, context))
  }

  @throws[XPathException]
  private def resolve(base: String, relative: String, context: XPathContext): AnyURIValue = {
    var escaped = false
    var relStr = relative
    var baseStr = base
    if (relStr.contains(" ")) {
      relStr = ResolveURI.escapeSpaces(relStr)
      escaped = true
    }
    if (baseStr.contains(" ")) {
      baseStr = ResolveURI.escapeSpaces(baseStr)
      escaped = true
    }
    var relativeURI: URI = null
    try relativeURI = new URI(relative)
    catch {
      case e: URISyntaxException =>
        throw new XPathException("Relative URI " + Err.wrap(relStr) + " is invalid: " + e.getMessage, "FORG0002", context)
    }
    if (relativeURI.isAbsolute) return new AnyURIValue(relStr)
    var absoluteURI: URI = null
    try absoluteURI = new URI(baseStr)
    catch {
      case e: URISyntaxException =>
        throw new XPathException("Base URI " + Err.wrap(baseStr) + " is invalid: " + e.getMessage, "FORG0002", context)
    }
    if (!absoluteURI.isAbsolute) throw new XPathException("Base URI " + Err.wrap(baseStr) + " is not an absolute URI", "FORG0002", context)
    if (absoluteURI.isOpaque && !baseStr.startsWith("jar:")) { // Special-case JAR file URLs, even though non-conformant
      throw new XPathException("Base URI " + Err.wrap(baseStr) + " is a non-hierarchic URI", "FORG0002", context)
    }
    if (absoluteURI.getRawFragment != null)
      throw new XPathException("Base URI " + Err.wrap(baseStr) + " contains a fragment identifier", "FORG0002", context)
    if (!baseStr.startsWith("jar:") && absoluteURI.getPath != null && absoluteURI.getPath.isEmpty) {
      // This deals with cases like base=http://www.example.com - changing it to http://www.example.com/
      try absoluteURI = new URI(absoluteURI.getScheme, absoluteURI.getUserInfo, absoluteURI.getHost, absoluteURI.getPort, "/", absoluteURI.getQuery, absoluteURI.getFragment)
      catch {
        case e: URISyntaxException =>
          throw new XPathException("Failed to parse JAR scheme URI " + Err.wrap(absoluteURI.toASCIIString), "FORG0002", context)
      }
      baseStr = absoluteURI.toString
    }
    var resolved: URI = null
    try resolved = ResolveURI.makeAbsolute(relStr, baseStr)
    catch {
      case e: URISyntaxException =>
        throw new XPathException(e.getMessage, "FORG0002")
    }
    if (!resolved.toASCIIString.startsWith("file:////")) resolved = resolved.normalize
    var result = if (escaped) ResolveURI.unescapeSpaces(resolved.toString)
    else resolved.toString
    while ( {
      result.endsWith("..")
    }) result = result.substring(0, result.length - 2)
    while ( {
      result.endsWith("../")
    }) result = result.substring(0, result.length - 3)
    new AnyURIValue(result)
  }
}