////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.io.{IOException, InputStream, InputStreamReader}
import java.net.{URI, URISyntaxException}

import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.functions.ResolveURI
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration


/**
  * This class is the standard ModuleURIResolver used to implement the "import module" declaration
  * in a Query Prolog. It is used when no user-defined ModuleURIResolver has been specified, or when
  * the user-defined ModuleURIResolver decides to delegate to the standard ModuleURIResolver.
  * It relies on location hints being supplied in the "import module" declaration, and attempts
  * to locate a module by dereferencing the URI given as the location hint. It accepts standard
  * URIs recognized by the Java URL class, including the <code>jar</code> URI scheme; it also
  * accepts <code>classpath</code> URIs as defined in the Spring framework.
  *
  * @author Michael H. Kay
  */
class StandardModuleURIResolver extends ModuleURIResolver {

  var config: Configuration = _

  def this(config: Configuration) = {
    this()
    this.config = config
  }

  def resolve(moduleURI: String,
              baseURI: String,
              locations: Array[String]): Array[StreamSource] =
    if (locations.length == 0) {
      val err = new XPathException(
        "Cannot locate module for namespace " + moduleURI)
      err.setErrorCode("XQST0059")
      err.setIsStaticError(true)
      throw err
    } else {
// One or more locations given: import modules from all these locations
      val sources: Array[StreamSource] =
        Array.ofDim[StreamSource](locations.length)
      for (m <- locations.indices) {
        val href: String = locations(m)
        var absoluteURI: URI = null
        try absoluteURI = ResolveURI.makeAbsolute(href, baseURI)
        catch {
          case err: URISyntaxException =>
            val se: XPathException =
              new XPathException("Cannot resolve relative URI " + href, err)
            se.setErrorCode("XQST0059")
            se.setIsStaticError(true)
            throw se
        }
        if (!config.getAllowedUriTest.test(absoluteURI)) {
          throw new XPathException(
            "URI scheme '" + absoluteURI.getScheme + "' has been disallowed")
        }
        sources(m) = getQuerySource(absoluteURI)
      }
      sources
    }

  /*@NotNull*/

   def getQuerySource(absoluteURI: URI): StreamSource = {
    var encoding: String = null
    try {
      var is: InputStream = null
      if ("classpath" == absoluteURI.getScheme) {
        val path: String = absoluteURI.getPath
        is = config.getResourceAsStream(path)
        if (is == null) {
          val se: XPathException = new XPathException(
            "Cannot locate module " + path + " on class path")
          se.setErrorCode("XQST0059")
          se.setIsStaticError(true)
          throw se
        }
      } else {
        // ORBEON: JVM only
        ???
//        val absoluteURL: URL = absoluteURI.toURL()
//        val connection: URLConnection = absoluteURL.openConnection()
//        connection.connect()
//        is = connection.getInputStream
//        var contentType: String = null
//        if ("file" != connection.getURL.getProtocol) {
//          contentType = connection.getContentType
//          if (contentType != null) {
//            var pos: Int = contentType.indexOf("charset")
//            if (pos >= 0) {
//              pos = contentType.indexOf('=', pos + 7)
//              if (pos >= 0) {
//                contentType = contentType.substring(pos + 1)
//              }
//              pos = contentType.indexOf(';')
//              if (pos > 0) {
//                contentType = contentType.substring(0, pos)
//              }
//              pos = contentType.indexOf('(')
//              if (pos > 0) {
//                contentType = contentType.substring(0, pos)
//              }
//              pos = contentType.indexOf('"')
//              if (pos > 0) {
//                contentType = contentType.substring(
//                  pos + 1,
//                  contentType.indexOf('"', pos + 2))
//              }
//              encoding = contentType.trim()
//            }
//          }
//        }
      }
      // ORBEON: BufferedInputStream
      if (! is.markSupported())
        ???
//        is = new BufferedInputStream(is)
      val ss = new StreamSource
      if (encoding == null)
        ss.setInputStream(is)
      else
        ss.setReader(new InputStreamReader(is, encoding))
      ss.setSystemId(absoluteURI.toString)
      ss
    } catch {
      case err: IOException =>
        val se: XPathException = new XPathException(err)
        se.setErrorCode("XQST0059")
        se.setIsStaticError(true)
        throw se
    }
  }
}

