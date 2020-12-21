////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.io.{IOException, OutputStream, Writer}
import java.net.{MalformedURLException, URI, URISyntaxException}

import javax.xml.transform.Result
import javax.xml.transform.stream.StreamResult
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}


/**
  * This class defines the default OutputURIResolver. This is a counterpart to the JAXP
  * URIResolver, but is used to map the URI of a secondary result document to a Result object
  * which acts as the destination for the new document.
  *
  * @author Michael H. Kay
  */

object StandardOutputResolver {
  val getInstance: StandardOutputResolver = new StandardOutputResolver
}

class StandardOutputResolver extends OutputURIResolver {

  def newInstance(): StandardOutputResolver = this

  def resolve(href: String, base: String): Result = {
    var which: String = "base"
    try {
      var absoluteURI: URI = null
      if (href.isEmpty) {
        if (base == null) {
          throw new XPathException(
            "The system identifier of the principal output file is unknown",
            SaxonErrorCode.SXRD0002)
        }
        absoluteURI = new URI(base)
      } else {
        which = "relative"
        absoluteURI = new URI(href)
      }
      if (!absoluteURI.isAbsolute) {
        if (base == null) {
          throw new XPathException(
            "The system identifier of the principal output file is unknown",
            SaxonErrorCode.SXRD0002)
        }
        which = "base"
        val baseURI: URI = new URI(base)
        which = "relative"
        absoluteURI = baseURI.resolve(href)
      }
      createResult(absoluteURI)
    } catch {
      case err: URISyntaxException =>
        val xe: XPathException = new XPathException(
          "Invalid syntax for " + which + " URI")
        xe.setErrorCode(SaxonErrorCode.SXRD0001)
        throw xe
      case err2: IllegalArgumentException =>
        val xe: XPathException = new XPathException(
          "Invalid " + which + " URI syntax")
        xe.setErrorCode(SaxonErrorCode.SXRD0001)
        throw xe
      case err3: MalformedURLException =>
        val xe: XPathException =
          new XPathException("Resolved URL is malformed", err3)
        xe.setErrorCode(SaxonErrorCode.SXRD0001)
        throw xe
      // ORBEON: JVM only
      ???
//      case err4: UnknownServiceException => {
//        val xe: XPathException =
//          new XPathException("Specified protocol does not allow output", err4)
//        xe.setErrorCode(SaxonErrorCode.SXRD0001)
//        throw xe
//      }
      case err5: IOException =>
        val xe: XPathException =
          new XPathException("Cannot open connection to specified URL", err5)
        xe.setErrorCode(SaxonErrorCode.SXRD0001)
        throw xe
    }
  }
// System.err.println("Output URI Resolver (href='" + href + "', base='" + base + "')");
// System.err.println("Output URI Resolver (href='" + href + "', base='" + base + "')");

   def createResult(absoluteURI: URI): Result =
    if ("file" == absoluteURI.getScheme) {
      // ORBEON: No `File` support.
      ???
//      StandardResultDocumentResolver.makeOutputFile(absoluteURI)
    } else {
      // ORBEON: JVM only
        ???

      // See if the Java VM can conjure up a writable URL connection for us.
      // This is optimistic: I have yet to discover a URL scheme that it can handle "out of the box".
      // But it can apparently be achieved using custom-written protocol handlers.

//      val connection: URLConnection = absoluteURI.toURL().openConnection()
//      connection.setDoInput(false)
//      connection.setDoOutput(true)
//      connection.connect()
//      val stream: OutputStream = connection.getOutputStream
//      val result: StreamResult = new StreamResult(stream)
//      result.setSystemId(absoluteURI.toASCIIString())
//      result
    }

  def close(result: Result): Unit = {
    result match {
      case result1: StreamResult =>
        val stream: OutputStream =
          result1.getOutputStream
        if (stream != null) {
          try stream.close()
          catch {
            case err: IOException =>
              val xe: XPathException =
                new XPathException("Failed while closing output file", err)
              xe.setErrorCode(SaxonErrorCode.SXRD0003)
              throw xe
          }
        }
        // Path not used, but there for safety
        val writer: Writer = result1.getWriter
        if (writer != null) {
          try writer.close()
          catch {
            case err: IOException =>
              val xe: XPathException =
                new XPathException("Failed while closing output file", err)
              xe.setErrorCode(SaxonErrorCode.SXRD0003)
              throw xe
          }
        }
      case _ =>
    }
  }
}
