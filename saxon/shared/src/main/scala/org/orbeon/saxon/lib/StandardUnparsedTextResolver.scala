////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Default implementation of the UnparsedTextURIResolver, used if no other implementation
 * is nominated to the Configuration. This implementation
 *  * handles anything that the java URL class will handle, plus the <code>classpath</code>
 *  * URI scheme defined in the Spring framework, and the <code>data</code> URI scheme defined in
 *  * RFC 2397.
 */
package org.orbeon.saxon.lib

import java.io._
import java.net.URI
import java.nio.charset._

import org.orbeon.saxon.resource.{BinaryResource, DataURIScheme, UnparsedTextResource}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration


object StandardUnparsedTextResolver {

  def inferStreamEncoding(is: InputStream, err: Logger): String = {
    is.mark(100)
    val start: Array[Byte] = Array.ofDim[Byte](100)
    val read: Int = is.read(start, 0, 100)
    is.reset()
    inferEncoding(start, read, err)
  }

  private def inferEncoding(start: Array[Byte],
                            read: Int,
                            logger: Logger): String = {
    val debug: Boolean = logger != null
    if (read >= 2) {
      if (ch(start(0)) == 0xFE && ch(start(1)) == 0xFF) {
        if (debug) {
          logger.info("unparsed-text(): found UTF-16 byte order mark")
        }
        return "UTF-16"
      } else if (ch(start(0)) == 0xFF && ch(start(1)) == 0xFE) {
        if (debug) {
          logger.info("unparsed-text(): found UTF-16LE byte order mark")
        }
        return "UTF-16LE"
      }
    }
    if (read >= 3) {
      if (ch(start(0)) == 0xEF && ch(start(1)) == 0xBB && ch(start(2)) == 0xBF) {
        if (debug) {
          logger.info("unparsed-text(): found UTF-8 byte order mark")
        }
        return "UTF-8"
      }
    }
    if (read >= 4) {
      if (ch(start(0)) == '<' && ch(start(1)) == '?' && ch(start(2)) == 'x' &&
          ch(start(3)) == 'm' &&
          ch(start(4)) == 'l') {
        if (debug) {
          logger.info("unparsed-text(): found XML declaration")
        }
        val sb: FastStringBuffer = new FastStringBuffer(read)
        for (b <- 0 until read) {
          sb.cat(start(b).toChar)
        }
        val p: String = sb.toString
        var v: Int = p.indexOf("encoding")
        if (v >= 0) {
          v += 8
          while (v < p.length && " \n\r\t=\"'".indexOf(p.charAt(v)) >= 0) {
            v += 1; v - 1
          }
          sb.setLength(0)
          while (v < p.length && p.charAt(v) != '"' && p.charAt(v) != '\'') sb
            .cat(p.charAt({ v += 1; v - 1 }))
          if (debug) {
            logger.info(
              "unparsed-text(): encoding in XML declaration = " + sb.toString)
          }
          sb.toString
        }
        if (debug) {
          logger.info("unparsed-text(): no encoding found in XML declaration")
        }
      }
    } else if (read > 0 && start(0) == 0 && start(2) == 0 && start(4) == 0 &&
               start(6) == 0) {
      if (debug) {
        logger.info(
          "unparsed-text(): even-numbered bytes are zero, inferring UTF-16")
      }
      return "UTF-16"
    } else if (read > 1 && start(1) == 0 && start(3) == 0 && start(5) == 0 &&
               start(7) == 0) {
      if (debug) {
        logger.info(
          "unparsed-text(): odd-numbered bytes are zero, inferring UTF-16LE")
      }
      return "UTF-16LE"
    }
// If all else fails, assume UTF-8
    if (debug) {
      logger.info("unparsed-text(): assuming fallback encoding (UTF-8)")
    }
    "UTF-8"
  }

  private def ch(b: Byte): Int = b.toInt & 0xff

}

class StandardUnparsedTextResolver extends UnparsedTextURIResolver {

  private var debug: Boolean = false

  def setDebugging(debug: Boolean): Unit = {
    this.debug = debug
  }

  def resolve(absoluteURI: URI,
              encoding: String,
              config: Configuration): Reader = {
    val err: Logger = config.getLogger
    var encodingVar = encoding
    if (debug) {
      err.info("unparsed-text(): processing " + absoluteURI)
      err.info("unparsed-text(): requested encoding = " + encodingVar)
    }
    if (!absoluteURI.isAbsolute) {
      throw new XPathException(
        "Resolved URI supplied to unparsed-text() is not absolute: " +
          absoluteURI.toString,
        "FOUT1170")
    }
    if (!config.getAllowedUriTest.test(absoluteURI)) {
      throw new XPathException(
        "URI scheme '" + absoluteURI.getScheme + "' has been disallowed")
    }
    var inputStream: InputStream = null
    var contentEncoding: String = null
    var isXmlMediaType: Boolean = false
    if (absoluteURI.getScheme.==("data")) {
      var resource: Resource = null
      resource = DataURIScheme.decode(absoluteURI)
      resource match {
        case binaryResource: BinaryResource =>
          val octets: Array[Byte] = binaryResource.getData
          inputStream = new ByteArrayInputStream(octets)
          contentEncoding = "utf-8"
        case _ =>
          assert(resource.isInstanceOf[UnparsedTextResource])
          new StringReader(
            resource.asInstanceOf[UnparsedTextResource].getContent)
      }
      if (encodingVar == null) {
        encodingVar = contentEncoding
      }
      val mediaType: String = resource.getContentType
      isXmlMediaType = (mediaType.startsWith("application/") || mediaType
          .startsWith("text/")) &&
          (mediaType.endsWith("/xml") || mediaType.endsWith("+xml"))
    } else if (absoluteURI.getScheme.==("classpath")) {
      val is = config.getResourceAsStream(absoluteURI.toString.substring(10))
      if (is != null) {
        if (encodingVar == null)
          encodingVar = "UTF-8"
        new InputStreamReader(is, encodingVar)
      }
    } else {
      // ORBEON: JVM only
      ???
//      var absoluteURL: URL = null
//      try absoluteURL = absoluteURI.toURL
//      catch {
//        case mue: MalformedURLException =>
//          val e: XPathException = new XPathException(
//            "Cannot convert absolute URI " + absoluteURI + " to URL",
//            mue)
//          e.setErrorCode("FOUT1170")
//          throw e
//      }
//      val connection: URLConnection = absoluteURL.openConnection()
//      connection.setRequestProperty("Accept-Encoding", "gzip")
//      try connection.connect()
//      catch {
//        case ioe: IOException => {
//          if (debug) {
//            err.error(
//              "unparsed-text(): connection failure on " + absoluteURL +
//                ". " +
//                ioe.getMessage)
//          }
//          val xpe: XPathException =
//            new XPathException("Failed to read input file " + absoluteURL, ioe)
//          xpe.setErrorCode("FOUT1170")
//          throw xpe
//        }
//
//      }
//      inputStream = connection.getInputStream
//      contentEncoding = connection.getContentEncoding
//      if ("gzip" == contentEncoding) {
//        inputStream = new GZIPInputStream(inputStream)
//      }
//      if (debug) {
//        err.info(
//          "unparsed-text(): established connection " +
//            (if ("gzip" == contentEncoding) " (zipped)" else ""))
//      }
//      if (!inputStream.markSupported()) {
//        inputStream = new BufferedInputStream(inputStream)
//      }
//// Get any external (HTTP) encoding label.
//      isXmlMediaType = false
//// The file:// URL scheme gives no useful information...
//      if ("file" != connection.getURL.getProtocol) {
//// Use the contentType from the HTTP header if available
//        val contentType: String = connection.getContentType
//        if (debug) {
//          err.info("unparsed-text(): content type = " + contentType)
//        }
//        if (contentType != null) {
//          var mediaType: String = null
//          var pos: Int = contentType.indexOf(';')
//          mediaType =
//            if (pos >= 0) contentType.substring(0, pos) else contentType
//          mediaType = mediaType.trim()
//          if (debug) {
//            err.info("unparsed-text(): media type = " + mediaType)
//          }
//          isXmlMediaType = (mediaType.startsWith("application/") || mediaType
//              .startsWith("text/")) &&
//              (mediaType.endsWith("/xml") || mediaType.endsWith("+xml"))
//          var charset: String = ""
//          pos = contentType.toLowerCase().indexOf("charset")
//          if (pos >= 0) {
//            pos = contentType.indexOf('=', pos + 7)
//            if (pos >= 0) {
//              charset = contentType.substring(pos + 1)
//            }
//            pos = charset.indexOf(';')
//            if ((pos) > 0) {
//              charset = charset.substring(0, pos)
//            }
//// attributes can have comment fields (RFC 822)
//            pos = charset.indexOf('(')
//            if ((pos) > 0) {
//              charset = charset.substring(0, pos)
//            }
//// ... and values may be quoted
//            pos = charset.indexOf('"')
//            if ((pos) > 0) {
//              charset =
//                charset.substring(pos + 1, charset.indexOf('"', pos + 2))
//            }
//            if (debug) {
//              err.info("unparsed-text(): charset = " + charset.trim())
//            }
//            encodingVar = charset.trim()
//          }
//        }
//      }
//      try if (encodingVar == null || isXmlMediaType) {
//        encodingVar = inferStreamEncoding(inputStream, if (debug) err else null)
//        if (debug) {
//          err.info("unparsed-text(): inferred encoding = " + encodingVar)
//        }
//      } catch {
//        case e: IOException => encodingVar = "UTF-8"
//
//      }
    }
// The following is necessary to ensure that encoding errors are not recovered.
    val charset: Charset = Charset.forName(encodingVar)
    var decoder: CharsetDecoder = charset.newDecoder()
    decoder = decoder.onMalformedInput(CodingErrorAction.REPORT)
    decoder = decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    new BufferedReader(new InputStreamReader(inputStream, decoder))
  }
}
