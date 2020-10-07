package org.orbeon.saxon.query

import org.orbeon.saxon.functions.UnparsedTextFunction

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.Whitespace

import javax.xml.transform.stream.StreamSource

import java.io._

import java.util.function.IntPredicate

object QueryReader {

  def readSourceQuery(ss: StreamSource, charChecker: IntPredicate): String = {
    var queryText: CharSequence = null
    if (ss.getInputStream != null) {
      var is: InputStream = ss.getInputStream
      if (!is.markSupported()) {
        is = new BufferedInputStream(is)
      }
      val encoding: String = readEncoding(is)
      queryText = readInputStream(is, encoding, charChecker)
    } else if (ss.getReader != null) {
      queryText = readQueryFromReader(ss.getReader, charChecker)
    } else {
      throw new XPathException(
        "Module URI Resolver must supply either an InputStream or a Reader")
    }
    queryText.toString
  }

  def readEncoding(is: InputStream): String = {
    if (!is.markSupported()) {
      throw new IllegalArgumentException(
        "InputStream must have markSupported() = true")
    }
    is.mark(100)
    val start: Array[Byte] = Array.ofDim[Byte](100)
    val read: Int = is.read(start, 0, 100)
    if (read == -1) {
      throw new XPathException("Query source file is empty")
    }
    is.reset()
    inferEncoding(start, read)
  }

  def readInputStream(is: InputStream,
                      encoding: String,
                      nameChecker: IntPredicate): String = {
    var encodeStr = encoding
    var inStream = is
    if (encodeStr == null) {
      if (!inStream.markSupported()) {
        inStream = new BufferedInputStream(inStream)
      }
      encodeStr = readEncoding(inStream)
    }
    try {
      val reader: Reader = new BufferedReader(
        new InputStreamReader(inStream, encodeStr))
      readQueryFromReader(reader, nameChecker)
    } catch {
      case encErr: UnsupportedEncodingException => {
        val err =
          new XPathException("Unknown encoding " + Err.wrap(encodeStr), encErr)
        err.setErrorCode("XQST0087")
        throw err
      }

    }
  }

  private def readQueryFromReader(reader: Reader,
                                  charChecker: IntPredicate): String =
    try {
      val content: CharSequence =
        UnparsedTextFunction.readFile(charChecker, reader)
      content.toString
    } catch {
      case err: XPathException => {
        err.setErrorCode("XPST0003")
        err.setIsStaticError(true)
        throw err
      }

      case ioErr: IOException =>
        throw new XPathException("Failed to read supplied query file", ioErr)

    }

  private def inferEncoding(start: Array[Byte], read: Int): String = {
    if (read >= 2) {
      if (ch(start(0)) == 0xFE && ch(start(1)) == 0xFF) {
        return "UTF-16"
      } else if (ch(start(0)) == 0xFF && ch(start(1)) == 0xFE) {
        return "UTF-16LE"
      }
    }
    if (read >= 3) {
      if (ch(start(0)) == 0xEF && ch(start(1)) == 0xBB && ch(start(2)) == 0xBF) {
        return "UTF-8"
      }
    }
    if (read >= 8 && start(0) == 0 && start(2) == 0 && start(4) == 0 &&
      start(6) == 0) {
      return "UTF-16"
    }
    if (read >= 8 && start(1) == 0 && start(3) == 0 && start(5) == 0 &&
      start(7) == 0) {
      return "UTF-16LE"
    }
    var i: Int = 0
    var tok: String = readToken(start, i, read)
    if (Whitespace.trim(tok).==("xquery")) {
      i += tok.length
    } else {
      return "UTF-8"
    }
    tok = readToken(start, i, read)
    if (Whitespace.trim(tok).==("encoding")) {
      i += tok.length
    } else {
      if (Whitespace.trim(tok).==("version")) {
        i += tok.length
      } else {
        return "UTF-8"
      }
      tok = readToken(start, i, read)
      i += tok.length
      tok = readToken(start, i, read)
      if (Whitespace.trim(tok).==("encoding")) {
        i += tok.length
      } else {
        return "UTF-8"
      }
    }
    tok = Whitespace.trim(readToken(start, i, read))
    if (tok.startsWith("\"") && tok.endsWith("\"") && tok.length > 2) {
      tok.substring(1, tok.length - 1)
    } else if (tok.startsWith("'") && tok.endsWith("'") && tok.length > 2) {
      tok.substring(1, tok.length - 1)
    } else {
      throw new XPathException(
        "Unrecognized encoding " + Err.wrap(tok) + " in query prolog")
    }
  }

  private def readToken(in: Array[Byte], i: Int, len: Int): String = {
    var p: Int = i
    while (p < len && " \n\r\t".indexOf(ch(in(p))) >= 0) {
      p += 1;
      p - 1
    }
    if (ch(in(p)) == '"') {
      p += 1
      while (p < len && ch(in(p)) != '"') {
        p += 1
      }
    } else if (ch(in(p)) == '\'') {
        p += 1
      while (p < len && ch(in(p)) != '\'') {
        p += 1
      }
    } else {
      while (p < len && " \n\r\t".indexOf(ch(in(p))) < 0) {
        p += 1
      }
    }
    if (p >= len) {
      new String(in, i, len - i)
    }
    val sb: FastStringBuffer = new FastStringBuffer(p - i + 1)
    var c: Int = i
    while (c <= p) {
      sb.cat(ch(in(c)).toChar)
      c += 1
    }
    sb.toString
  }

  private def ch(b: Byte): Int = b.toInt & 0xff

}
