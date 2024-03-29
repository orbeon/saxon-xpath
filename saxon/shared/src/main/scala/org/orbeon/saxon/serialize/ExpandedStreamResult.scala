//package org.orbeon.saxon.serialize
//
//import java.io._
//import java.net.URI
//import java.util.Properties
//
//import javax.xml.transform.OutputKeys
//import javax.xml.transform.stream.StreamResult
//import org.orbeon.saxon.lib.SaxonOutputKeys
//import org.orbeon.saxon.serialize.ExpandedStreamResult._
//import org.orbeon.saxon.serialize.charcode.{CharacterSet, UTF16CharacterSet, UTF8CharacterSet}
//import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
//import org.orbeon.saxon.utils.Configuration
//
//import scala.beans.BeanProperty
//
//
//object ExpandedStreamResult {
//
//  def makeWritableOutputFile(uriString: String): File = {
//    var uri = new URI(uriString)
//    if (! uri.isAbsolute) {
//      try uri = new File(uriString).getAbsoluteFile.toURI
//      catch {
//        case _: Exception =>
//      }
//    }
//    val file: File = new File(uri)
//    if ("file" == uri.getScheme && !file.exists()) {
//      val directory: File = file.getParentFile
//      if (directory != null && !directory.exists()) {
//        directory.mkdirs()
//      }
//      file.createNewFile()
//    }
//    if (file.isDirectory)
//      throw new XPathException("Cannot write to a directory: " + uriString, SaxonErrorCode.SXRD0004)
//    if (! file.canWrite)
//      throw new XPathException("Cannot write to URI " + uriString, SaxonErrorCode.SXRD0004)
//    file
//  }
//
//}
//
//class ExpandedStreamResult(private var config: Configuration,
//                           var result: StreamResult,
//                           var outputProperties: Properties) {
//
//  private val systemId: String = result.getSystemId
//  var writer: Writer = result.getWriter
//  @BeanProperty
//  var outputStream: OutputStream = result.getOutputStream
//  @BeanProperty
//  var characterSet: CharacterSet = _
//  private var encoding: String = outputProperties.getProperty(OutputKeys.ENCODING)
//  private var mustClose: Boolean = _
//  private var allCharactersEncodable: Boolean = _
//
//  def getWriter: Writer = writer
//
//  if (encoding == null) {
//    encoding = "UTF8"
//    allCharactersEncodable = true
//  } else if (encoding.equalsIgnoreCase("UTF-8")) {
//    encoding = "UTF8"
//    allCharactersEncodable = true
//  } else if (encoding.equalsIgnoreCase("UTF-16")) {
//    encoding = "UTF16"
//  }
//
//  if (characterSet == null) {
//    characterSet = config.getCharacterSetFactory.getCharacterSet(encoding)
//  }
//
//  val byteOrderMark: String =
//    outputProperties.getProperty(SaxonOutputKeys.BYTE_ORDER_MARK)
//
//  if ("no" == byteOrderMark && "UTF16" == encoding) {
//    encoding = "UTF-16BE"
//  } else if (! characterSet.isInstanceOf[UTF8CharacterSet]) {
//    encoding = characterSet.getCanonicalName
//  }
//
//  def obtainWriter(): Writer =
//    if (writer != null) {
//      writer
//    } else {
//      val os: OutputStream = obtainOutputStream()
//      writer = makeWriterFromOutputStream(os)
//      writer
//    }
//
//   def obtainOutputStream(): OutputStream = {
//    if (outputStream != null) {
//      return outputStream
//    }
//    val uriString: String = systemId
//    if (uriString == null) {
//      throw new XPathException(
//        "Result has no system ID, writer, or output stream defined")
//    }
//    val file: File = makeWritableOutputFile(uriString)
//    outputStream = new FileOutputStream(file)
//    mustClose = true
//    outputStream
//  }
//
//  def usesWriter(): Boolean = true
//
//  def setWriter(writer: Writer): Unit = {
//    this.writer = writer
//    writer match {
//      case osWriter: OutputStreamWriter if outputProperties != null =>
//        val enc = osWriter.getEncoding
//        outputProperties.setProperty(OutputKeys.ENCODING, enc)
//        characterSet = config.getCharacterSetFactory.getCharacterSet(outputProperties)
//        allCharactersEncodable = characterSet
//          .isInstanceOf[UTF8CharacterSet] || characterSet
//          .isInstanceOf[UTF16CharacterSet]
//      case _ =>
//    }
//  }
//
//  private def makeWriterFromOutputStream(stream: OutputStream): Writer = {
//    outputStream = stream
//    try {
//      var javaEncoding: String = encoding
//      if (encoding.equalsIgnoreCase("iso-646") || encoding.equalsIgnoreCase(
//        "iso646")) {
//        javaEncoding = "US-ASCII"
//      }
//      writer =
//        if (encoding.equalsIgnoreCase("UTF8")) new UTF8Writer(outputStream)
//        else
//          new BufferedWriter(
//            new OutputStreamWriter(outputStream, javaEncoding))
//      writer
//    } catch {
//      case _: Exception =>
//        if (encoding.equalsIgnoreCase("UTF8"))
//          throw new XPathException("Failed to create a UTF8 output writer")
//        throw new XPathException("Encoding " + encoding + " is not supported", "SESU0007")
//    }
//  }
//
//}
