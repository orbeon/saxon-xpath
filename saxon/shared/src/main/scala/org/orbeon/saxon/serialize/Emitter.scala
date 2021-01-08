////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.{ReceiverOption, ReceiverWithOutputProperties, SequenceReceiver}
import org.orbeon.saxon.lib.SaxonOutputKeys
import org.orbeon.saxon.om.{Item, NodeInfo}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.charcode.{CharacterSet, UTF16CharacterSet, UTF8CharacterSet}
import org.orbeon.saxon.trans.{XPathException, XmlProcessingIncident}

import java.io.{BufferedWriter, OutputStream, OutputStreamWriter, Writer}
import java.util.Properties
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import scala.util.control.Breaks._


/**
 * Emitter: This abstract class defines methods that must be implemented by
 * components that format SAXON output. There is one emitter for XML,
 * one for HTML, and so on. Additional methods are concerned with
 * setting options and providing a Writer.
 *
 * The interface is deliberately designed to be as close as possible to the
 * standard SAX2 ContentHandler interface, however, it allows additional
 * information to be made available.
 *
 * An Emitter is a Receiver, specifically it is a Receiver that can direct output
 * to a Writer or OutputStream, using serialization properties defined in a Properties
 * object.
 */
abstract class Emitter
  extends SequenceReceiver(null)
    with ReceiverWithOutputProperties {

  var streamResult           : StreamResult = _
  var writer                 : Writer       = _
  var outputStream           : OutputStream = _
  var outputProperties       : Properties   = _
  var characterSet           : CharacterSet = _
  var allCharactersEncodable : Boolean      = false

  private val mustClose = false

  def setOutputProperties(details: Properties): Unit = {
    if (characterSet == null) {
      characterSet = getConfiguration.getCharacterSetFactory.getCharacterSet(details)
      allCharactersEncodable = characterSet.isInstanceOf[UTF8CharacterSet] || characterSet.isInstanceOf[UTF16CharacterSet]
    }
    outputProperties = details
  }

  def getOutputProperties: Properties = outputProperties

  def setStreamResult(result: StreamResult): Unit = {
    streamResult = result
    if (systemId == null)
      systemId = result.getSystemId
  }

  def makeWriter(): Unit = {
    if (writer != null)
      return
    if (streamResult == null)
      throw new IllegalStateException("Emitter must have either a Writer or a StreamResult to write to")
    writer = streamResult.getWriter
    if (writer == null) {
      val os = streamResult.getOutputStream
      if (os != null)
        setOutputStream(os)
    }

    if (writer == null)
      makeOutputStream()
  }

  def makeOutputStream(): OutputStream = {

    // ORBEON: No `File` support.
    ???
//    this.outputStream = System.out
//    streamResult.setOutputStream(outputStream)
//    outputStream

//    val uriString = streamResult.getSystemId
//    if (uriString == null)
//      throw new XPathException("Result has no system ID, writer, or output stream defined", SaxonErrorCode.SXRD0004)
//
//    try {
//      val file = ExpandedStreamResult.makeWritableOutputFile(uriString)
//      this.outputStream = new FileOutputStream(file)
//      streamResult.setOutputStream(outputStream)
//      mustClose = true
//    } catch {
//      case fnf@(_: FileNotFoundException | _: URISyntaxException | _: IllegalArgumentException) =>
//        val err = new XPathException("Unable to write to output destination", fnf)
//        err.setErrorCode(SaxonErrorCode.SXRD0004)
//        throw err
//
//    }
//    outputStream
  }

  def usesWriter: Boolean = true

  def setWriter(writer: Writer): Unit = {
    this.writer = writer
    writer match {
      case writer1: OutputStreamWriter if outputProperties != null =>
        val enc: String = writer1.getEncoding
        outputProperties.setProperty(OutputKeys.ENCODING, enc)
        characterSet = getConfiguration.getCharacterSetFactory.getCharacterSet(
          outputProperties)
        allCharactersEncodable = characterSet.isInstanceOf[UTF8CharacterSet] || characterSet.isInstanceOf[UTF16CharacterSet]
      case _ =>
    }
  }

  def getWriter: Writer = writer

  def setOutputStream(stream: OutputStream): Unit = {
    outputStream = stream
    if (usesWriter) {
      if (outputProperties == null)
        outputProperties = new Properties
      var encoding = outputProperties.getProperty(OutputKeys.ENCODING)
      if (encoding == null) {
        encoding = "UTF8"
        allCharactersEncodable = true
      } else if (encoding.equalsIgnoreCase("UTF-8")) {
        encoding = "UTF8"
        allCharactersEncodable = true
      } else if (encoding.equalsIgnoreCase("UTF-16")) {
        encoding = "UTF16"
      }
      if (characterSet == null)
        characterSet = getConfiguration.getCharacterSetFactory.getCharacterSet(outputProperties)
      val byteOrderMark = outputProperties.getProperty(SaxonOutputKeys.BYTE_ORDER_MARK)
      if ("no" == byteOrderMark && "UTF16" == encoding)
        encoding = "UTF-16BE"
      else if (! characterSet.isInstanceOf[UTF8CharacterSet])
        encoding = characterSet.getCanonicalName
      breakable {
        while (true)
          try {
            var javaEncoding = encoding
            if (encoding.equalsIgnoreCase("iso-646") || encoding.equalsIgnoreCase("iso646"))
              javaEncoding = "US-ASCII"
            writer =
              if (encoding.equalsIgnoreCase("UTF8"))
                new UTF8Writer(outputStream)
              else
                new BufferedWriter(new OutputStreamWriter(outputStream, javaEncoding))
            break()
          } catch {
            case _: Exception =>
              if (encoding.equalsIgnoreCase("UTF8"))
                throw new XPathException("Failed to create a UTF8 output writer")
              val de = new XmlProcessingIncident(
                "Encoding " + encoding + " is not supported: using UTF8",
                "SESU0007")
              getPipelineConfiguration.getErrorReporter.report(de)
              encoding = "UTF8"
              characterSet = UTF8CharacterSet.getInstance
              allCharactersEncodable = true
              outputProperties.setProperty(OutputKeys.ENCODING, "UTF-8")
          }
      }
    }
  }

  def getOutputStream: OutputStream = outputStream

  override def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = ()

  def close(): Unit =
    if (mustClose && outputStream != null)
      outputStream.close()

  /**
   * Ask whether this Receiver (or the downstream pipeline) makes any use of the type annotations
   * supplied on element and attribute events
   *
   * @return true if the Receiver makes any use of this information. If false, the caller
   *         may supply untyped nodes instead of supplying the type annotation (or conversely, it may
   *         avoid stripping unwanted type annotations)
   */
  override def usesTypeAnnotations: Boolean = false

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit =
    if (item.isInstanceOf[NodeInfo])
      decompose(item, locationId, copyNamespaces)
    else
      characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
}
