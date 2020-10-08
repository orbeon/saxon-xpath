//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Copyright (c) 2018-2020 Saxonica Limited
//// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
//// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
//// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//package org.orbeon.saxon.lib
//
//import java.io.{File, OutputStream}
//import java.net._
//
//import javax.xml.transform.Result
//import javax.xml.transform.stream.StreamResult
//import org.orbeon.saxon.event.{PipelineConfiguration, Receiver}
//import org.orbeon.saxon.expr.XPathContext
//import org.orbeon.saxon.serialize.SerializationProperties
//import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
//
//
///**
//  * This class defines the default ResultDocumentResolver. This is a counterpart to the JAXP
//  * URIResolver, but is used to map the URI of a secondary result document to a Receiver object
//  * which acts as the destination for the new document.
//  */
//object StandardResultDocumentResolver {
//
//  private val theInstance: StandardResultDocumentResolver =
//    new StandardResultDocumentResolver()
//
//  def getInstance: StandardResultDocumentResolver = theInstance
//
//  def makeOutputFile(absoluteURI: URI): StreamResult = synchronized {
//    val outputFile: File = new File(absoluteURI)
//    if (outputFile.isDirectory)
//      throw new XPathException("Cannot write to a directory: " + absoluteURI,
//        SaxonErrorCode.SXRD0004)
//    if (outputFile.exists() && !outputFile.canWrite)
//      throw new XPathException("Cannot write to URI " + absoluteURI, SaxonErrorCode.SXRD0004)
//    new StreamResult(outputFile)
//  }
//}
//
//class StandardResultDocumentResolver extends ResultDocumentResolver {
//
//  def resolve(context: XPathContext,
//                       href: String,
//                       baseUri: String,
//                       properties: SerializationProperties): Receiver = {
//    val result: StreamResult = resolve(href, baseUri)
//    val factory: SerializerFactory =
//      context.getConfiguration.getSerializerFactory
//    val pipe: PipelineConfiguration =
//      context.getController.makePipelineConfiguration
//    factory.getReceiver(result, properties, pipe)
//  }
//
//  def resolve(href: String, base: String): StreamResult = {
//    var which = "base"
//    var absoluteURI: URI = null
//    if (href.isEmpty) {
//      if (base == null)
//        throw new XPathException(
//          "The system identifier of the principal output file is unknown")
//
//      absoluteURI = new URI(base)
//    } else {
//      which = "relative"
//      absoluteURI = new URI(href)
//    }
//    if (! absoluteURI.isAbsolute) {
//      if (base == null)
//        throw new XPathException(
//          "The system identifier of the principal output file is unknown")
//      which = "base"
//      val baseURI = new URI(base)
//      which = "relative"
//      absoluteURI = baseURI.resolve(href)
//    }
//    createResult(absoluteURI)
//  }
//// System.err.println("Output URI Resolver (href='" + href + "', base='" + base + "')");
//// System.err.println("Output URI Resolver (href='" + href + "', base='" + base + "')");
//
//   def createResult(absoluteURI: URI): StreamResult =
//    if ("file" == absoluteURI.getScheme) {
//      StandardResultDocumentResolver.makeOutputFile(absoluteURI)
//    } else {
//
//      // See if the Java VM can conjure up a writable URL connection for us.
//      // This is optimistic: I have yet to discover a URL scheme that it can handle "out of the box".
//      // But it can apparently be achieved using custom-written protocol handlers.
//
//      val connection: URLConnection = absoluteURI.toURL().openConnection()
//      connection.setDoInput(false)
//      connection.setDoOutput(true)
//      connection.connect()
//      val stream: OutputStream = connection.getOutputStream
//      val result: StreamResult = new StreamResult(stream)
//      result.setSystemId(absoluteURI.toASCIIString())
//      result
//    }
//
//  def close(result: Result): Unit = {
//    result match {
//      case streamResult: StreamResult =>
//        val stream = streamResult.getOutputStream
//        if (stream != null)
//          stream.close()
//
//        // Path not used, but there for safety
//        val writer = streamResult.getWriter
//        if (writer != null)
//          writer.close()
//      case _ =>
//    }
//  }
//}
