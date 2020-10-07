////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.query

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.SequenceCopier

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.lib.SerializerFactory

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.CopyOptions

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.serialize.SerializationProperties

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.SingletonIterator

import org.orbeon.saxon.tree.tiny.TinyBuilder

import javax.xml.transform.Result

import javax.xml.transform.stream.StreamResult

import java.io._

import java.net.URI

import java.net.URISyntaxException

import java.util.Properties


object QueryResult {

  /*@NotNull*/

  var RESULT_NS: String = "http://saxon.sf.net/xquery-results"

  def serialize(nodeInfo: NodeInfo): String = {
    val sw: StringWriter = new StringWriter()
    val props: Properties = new Properties()
    props.setProperty("method", "xml")
    props.setProperty("indent", "yes")
    props.setProperty("omit-xml-declaration", "yes")
    serialize(nodeInfo, new StreamResult(sw), props)
    sw.toString
  }

  /*@Nullable*/

  def wrap(iterator: SequenceIterator, config: Configuration): NodeInfo = {
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    val builder: TinyBuilder = new TinyBuilder(pipe)
    builder.setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
    sendWrappedSequence(iterator, builder)
    builder.getCurrentRoot
  }

  def sendWrappedSequence(iterator: SequenceIterator,
                          destination: Receiver): Unit = {
    val sf: SerializerFactory =
      destination.getPipelineConfiguration.getConfiguration.getSerializerFactory
    SequenceCopier.copySequence(iterator, sf.newSequenceWrapper(destination))
  }

  def serialize(node: NodeInfo,
                destination: Result,
                outputProperties: Properties): Unit = {
    val config: Configuration = node.getConfiguration
    serializeSequence(SingletonIterator.makeIterator(node),
      config,
      destination,
      outputProperties)
  }

  def serialize(node: NodeInfo,
                destination: Result,
                properties: SerializationProperties): Unit = {
    val config: Configuration = node.getConfiguration
    serializeSequence(SingletonIterator.makeIterator(node),
      config,
      destination,
      properties)
  }

  def serializeSequence(iterator: SequenceIterator,
                        config: Configuration,
                        destination: OutputStream,
                        outputProps: Properties): Unit = {
    serializeSequence(iterator,
      config,
      new StreamResult(destination),
      outputProps)
    destination.flush()
  }

  def serializeSequence(iterator: SequenceIterator,
                        config: Configuration,
                        writer: Writer,
                        outputProps: Properties): Unit = {
    serializeSequence(iterator, config, new StreamResult(writer), outputProps)
    writer.flush()
  }

  def serializeSequence(iterator: SequenceIterator,
                        config: Configuration,
                        result: Result,
                        outputProperties: Properties): Unit = {
    val sf: SerializerFactory = config.getSerializerFactory
    val tr: Receiver =
      sf.getReceiver(result, new SerializationProperties(outputProperties))
    SequenceCopier.copySequence(iterator, tr)
  }

  def serializeSequence(iterator: SequenceIterator,
                        config: Configuration,
                        result: Result,
                        properties: SerializationProperties): Unit = {
    val sf: SerializerFactory = config.getSerializerFactory
    val tr: Receiver = sf.getReceiver(result, properties)
    SequenceCopier.copySequence(iterator, tr)
  }

  def rewriteToDisk(doc: NodeInfo,
                    outputProperties: Properties,
                    backup: Boolean,
                    log: PrintStream): Unit = {
    doc.getNodeKind match {
      case Type.DOCUMENT =>
      case Type.ELEMENT =>
        var parent: NodeInfo = doc.getParent
        if (parent != null && parent.getNodeKind != Type.DOCUMENT) {
          throw new XPathException(
            "Cannot rewrite an element node unless it is top-level")
        }
      case _ =>
        throw new XPathException(
          "Node to be rewritten must be a document or element node")

    }
    val uri: String = doc.getSystemId
    if (uri == null || uri.isEmpty) {
      throw new XPathException("Cannot rewrite a document with no known URI")
    }
    var u: URI = null
    u = new URI(uri)
    val existingFile: File = new File(u)
    val dir: File = existingFile.getParentFile
    if (backup && existingFile.exists()) {
      val backupFile: File = new File(dir, existingFile.getName + ".bak")
      if (log != null) {
        log.println("Creating backup file " + backupFile)
      }
      val success: Boolean = existingFile.renameTo(backupFile)
      if (!success) {
        throw new XPathException(
          "Failed to create backup file of " + backupFile)
      }
    }
    if (!existingFile.exists()) {
      if (log != null) {
        log.println("Creating file " + existingFile)
      }
      existingFile.createNewFile()
    } else {
      if (log != null) {
        log.println("Overwriting file " + existingFile)
      }
    }
    val config: Configuration = doc.getConfiguration
    val factory: SerializerFactory = config.getSerializerFactory
    val r: Receiver = factory.getReceiver(
      new StreamResult(existingFile),
      new SerializationProperties(outputProperties))
    doc.copy(r, CopyOptions.ALL_NAMESPACES, Loc.NONE)
    r.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
