////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.lib.SaxonOutputKeys

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.serialize.charcode.UTF8CharacterSet

import org.orbeon.saxon.trans.XPathException

import javax.xml.transform.OutputKeys

import java.util.regex.Pattern




class TEXTEmitter extends XMLEmitter {

  private var newlineMatcher: Pattern = null

  private var newlineRepresentation: String = null

  override def open(): Unit = ()

  override  def openDocument(): Unit = {
    if (writer == null) {
      makeWriter()
    }
    if (characterSet == null) {
      characterSet = UTF8CharacterSet.getInstance
    }
// Write a BOM if requested
    var encoding: String = outputProperties.getProperty(OutputKeys.ENCODING)
    if (encoding == null || encoding.equalsIgnoreCase("utf8")) {
      encoding = "UTF-8"
    }
    val byteOrderMark: String =
      outputProperties.getProperty(SaxonOutputKeys.BYTE_ORDER_MARK)
    val nl: String = outputProperties.getProperty(SaxonOutputKeys.NEWLINE)
    if (nl != null && nl.!=("\n")) {
      newlineRepresentation = nl
      newlineMatcher = Pattern.compile("\\n")
    }
    if ("yes" == byteOrderMark &&
        ("UTF-8".equalsIgnoreCase(encoding) || "UTF-16LE".equalsIgnoreCase(
          encoding) ||
        "UTF-16BE".equalsIgnoreCase(encoding))) {
      try writer.write('﻿')
      catch {
        case err: java.io.IOException => {}

      }
    }
    started = true
  }

  override def writeDeclaration(): Unit = ()

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {

    if (!started) {
      openDocument()
    }
    if (!ReceiverOption.contains(properties, ReceiverOption.NO_SPECIAL_CHARS)) {
      val badchar: Int = testCharacters(chars)
      if (badchar != 0) {
        throw new XPathException(
          "Output character not available in this encoding (x" +
            java.lang.Integer.toString(badchar, 16) +
            ")",
          "SERE0008")
      }
    }
    var charVar = chars
    if (newlineMatcher != null) {
      charVar = newlineMatcher.matcher(chars).replaceAll(newlineRepresentation)
    }
    writer.write(charVar.toString)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    previousAtomic = false
  }

  override def endElement(): Unit = ()
// no-op
// no-op

  override def processingInstruction(name: String,
                                     value: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = ()

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = ()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class generates TEXT output
  *
  * @author Michael H. Kay
  */
