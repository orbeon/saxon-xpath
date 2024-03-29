////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.lib.ErrorReporter

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.trans.XmlProcessingIncident

import java.io.StringWriter




class MessageWarner extends XMLEmitter {

  private var abort: Boolean = false

  private var errorCode: String = null

  override def startDocument(properties: Int): Unit = {
    this.writer = new StringWriter()
    abort = ReceiverOption.contains(properties, ReceiverOption.TERMINATE)
    super.startDocument(properties)
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (target.==("error-code")) {
      errorCode = data.toString
    } else {
      super.processingInstruction(target, data, locationId, properties)
    }
  }

  override def endDocument(): Unit = {
    val reporter: ErrorReporter = getPipelineConfiguration.getErrorReporter
    var de: XmlProcessingIncident = new XmlProcessingIncident(
      getWriter.toString,
      if (errorCode == null) "XTMM9000" else errorCode)
    if (!abort) {
      de = de.asWarning()
    }
    reporter.report(de)
  }

  override def close(): Unit = ()
// do nothing
// do nothing

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * MessageWarner is a user-selectable receiver for XSLT xsl:message output. It causes xsl:message output
  * to be notified to the warning() method of the JAXP ErrorListener, or to the error() method if
  * terminate="yes" is specified. This behaviour is specified in recent versions of the JAXP interface
  * specifications, but it is not the default behaviour, for backwards compatibility reasons.
  * <p>The text of the message that is sent to the ErrorListener is an XML serialization of the actual
  * message content.</p>
  */
