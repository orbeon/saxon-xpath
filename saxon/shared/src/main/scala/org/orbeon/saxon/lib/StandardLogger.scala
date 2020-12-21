////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.lib

import java.io.PrintStream

import javax.xml.transform.stream.StreamResult

import scala.beans.BeanProperty

//remove if not needed

/**
  * The default Logger used by Saxon on the Java platform. All messages are written by default
  * to System.err. The logger can be configured by setting a different output destination, and
  * by setting a minimum threshold for the severity of messages to be output.
  */
class StandardLogger extends Logger {

  private var out: PrintStream = System.err

  @BeanProperty
  var threshold: Int = Logger.INFO

  private var mustClose: Boolean = false

  def this(stream: PrintStream) = {
    this()
    setPrintStream(stream)
  }

  // ORBEON: No `File` support.
//  def this(fileName: File) = {
//    this()
//    setPrintStream(new PrintStream(fileName))
//    mustClose = true
//  }

  def setPrintStream(stream: PrintStream): Unit =
    out = stream

  def getPrintStream: PrintStream = out

  /**
    * Get a JAXP Result object allowing serialized XML to be written to this Logger
    *
    * @return a Result that serializes XML to this Logger
    */
  def asStreamResult(): StreamResult = new StreamResult(out)

  def println(message: String, severity: Int): Unit =
    if (severity >= threshold)
      out.println(message)

  /**
    * Close the logger, indicating that no further messages will be written
    */
  override def close(): Unit =
    if (mustClose)
      out.close()
}
