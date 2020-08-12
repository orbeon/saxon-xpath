////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.trans.SaxonErrorCode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XmlProcessingException

import net.sf.saxon.trans.XmlProcessingIncident

import org.xml.sax.SAXException

import org.xml.sax.SAXParseException

import java.util.Objects

import scala.beans.{BeanProperty, BooleanBeanProperty}




class StandardErrorHandler(reporter: ErrorReporter)
    extends org.xml.sax.ErrorHandler {

  private var errorReporter: ErrorReporter = Objects.requireNonNull(reporter)

  @BeanProperty
  var warningCount: Int = 0

  @BeanProperty
  var errorCount: Int = 0

  @BeanProperty
  var fatalErrorCount: Int = 0

  private var silent: Boolean = false

  def setSilent(silent: Boolean): Unit = {
    this.silent = silent
  }

  def warning(e: SAXParseException): Unit = {
    try {
      { warningCount += 1; warningCount - 1 }
      if (!silent) {
        errorReporter.report(
          new XmlProcessingException(XPathException.makeXPathException(e))
            .asWarning())
      }
    } catch {
      case ignored: Exception => {}

    }
  }

  def error(e: SAXParseException): Unit = {
//System.err.println("ErrorHandler.error " + e.getMessage());
    { errorCount += 1; errorCount - 1 }
    if (!silent) {
      reportError(e, false)
    }
  }

  def fatalError(e: SAXParseException): Unit = {
//System.err.println("ErrorHandler.fatalError " + e.getMessage());
    { fatalErrorCount += 1; fatalErrorCount - 1 }
    if (!silent) {
      reportError(e, true)
    }
    throw e
  }

   def reportError(e: SAXParseException, isFatal: Boolean): Unit = {
    val loc: Loc = new Loc(e.getSystemId, e.getLineNumber, e.getColumnNumber)
    if (errorReporter != null) {
      val err: XmlProcessingIncident = new XmlProcessingIncident(
        "Error reported by XML parser",
        SaxonErrorCode.SXXP0003,
        loc)
      err.setCause(e)
      errorReporter.report(err)
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A default implementation of the SAX ErrorHandler interface. Used by Saxon to catch XML parsing errors
  * if no error handler is supplied by the application.
  */
