////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.io.{PrintWriter, StringWriter}
import java.{util => ju}

import org.orbeon.saxon.expr.parser.XPathParser
import org.orbeon.saxon.expr.{EarlyEvaluationContext, Expression, XPathContext}
import org.orbeon.saxon.s9api.{HostLanguage, Location, XmlProcessingError}
import org.orbeon.saxon.trans.{Err, XmlProcessingException}
import org.orbeon.saxon.tree.AttributeLocation
import org.xml.sax.SAXParseException

import scala.beans.BeanProperty


/**
  * <B>StandardErrorReporter</B> is the standard error handler for processing XSLT and XQuery static
  * errors, used if no other error handler is nominated.
  */
class StandardErrorReporter extends StandardDiagnostics with ErrorReporter {

  private var warningCount: Int = 0
  @BeanProperty
  var maximumNumberOfWarnings: Int = 25
  private var errorCount: Int = 0
  @BeanProperty
  var maximumNumberOfErrors: Int = 1000
  private var maxOrdinaryCharacter: Int = 255
  @BeanProperty
  var stackTraceDetail: Int = 2
  private val warningsIssued: ju.Set[String] = new ju.HashSet
  @transient  var logger: Logger = new StandardLogger
  private var latestError: XmlProcessingError = _
  private var outputErrorCodes: Boolean = true

  def setLogger(logger: Logger): Unit =
    this.logger = logger

  def getLogger: Logger = logger

  def setMaxOrdinaryCharacter(max: Int): Unit =
    maxOrdinaryCharacter = max

  def getMaxOrdinaryCharacter(max: Int): Int = maxOrdinaryCharacter

  def setOutputErrorCodes(include: Boolean): Unit =
    this.outputErrorCodes = include

  def report(error: XmlProcessingError): Unit =
    if (error != latestError) {
      latestError = error
      if (error.isWarning)
        warning(error)
      else
        this.error(error)
    }

   def warning(error: XmlProcessingError): Unit = {

    if (logger == null)
      logger = new StandardLogger

    val message = constructMessage(error, "", "Warning ")
    if (! warningsIssued.contains(message)) {
      logger.warning(message)
      warningCount += 1
      if (warningCount > getMaximumNumberOfWarnings)
        logger.info("No more warnings will be displayed")
      warningsIssued.add(message)
    }
  }

  def isReportingWarnings: Boolean =
    warningCount < getMaximumNumberOfWarnings

   def error(error: XmlProcessingError): Unit = {

    if ({ errorCount += 1; errorCount - 1 } > maximumNumberOfErrors)
      error.setFatal("Too many errors reported")

    if (logger == null)
      logger = new StandardLogger

    var message: String = null
    val lang = error.getHostLanguage
    var langText = ""
    if (lang != null)
      lang match {
        case HostLanguage.XSLT         =>
        case HostLanguage.XQUERY       => langText = "in query "
        case HostLanguage.XPATH        => langText = "in expression "
        case HostLanguage.XML_SCHEMA   => langText = "in schema "
        case HostLanguage.XSLT_PATTERN => langText = "in pattern "
      }

    var kind = "Error "
    if (error.isTypeError)
      kind = "Type error "
    else if (error.isStaticError)
      kind = "Static error "

    message = constructMessage(error, langText, kind)
    logger.error(message)

    error match {
      case xmlProcessingException: XmlProcessingException =>
        val exception = xmlProcessingException.getXPathException
        val context = exception.getXPathContext
        if (context != null && !context.isInstanceOf[EarlyEvaluationContext])
          outputStackTrace(logger, context)
      case _ =>
    }
  }

  def constructMessage(exception: XmlProcessingError,
                       langText: String,
                       kind: String): String =
    constructFirstLine(exception, langText, kind) + "\n  " + constructSecondLine(exception)

  /**
    * Construct the first line of the error or warning message. This typically contains
    * information about the kind of error that occurred, and the location where it occurred
    * @param error the error originally reported to the ErrorReporter
    * @param langText a string such as "in expression" or "in query" identifying the kind of
    *                 construct that is in error
    * @param kind the kind of error, for example "Syntax error", "Static error", "Type error"
    * @return the constructed message
    */
  def constructFirstLine(error: XmlProcessingError,
                         langText: String,
                         kind: String): String = {
    error.getLocation match {
      case attLocation: AttributeLocation =>
        kind + langText + getLocationMessageText(attLocation)
      case nestedLocation: XPathParser.NestedLocation =>
        val outerLoc = nestedLocation.getContainingLocation
        val line = nestedLocation.getLocalLineNumber
        val column = nestedLocation.getColumnNumber
        val lineInfo = if (line <= 0) "" else "on line " + line + ' '
        val columnInfo =
          if (column < 0)
            ""
          else
            "at " + (if (line <= 0) "char " else "column ") + column +
              ' '
        val nearBy = nestedLocation.getNearbyText
        val failingExpression: Expression = null
        val extraContext = formatExtraContext(failingExpression, nearBy)
        if (outerLoc.isInstanceOf[AttributeLocation]) {
          val innerLoc = lineInfo + extraContext + columnInfo
          kind + innerLoc + langText + getLocationMessageText(outerLoc)
        } else {
          var innerLoc = lineInfo + columnInfo
          if (outerLoc.getLineNumber > 1)
            innerLoc += "(" + langText + "on line " + outerLoc.getLineNumber + ") "
          if (outerLoc.getSystemId != null)
            innerLoc += "of " + outerLoc.getSystemId + " "
          kind + extraContext + innerLoc
        }
      case _ =>
        kind + getLocationMessage(error)
    }
  }

  def formatExtraContext(failingExpression: Expression, nearBy: String): String =
    if (failingExpression != null) {
      if (failingExpression.isCallOn(classOf[org.orbeon.saxon.functions.Error]))
        "signaled by call to error() "
      else
        "evaluating (" + failingExpression.toShortString + ") "
    } else if (nearBy != null && !nearBy.isEmpty) {
      (
        if (nearBy.startsWith("..."))
          "near"
        else
          "in"
      ) + ' ' + Err.wrap(nearBy) + " "
    } else {
      ""
    }

  def constructSecondLine(err: XmlProcessingError): String =
    expandSpecialCharacters(wordWrap(getExpandedMessage(err))).toString

   def getLocationMessage(err: XmlProcessingError): String = {
    val loc: Location = err.getLocation
    getLocationMessageText(loc)
  }

  def getExpandedMessage(err: XmlProcessingError): String = {
    var message = formatErrorCode(err) + " " + err.getMessage
    message = formatNestedMessages(err, message)
    message
  }

  def formatNestedMessages(err: XmlProcessingError, message: String): String =
    if (err.getCause == null) {
      message
    } else {
      val sb = new StringBuilder(message)
      var e = err.getCause
      while (e != null) {
        if (! e.isInstanceOf[SAXParseException]) {
          if (e.isInstanceOf[RuntimeException]) {
            val sw: StringWriter = new StringWriter()
            e.printStackTrace(new PrintWriter(sw))
            sb.append('\n').append(sw)
          } else {
            sb.append(". Caused by ").append(e.getClass.getName)
          }
        }
        val next = e.getMessage
        if (next != null)
          sb.append(": ").append(next)
        e = e.getCause
      }
      sb.toString
    }

  def formatErrorCode(err: XmlProcessingError): String = {
    if (outputErrorCodes) {
      val qCode = err.getErrorCode
      if (qCode != null) {
        if (qCode.getNamespaceURI == NamespaceConstant.ERR)
          return qCode.getLocalName + " "
        else
          return qCode.toString + " "
      }
    }
    ""
  }

  def expandSpecialCharacters(in: CharSequence): CharSequence =
    if (logger.isUnicodeAware)
      in
    else
      expandSpecialCharacters(in, maxOrdinaryCharacter)

  def outputStackTrace(out: Logger, context: XPathContext): Unit =
    printStackTrace(context, out, stackTraceDetail)
}
