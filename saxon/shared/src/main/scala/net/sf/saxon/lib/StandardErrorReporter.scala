////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * <B>StandardErrorReporter</B> is the standard error handler for processing XSLT and XQuery static
  * errors, used if no other error handler is nominated.
  */
package net.sf.saxon.lib

import java.io.{PrintWriter, StringWriter}
import java.util.{HashSet, Set}

import net.sf.saxon.expr.{EarlyEvaluationContext, Expression, XPathContext}
import net.sf.saxon.expr.parser.XPathParser
import net.sf.saxon.s9api.HostLanguage.HostLanguage
import net.sf.saxon.s9api.{HostLanguage, Location, QName, XmlProcessingError}
import net.sf.saxon.trans.{Err, XPathException, XmlProcessingException}
import net.sf.saxon.tree.AttributeLocation
import org.xml.sax.SAXParseException

import scala.beans.BeanProperty

//remove if not needed

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

  private var warningsIssued: Set[String] = new HashSet()

  @transient  var logger: Logger = new StandardLogger()

  private var latestError: XmlProcessingError = _

  private var outputErrorCodes: Boolean = true

  def setLogger(logger: Logger): Unit = {
    this.logger = logger
  }

  def getLogger: Logger = logger

  def setMaxOrdinaryCharacter(max: Int): Unit = {
    maxOrdinaryCharacter = max
  }

  def getMaxOrdinaryCharacter(max: Int): Int = maxOrdinaryCharacter

  def setOutputErrorCodes(include: Boolean): Unit = {
    this.outputErrorCodes = include
  }

  def report(error: XmlProcessingError): Unit = {
    if (error != latestError) {
      latestError = error
      if (error.isWarning) {
        warning(error)
      } else {
        this.error(error)
      }
    }
  }

   def warning(error: XmlProcessingError): Unit = {
    if (logger == null) {
      logger = new StandardLogger()
    }
    val message: String = constructMessage(error, "", "Warning ")
    if (!warningsIssued.contains(message)) {
      logger.warning(message)
       warningCount += 1
      if (warningCount > getMaximumNumberOfWarnings) {
        logger.info("No more warnings will be displayed")
      }
      warningsIssued.add(message)
    }
  }

  def isReportingWarnings: Boolean =
    warningCount < getMaximumNumberOfWarnings

   def error(error: XmlProcessingError): Unit = {
    if ({ errorCount += 1; errorCount - 1 } > maximumNumberOfErrors) {
      error.setFatal("Too many errors reported")
    }
    if (logger == null) {
      logger = new StandardLogger()
    }
    var message: String = null
    val lang: HostLanguage = error.getHostLanguage
    var langText: String = ""
    if (lang != null) {
      lang match {
        case HostLanguage.XSLT =>
        case HostLanguage.XQUERY => langText = "in query "
        case HostLanguage.XPATH => langText = "in expression "
        case HostLanguage.XML_SCHEMA => langText = "in schema "
        case HostLanguage.XSLT_PATTERN => langText = "in pattern "

      }
    }
    var kind: String = "Error "
    if (error.isTypeError) {
      kind = "Type error "
    } else if (error.isStaticError) {
      kind = "Static error "
    }
    message = constructMessage(error, langText, kind)
    logger.error(message)
    if (error.isInstanceOf[XmlProcessingException]) {
      val exception: XPathException =
        error.asInstanceOf[XmlProcessingException].getXPathException
      val context: XPathContext = exception.getXPathContext
      if (context != null && !(context.isInstanceOf[EarlyEvaluationContext])) {
        outputStackTrace(logger, context)
      }
    }
  }

  def constructMessage(exception: XmlProcessingError,
                       langText: String,
                       kind: String): String =
    constructFirstLine(exception, langText, kind) + "\n  " +
      constructSecondLine(exception)

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
    val locator: Location = error.getLocation
    if (locator.isInstanceOf[AttributeLocation]) {
      kind + langText + getLocationMessageText(locator)
    } else if (locator.isInstanceOf[XPathParser.NestedLocation]) {
      val nestedLoc: XPathParser.NestedLocation =
        locator.asInstanceOf[XPathParser.NestedLocation]
      val outerLoc: Location = nestedLoc.getContainingLocation
      val line: Int = nestedLoc.getLocalLineNumber
      val column: Int = nestedLoc.getColumnNumber
      val lineInfo: String = if (line <= 0) "" else "on line " + line + ' '
      val columnInfo: String =
        if (column < 0) ""
        else
          "at " + (if (line <= 0) "char " else "column ") + column +
            ' '
      val nearBy: String = nestedLoc.getNearbyText
      val failingExpression: Expression = null
      val extraContext: String = formatExtraContext(failingExpression, nearBy)
      if (outerLoc.isInstanceOf[AttributeLocation]) {
        val innerLoc: String = lineInfo + extraContext + columnInfo
        kind + innerLoc + langText + getLocationMessageText(outerLoc)
      } else {
        var innerLoc: String = lineInfo + columnInfo
        if (outerLoc.getLineNumber > 1) {
          innerLoc += "(" + langText + "on line " + outerLoc.getLineNumber +
            ") "
        }
        if (outerLoc.getSystemId != null) {
          innerLoc += "of " + outerLoc.getSystemId + " "
        }
        kind + extraContext + innerLoc
      }
    } else {
      kind + getLocationMessage(error)
    }
  }

  def formatExtraContext(failingExpression: Expression,
                         nearBy: String): String =
    if (failingExpression != null) {
      if (failingExpression.isCallOn(classOf[net.sf.saxon.functions.Error])) {
        "signaled by call to error() "
      } else {
        "evaluating (" + failingExpression.toShortString + ") "
      }
    } else if (nearBy != null && !nearBy.isEmpty) {
      (if (nearBy.startsWith("...")) "near" else "in") + ' ' +
        Err.wrap(nearBy) +
        " "
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
    var message: String = formatErrorCode(err) + " " + err.getMessage
    message = formatNestedMessages(err, message)
    message
  }

  def formatNestedMessages(err: XmlProcessingError, message: String): String =
    if (err.getCause == null) {
      message
    } else {
      val sb: StringBuilder = new StringBuilder(message)
      var e: Throwable = err.getCause
      while (e != null) {
        if (!(e.isInstanceOf[SAXParseException])) {
          if (e.isInstanceOf[RuntimeException]) {
            val sw: StringWriter = new StringWriter()
            e.printStackTrace(new PrintWriter(sw))
            sb.append('\n').append(sw)
          } else {
            sb.append(". Caused by ").append(e.getClass.getName)
          }
        }
        val next: String = e.getMessage
        if (next != null) {
          sb.append(": ").append(next)
        }
        e = e.getCause
      }
      sb.toString
    }

  def formatErrorCode(err: XmlProcessingError): String = {
    if (outputErrorCodes) {
      val qCode: QName = err.getErrorCode
      if (qCode != null) {
        if (qCode.getNamespaceURI == NamespaceConstant.ERR) {
          qCode.getLocalName + " "
        } else {
          qCode.toString + " "
        }
      }
    }
    ""
  }

  def expandSpecialCharacters(in: CharSequence): CharSequence =
    if (logger.isUnicodeAware) {
      in
    } else {
      expandSpecialCharacters(in, maxOrdinaryCharacter)
    }

   def outputStackTrace(out: Logger, context: XPathContext): Unit = {
    printStackTrace(context, out, stackTraceDetail)
  }

}


