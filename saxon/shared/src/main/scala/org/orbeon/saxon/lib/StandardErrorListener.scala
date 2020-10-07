package org.orbeon.saxon.lib

import java.util.{HashSet, Set}

import javax.xml.transform.{ErrorListener, SourceLocator, TransformerException}
import org.orbeon.saxon.expr.{EarlyEvaluationContext, Expression, XPathContext}
import org.orbeon.saxon.expr.parser.XPathParser
import org.orbeon.saxon.model.{ValidationException, ValidationFailure}
import org.orbeon.saxon.om.{Sequence, StructuredQName}
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.AttributeLocation
import org.xml.sax.SAXException

import scala.beans.BeanProperty
import scala.util.control.Breaks._

//remove if not needed

class StandardErrorListener extends StandardDiagnostics with ErrorListener {

  private var warningCount: Int = 0

  @BeanProperty
  var maximumNumberOfWarnings: Int = 25

  private var maxOrdinaryCharacter: Int = 255

  @BeanProperty
  var stackTraceDetail: Int = 2

  private var warningsIssued: Set[String] = new HashSet()

  @transient var logger: Logger = new StandardLogger()

  def makeAnother(hostLanguage: HostLanguage): StandardErrorListener = {
    var sel: StandardErrorListener = null
    try sel = this.getClass.newInstance()
    catch {
      case e@(_: InstantiationException | _: IllegalAccessException) =>
        sel = new StandardErrorListener()

    }
    sel.logger = logger
    sel
  }

  def setLogger(logger: Logger): Unit = {
    this.logger = logger
  }

  def getLogger: Logger = logger

  def setMaxOrdinaryCharacter(max: Int): Unit = {
    maxOrdinaryCharacter = max
  }

  def getMaxOrdinaryCharacter(max: Int): Int = maxOrdinaryCharacter

  def warning(exception: TransformerException): Unit = {
    if (logger == null) {
      logger = new StandardLogger()
    }
    val xe: XPathException = XPathException.makeXPathException(exception)
    val message: String = constructMessage(exception, xe, "", "Warning ")
    if (!warningsIssued.contains(message)) {
      if (exception.isInstanceOf[ValidationException]) {
        logger.error(message)
      } else {
        logger.warning(message)
        warningCount += 1
        if (warningCount > getMaximumNumberOfWarnings) {
          logger.info("No more warnings will be displayed")
          warningCount = 0
        }
      }
      warningsIssued.add(message)
    }
  }

  def isReportingWarnings: Boolean = true

  def error(exception: TransformerException): Unit = {
    if (logger == null) {
      logger = new StandardLogger()
    }
    var message: String = null
    if (exception.isInstanceOf[ValidationException]) {
      val explanation: String = getExpandedMessage(exception)
      val failure: ValidationFailure =
        exception.asInstanceOf[ValidationException].getValidationFailure
      val constraintReference: String = failure.getConstraintReferenceMessage
      val validationLocation: String = failure.getValidationLocationText
      val contextLocation: String = failure.getContextLocationText
      message = "Validation error " + getLocationMessage(exception) +
        "\n  " +
        wordWrap(explanation) +
        wordWrap(
          if (validationLocation.isEmpty) ""
          else "\n  " + validationLocation) +
        wordWrap(
          if (contextLocation.isEmpty) "" else "\n  " + contextLocation) +
        wordWrap(
          if (constraintReference == null) ""
          else "\n  " + constraintReference) +
        formatListOfOffendingNodes(failure)
    } else {
      val prefix: String = "Error "
      message = constructMessage(exception,
        XPathException.makeXPathException(exception),
        "",
        prefix)
    }
    if (exception.isInstanceOf[ValidationException]) {
      logger.error(message)
    } else {
      logger.error(message)
      logger.info("Processing terminated because error recovery is disabled")
    }
  }

  def fatalError(exception: TransformerException): Unit = {
    val xe: XPathException = XPathException.makeXPathException(exception)
    if (xe.hasBeenReported) {
      return
    }
    if (logger == null) {
      logger = new StandardLogger()
    }
    var message: String = null
    val lang: String = xe.getHostLanguage
    var langText: String = ""
    if ("XPath" == lang) {
      langText = "in expression "
    } else if ("XQuery" == lang) {
      langText = "in query "
    } else if ("XSLT Pattern" == lang) {
      langText = "in pattern "
    }
    var kind: String = "Error "
    if (xe.isSyntaxError) {
      kind = "Syntax error "
    } else if (xe.isStaticError) {
      kind = "Static error "
    } else if (xe.isTypeError) {
      kind = "Type error "
    }
    message = constructMessage(exception, xe, langText, kind)
    logger.error(message)
    if (exception.isInstanceOf[XPathException]) {
      exception.asInstanceOf[XPathException].setHasBeenReported(true)
    }
    if (exception.isInstanceOf[XPathException]) {
      val context: XPathContext =
        exception.asInstanceOf[XPathException].getXPathContext
      if (context != null && !(context.isInstanceOf[EarlyEvaluationContext])) {
        outputStackTrace(logger, context)
      }
    }
  }

  def constructMessage(exception: TransformerException,
                       xe: XPathException,
                       langText: String,
                       kind: String): String =
    constructFirstLine(exception, xe, langText, kind) + "\n  " +
      constructSecondLine(exception, xe)

  def constructFirstLine(exception: TransformerException,
                         xe: XPathException,
                         langText: String,
                         kind: String): String = {
    var failingExpression: Expression = null
    if (exception.isInstanceOf[XPathException]) {
      failingExpression =
        exception.asInstanceOf[XPathException].getFailingExpression
    }
    if (xe.getLocator.isInstanceOf[AttributeLocation]) {
      kind + langText + getLocationMessageText(xe.getLocator)
    } else if (xe.getLocator.isInstanceOf[XPathParser.NestedLocation]) {
      val nestedLoc: XPathParser.NestedLocation =
        xe.getLocator.asInstanceOf[XPathParser.NestedLocation]
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
    } else if (xe.isInstanceOf[ValidationException]) {
      "Validation error " + getLocationMessage(exception)
    } else {
      kind +
        (if (failingExpression != null)
          "evaluating (" + failingExpression.toShortString + ") "
        else "") +
        getLocationMessage(exception)
    }
  }

  def formatExtraContext(failingExpression: Expression,
                         nearBy: String): String =
    if (failingExpression != null) {
      if (failingExpression.isCallOn(classOf[org.orbeon.saxon.functions.Error])) {
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

  def constructSecondLine(err: TransformerException,
                          xe: XPathException): String =
    if (xe.isInstanceOf[ValidationException]) {
      var explanation: String = getExpandedMessage(err)
      val failure: ValidationFailure =
        xe.asInstanceOf[ValidationException].getValidationFailure
      val constraintReference: String = failure.getConstraintReferenceMessage
      if (constraintReference != null) {
        explanation += " (" + constraintReference + ')'
      }
      wordWrap(explanation + formatListOfOffendingNodes(failure))
    } else {
      expandSpecialCharacters(wordWrap(getExpandedMessage(err))).toString
    }

  def outputStackTrace(out: Logger, context: XPathContext): Unit = {
    printStackTrace(context, out, stackTraceDetail)
  }

  def getLocationMessage(err: TransformerException): String = {
    var loc: SourceLocator = err.getLocator
    var errVar = err
    while (loc == null) if (errVar.getException
      .isInstanceOf[TransformerException]) {
      errVar = errVar.getException.asInstanceOf[TransformerException]
      loc = err.getLocator
    } else if (errVar.getCause.isInstanceOf[TransformerException]) {
      errVar = errVar.getCause.asInstanceOf[TransformerException]
      loc = errVar.getLocator
    } else {
      ""
    }
    getLocationMessageText(loc)
  }

  def getExpandedMessage(err: TransformerException): String = {
    var message: String = formatErrorCode(err)
    if (err.isInstanceOf[XPathException]) {
      val errorObject: Sequence =
        err.asInstanceOf[XPathException].getErrorObject
      if (errorObject != null) {
        val errorObjectDesc: String = formatErrorObject(errorObject)
        if (errorObjectDesc != null) {
          message += " " + errorObjectDesc
        }
      }
    }
    message = formatNestedMessages(err, message)
    message
  }

  def formatNestedMessages(err: TransformerException,
                           message: String): String = {
    var e: Throwable = err
    var messageVar = message
    breakable {
      while (true) {
        if (e == null) {
          break()
        }
        var next: String = e.getMessage
        if (next == null) {
          next = ""
        }
        if (next.startsWith("org.orbeon.saxon.trans.XPathException: ")) {
          next = next.substring(next.indexOf(": ") + 2)
        }
        if (!("TRaX Transform Exception" == next || messageVar.endsWith(next))) {
          if ("" != messageVar && !messageVar.trim().endsWith(":")) {
            messageVar += ": "
          }
          messageVar += next
        }
        if (e.isInstanceOf[TransformerException]) {
          e = e.asInstanceOf[TransformerException].getException
        } else if (e.isInstanceOf[SAXException]) {
          e = e.asInstanceOf[SAXException].getException
        } else {
          break()
        }
      }
    }
    messageVar
  }

  def formatErrorCode(err: TransformerException): String = {
    var qCode: StructuredQName = null
    if (err.isInstanceOf[XPathException]) {
      qCode = err.asInstanceOf[XPathException].getErrorCodeQName
    }
    if (qCode == null && err.getException.isInstanceOf[XPathException]) {
      qCode = err.getException.asInstanceOf[XPathException].getErrorCodeQName
    }
    var message: String = ""
    if (qCode != null) {
      message =
        if (qCode.hasURI(NamespaceConstant.ERR)) qCode.getLocalPart
        else qCode.getDisplayName
    }
    message
  }

  def formatErrorObject(errorObject: Sequence): String = null

  def expandSpecialCharacters(in: CharSequence): CharSequence =
    if (logger.isUnicodeAware) {
      in
    } else {
      expandSpecialCharacters(in, maxOrdinaryCharacter)
    }

}
