////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import javax.xml.transform.{SourceLocator, TransformerException}
import net.sf.saxon.expr.{Expression, XPathContext}
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.om.{Sequence, StructuredQName}
import net.sf.saxon.s9api.HostLanguage.HostLanguage
import net.sf.saxon.s9api.{Location, QName, XmlProcessingError}
import net.sf.saxon.trans.XPathException._

import scala.beans.BeanProperty




object XPathException {

  private def breakPoint(): Unit = {
//System.err.println("Error!!");
    assert(true)
  }

  /*@NotNull*/

  def makeXPathException(err: Exception): XPathException =
    if (err.isInstanceOf[XPathException]) {
      err.asInstanceOf[XPathException]
    } else if (err.getCause.isInstanceOf[XPathException]) {
      err.getCause.asInstanceOf[XPathException]
    } else if (err.isInstanceOf[TransformerException]) {
      val xe: XPathException = new XPathException(err.getMessage, err)
      xe.setLocator(err.asInstanceOf[TransformerException].getLocator)
      xe
    } else {
      new XPathException(err)
    }

  def fromXmlProcessingError(error: XmlProcessingError): XPathException =
    if (error.isInstanceOf[XmlProcessingException]) {
      error.asInstanceOf[XmlProcessingException].getXPathException
    } else {
      val e: XPathException = new XPathException(error.getMessage)
      e.setLocation(error.getLocation)
      e.setHostLanguage(error.getHostLanguage)
      e.setIsStaticError(error.isStaticError)
      e.setIsTypeError(error.isTypeError)
      val code: QName = error.getErrorCode
      if (code != null) {
        e.setErrorCodeQName(code.getStructuredQName)
      }
      e
    }

  class Circularity /**
    * Create an exception indicating that a circularity was detected
    *
    * @param message the error message
    */
  (message: String)
      extends XPathException(message)

  class StackOverflow(message: String, errorCode: String, location: Location)
      extends XPathException(message, errorCode, location)

}

class XPathException(message: String) extends TransformerException(message) {

  var isTypeError: Boolean = false

  var isSyntaxError: Boolean = false

  var isStaticError: Boolean = false

  var isGlobalError: Boolean = false

  var hostLanguage: String = _

  private var errorCode: StructuredQName = _

  @BeanProperty
  var errorObject: Sequence = _

  @BeanProperty
  var failingExpression: Expression = _

  var hasBeenReported: Boolean = false

  @transient var context: XPathContext = _

  breakPoint()

  def this(err: Throwable) = {
    this("")
    this.initCause(err);
    breakPoint()
  }

  def this(message: String, err: Throwable) = {
    this(message)
    this.initCause(err);
    breakPoint()
  }

  def this(message: String, errorCode: String, loc: Location) = {
    this(message)
    setLocator(loc)
    breakPoint()
  }

  def this(message: String, loc: Location, err: Throwable) = {
    this(message)
    setLocator(loc)
    this.initCause(err);
    breakPoint()
  }

  def this(message: String, errorCode: String) = {
    this(message)
    this.setErrorCode(errorCode)
    breakPoint()
  }

  def this(message: String, errorCode: String, context: XPathContext) = {
    this(message)
    this.setErrorCode(errorCode)
    this.setXPathContext(context)
    breakPoint()
  }

  def setXPathContext(context: XPathContext): Unit = {
    this.context = context
  }

  def setLocation(loc: Location): Unit = {
    if (loc != null) {
      super.setLocator(loc)
    }
  }

  def maybeSetFailingExpression(failingExpression: Expression): Unit = {
    if (this.failingExpression == null) {
      this.failingExpression = failingExpression
    }
    maybeSetLocation(failingExpression.getLocation)
  }

  /**
    * Method getLocator retrieves an instance of a SourceLocator
    * object that specifies where an error occured.
    *
    * @return A SourceLocator object, or null if none was specified.
    */
  override def getLocator(): Location = {
    val locator: SourceLocator = super.getLocator
    if (locator == null) {
      null
    } else if (locator.isInstanceOf[Location]) {
      locator.asInstanceOf[Location]
    } else {
      new Loc(locator)
    }
  }

  def getXPathContext: XPathContext = context

  def setIsStaticError(is: Boolean): Unit = {
    isStaticError = is
  }

  def setIsSyntaxError(is: Boolean): Unit = {
    if (is) {
      isStaticError = true
    }
    isSyntaxError = is
  }

  def setIsTypeError(is: Boolean): Unit = {
    isTypeError = is
  }

  def setIsGlobalError(is: Boolean): Unit = {
    isGlobalError = is
  }

  def setHostLanguage(language: String): Unit = {
    this.hostLanguage = language
  }

  def setHostLanguage(language: HostLanguage): Unit = {
    this.hostLanguage = if (language == null) null
    else language.toString
  }

  def getHostLanguage: String = hostLanguage

  def setErrorCode(code: String): Unit = {
    if (code != null) {
      errorCode = new StructuredQName("err", NamespaceConstant.ERR, code)
    }
  }

  def maybeSetErrorCode(code: String): Unit = {
    if (errorCode == null && code != null) {
      errorCode = new StructuredQName("err", NamespaceConstant.ERR, code)
    }
  }

  def setErrorCodeQName(code: StructuredQName): Unit = {
    errorCode = code
  }

  /*@Nullable*/

  def getErrorCodeQName: StructuredQName = errorCode

  /*@Nullable*/

  def getErrorCodeLocalPart: String =
    if (errorCode == null) null else errorCode.getLocalPart

  /*@Nullable*/

  def getErrorCodeNamespace: String =
    if (errorCode == null) null else errorCode.getURI

  def setHasBeenReported(reported: Boolean): Unit = {
    hasBeenReported = reported
  }

  def maybeSetLocation(here: Location): Unit = {
    if (here != null) {
      if (getLocator == null) {
        setLocator(here.saveLocation());
      } else if (getLocator.getLineNumber == -1 &&
                 !(getLocator.getSystemId != null && here.getSystemId != null &&
                   getLocator.getSystemId != here.getSystemId)) {
        setLocator(here.saveLocation());
      }
    }
  }

  def maybeSetContext(context: XPathContext): Unit = {
    if (getXPathContext == null) setXPathContext(context)
  }

  def isReportableStatically: Boolean = {
    if (isStaticError || isTypeError) {
      return true
    }
    val err: StructuredQName = errorCode
    if (err != null && err.hasURI(NamespaceConstant.ERR)) {
      val local: String = err.getLocalPart
      return local.==("XTDE1260") || local.==("XTDE1280") || local.==("XTDE1390") ||
            local.==("XTDE1400") ||
            local.==("XTDE1428") ||
            local.==("XTDE1440") ||
            local.==("XTDE1460")
    }
    false
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * XPathException is used to indicate an error (static or dynamic) in an XPath expression,
  * or in a query or stylesheet.
  */
