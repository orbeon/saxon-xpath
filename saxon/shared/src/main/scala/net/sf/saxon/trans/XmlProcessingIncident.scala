package net.sf.saxon.trans

import net.sf.saxon.lib.ErrorReporter

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.HostLanguage

import net.sf.saxon.s9api.Location

import net.sf.saxon.s9api.QName

import net.sf.saxon.s9api.XmlProcessingError

import net.sf.saxon.tree.util.Navigator

import javax.xml.transform.TransformerException

import java.util.Objects

import XmlProcessingIncident._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object XmlProcessingIncident {

  def maybeSetHostLanguage(error: XmlProcessingError,
                           lang: HostLanguage.HostLanguage): Unit = {
    if (error.getHostLanguage == null) {
      if (error.isInstanceOf[XmlProcessingIncident]) {
        error.asInstanceOf[XmlProcessingIncident].setHostLanguage(lang)
      } else if (error.isInstanceOf[XmlProcessingException]) {
        error
          .asInstanceOf[XmlProcessingException]
          .getXPathException
          .setHostLanguage(lang)
      }
    }
  }

  def maybeSetLocation(error: XmlProcessingError, loc: Location): Unit = {
    if (error.getLocation == null) {
      if (error.isInstanceOf[XmlProcessingIncident]) {
        error.asInstanceOf[XmlProcessingIncident].setLocation(loc)
      } else if (error.isInstanceOf[XmlProcessingException]) {
        error
          .asInstanceOf[XmlProcessingException]
          .getXPathException
          .setLocation(loc)
      }
    }
  }

}

class XmlProcessingIncident(@BeanProperty var msg: String,
                            errCode: String,
                            location: Location)
  extends XmlProcessingError {

  private var errorCode: String = _

  @BeanProperty
  var cause: Throwable = _

  private var locator: Location = location

  var isWarning: Boolean = false

  var isTypeError: Boolean = _

  @BeanProperty
  var fatalErrorMessage: String = _

  private var hasBeenReported: Boolean = false

  @BeanProperty
  var hostLanguage: HostLanguage.HostLanguage = _

  var isStaticError: Boolean = _

  Objects.requireNonNull(msg)

  Objects.requireNonNull(errorCode)

  Objects.requireNonNull(location)

  this.setErrorCodeAsEQName(errorCode)

  def this(message: String) = {
    this(message, "", null)
    this.setMsg(message)
  }

  def this(message: String, errorCode: String) = {
    this(message,errorCode,null)
    this.setMsg(message)
    this.setErrorCodeAsEQName(errorCode)
  }

  def this(err: TransformerException, isWarning: Boolean) = {
    this("","",null)
    val exception: XPathException = XPathException.makeXPathException(err)
    setMsg(exception.getMessage)
    errorCode = exception.getErrorCodeQName.getEQName
    locator = exception.getLocator
    this.isWarning = isWarning
  }

  def setWarning(warning: Boolean): Unit = {
    isWarning = warning
  }

  def asWarning(): XmlProcessingIncident = {
    isWarning = true
    this
  }

  def setFatal(message: String): Unit = {
    fatalErrorMessage = message
  }

  def isAlreadyReported(): Boolean = hasBeenReported

  def setAlreadyReported(reported: Boolean): Unit = {
    this.hasBeenReported = reported
  }

  def setTypeError(isTypeError: Boolean): Unit = {
    this.isTypeError = isTypeError
  }

  def setStaticError(isStaticError: Boolean): Unit = {
    this.isStaticError = isStaticError
  }

  def getErrorCode(): QName = {
    if (errorCode == null) {
      null
    }
    new QName(StructuredQName.fromEQName(errorCode))
  }

  def setErrorCodeAsEQName(code: String): Unit = {
    this.errorCode =
      if (code.startsWith("Q{")) code
      else if (NameChecker.isValidNCName(code))
        "Q{" + NamespaceConstant.ERR + "}" + code
      else "Q{" + NamespaceConstant.SAXON + "}invalid-error-code"
  }

  override def getModuleUri(): String = getLocation.getSystemId

  def getLocation(): Location = locator

  def setLocation(loc: Location): Unit = {
    this.locator = loc
  }

  override def getColumnNumber(): Int = {
    val locator: Location = getLocation
    if (locator != null) {
      locator.getColumnNumber
    }
    -1
  }

  override def getLineNumber(): Int = {
    val locator: Location = getLocation
    if (locator != null) {
      locator.getLineNumber
    }
    -1
  }

  override def getInstructionName(): String =
    locator.asInstanceOf[NodeInfo].getDisplayName

  override def getPath(): String =
    if (locator.isInstanceOf[NodeInfo]) {
      Navigator.getPath(locator.asInstanceOf[NodeInfo])
    } else {
      null
    }

  override def getMessage(): String = msg
}
