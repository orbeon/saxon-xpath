package org.orbeon.saxon.trans

import org.orbeon.saxon.expr.instruct.Instruction
import org.orbeon.saxon.s9api.HostLanguage.HostLanguage
import org.orbeon.saxon.s9api.{HostLanguage, Location, QName, XmlProcessingError}
import org.orbeon.saxon.tree.AttributeLocation

import scala.beans.BeanProperty


class XmlProcessingException(private var exception: XPathException)
  extends XmlProcessingError {

  var isWarning: Boolean = _

  @BeanProperty
  var fatalErrorMessage: String = _

  def getXPathException: XPathException = exception

  def getHostLanguage: HostLanguage = {
    val loc = getLocation
    if (loc.isInstanceOf[Instruction] || loc.isInstanceOf[AttributeLocation])
      HostLanguage.XSLT
    else
      HostLanguage.XPATH
  }

  def isStaticError: Boolean = exception.isStaticError

  def isTypeError: Boolean = exception.isTypeError

  def getErrorCode: QName = {
    val errorCodeQName = exception.getErrorCodeQName
    if (errorCodeQName == null)
      null
    else
      new QName(errorCodeQName)
  }

  def getMessage: String = exception.getMessage
  def getLocation: Location = exception.getLocator
  override def getPath: String = null
  def getCause: Throwable = exception.getCause

  def setWarning(warning: Boolean): Unit =
    isWarning = warning

  def asWarning(): XmlProcessingException = {
    val e2 = new XmlProcessingException(exception)
    e2.setWarning(true)
    e2
  }

  def setFatal(message: String): Unit =
    this.fatalErrorMessage = message

  def isAlreadyReported: Boolean = exception.hasBeenReported

  def setAlreadyReported(reported: Boolean): Unit =
    exception.setHasBeenReported(reported)
}