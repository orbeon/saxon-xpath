package org.orbeon.saxon.trans

import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.{NameChecker, NodeInfo, StructuredQName}
import org.orbeon.saxon.s9api.{HostLanguage, Location, QName, XmlProcessingError}
import org.orbeon.saxon.tree.util.Navigator

import scala.beans.BeanProperty


object XmlProcessingIncident {

  def maybeSetHostLanguage(error: XmlProcessingError, lang: HostLanguage.HostLanguage): Unit =
    if (error.getHostLanguage == null)
      error match {
        case incident: XmlProcessingIncident   => incident.setHostLanguage(lang)
        case exception: XmlProcessingException => exception.getXPathException.setHostLanguage(lang)
        case _                                 =>
      }

  def maybeSetLocation(error: XmlProcessingError, loc: Location): Unit =
    if (error.getLocation == null)
      error match {
        case incident: XmlProcessingIncident   => incident.setLocation(loc)
        case exception: XmlProcessingException => exception.getXPathException.setLocation(loc)
        case _                                 =>
      }
}

class XmlProcessingIncident(
  message    : String,
  _errorCode : String   = null,
  _location  : Location = null
) extends XmlProcessingError {

  require(message ne null)

  private val errorCode: String = if (_errorCode ne null) errorCodeAsEQName(_errorCode) else null
  private var locator: Location = _location

  private var hasBeenReported: Boolean = false

  var isWarning: Boolean = false
  var isTypeError: Boolean = _

  @BeanProperty
  var cause: Throwable = _

  @BeanProperty
  var fatalErrorMessage: String = _

  @BeanProperty
  var hostLanguage: HostLanguage.HostLanguage = _

  var isStaticError: Boolean = _

  // ORBEON: No usages.
//  def this(err: TransformerException, isWarning: Boolean) = {
//    this("","",null)
//    val exception = XPathException.makeXPathException(err)
//    setMsg(exception.getMessage)
//    errorCode = exception.getErrorCodeQName.getEQName
//    locator = exception.getLocator
//    this.isWarning = isWarning
//  }

  def setWarning(warning: Boolean): Unit =
    isWarning = warning

  def asWarning(): XmlProcessingIncident = {
    isWarning = true
    this
  }

  def setFatal(message: String): Unit =
    fatalErrorMessage = message

  def isAlreadyReported: Boolean = hasBeenReported

  def setAlreadyReported(reported: Boolean): Unit =
    this.hasBeenReported = reported

  def setTypeError(isTypeError: Boolean): Unit =
    this.isTypeError = isTypeError

  def setStaticError(isStaticError: Boolean): Unit =
    this.isStaticError = isStaticError

  def getErrorCode: QName =
    if (errorCode == null)
      null
    else
      new QName(StructuredQName.fromEQName(errorCode))

  override def getModuleUri: String = getLocation.getSystemId

  def getLocation: Location = locator

  def setLocation(loc: Location): Unit =
    this.locator = loc

  override def getColumnNumber: Int = {
    val locator = getLocation
    if (locator != null)
      locator.getColumnNumber
    else
      -1
  }

  override def getLineNumber: Int = {
    val locator = getLocation
    if (locator != null)
      locator.getLineNumber
    else
      -1
  }

  override def getInstructionName: String =
    locator.asInstanceOf[NodeInfo].getDisplayName

  override def getPath: String =
    locator match {
      case nodeInfo: NodeInfo => Navigator.getPath(nodeInfo)
      case _                  => null
    }

  def getMessage: String = message

  private def errorCodeAsEQName(code: String): String =
    if (code.startsWith("Q{"))
      code
    else if (NameChecker.isValidNCName(code))
      "Q{" + NamespaceConstant.ERR + "}" + code
    else
      "Q{" + NamespaceConstant.SAXON + "}invalid-error-code"
}
