package net.sf.saxon.trans

import net.sf.saxon.expr.instruct.Instruction
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.s9api.HostLanguage.HostLanguage
import net.sf.saxon.s9api.{HostLanguage, Location, QName, XmlProcessingError}
import net.sf.saxon.tree.AttributeLocation

import scala.beans.BeanProperty

//remove if not needed
// import scala.collection.JavaConversions._

class XmlProcessingException(private var exception: XPathException)
  extends XmlProcessingError {

  var isWarning: Boolean = _

  @BeanProperty
  var fatalErrorMessage: String = _

  def getXPathException: XPathException = exception

  override def getHostLanguage(): HostLanguage = {
    val loc: Location = getLocation
    if (loc.isInstanceOf[Instruction] || loc.isInstanceOf[AttributeLocation]) {
      HostLanguage.XSLT
    } else {
      HostLanguage.XPATH
    }
  }

  override def isStaticError(): Boolean = exception.isStaticError

  override def isTypeError(): Boolean = exception.isTypeError

  override def getErrorCode(): QName = {
    val errorCodeQName: StructuredQName = exception.getErrorCodeQName
    if (errorCodeQName == null) null else new QName(errorCodeQName)
  }

  override def getMessage(): String = exception.getMessage

  override def getLocation(): Location = exception.getLocator

  override def getPath(): String = null

  override def getCause(): Throwable = exception.getCause

  def setWarning(warning: Boolean): Unit = {
    isWarning = warning
  }

  def asWarning(): XmlProcessingException = {
    val e2: XmlProcessingException = new XmlProcessingException(exception)
    e2.setWarning(true)
    e2
  }

  def setFatal(message: String): Unit = {
    this.fatalErrorMessage = message
  }

  def isAlreadyReported(): Boolean = exception.hasBeenReported

  def setAlreadyReported(reported: Boolean): Unit = {
    exception.setHasBeenReported(reported)
  }

}