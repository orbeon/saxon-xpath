package org.orbeon.saxon.s9api

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

class SaxonApiException(cause: Throwable) extends Exception(cause) {

  def this(message: String) = {
    this(new Exception(new XPathException(message)))
  }

  def this(message: String, cause: Throwable) = {

    this(new Exception(new XPathException(message, cause)))
  }

  override def getMessage: String = getCause.getMessage

  def getErrorCode: QName = {
    val cause: Throwable = getCause
    if (cause.isInstanceOf[XPathException]) {
      val code: StructuredQName =
        cause.asInstanceOf[XPathException].getErrorCodeQName
      if (code == null) null else new QName(code)
    } else {
      null
    }
  }

  def getLineNumber: Int = {
    val cause: Throwable = getCause
    if (cause.isInstanceOf[XPathException]) {
      val loc: Location = cause.asInstanceOf[XPathException].getLocator
      if (loc == null) -1 else loc.getLineNumber
    } else {
      -1
    }
  }

  def getSystemId: String = {
    val cause: Throwable = getCause
    if (cause.isInstanceOf[XPathException]) {
      val loc: Location = cause.asInstanceOf[XPathException].getLocator
      if (loc == null) null else loc.getSystemId
    } else {
      null
    }
  }

}
