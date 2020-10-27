package org.orbeon.saxon.s9api

import org.orbeon.saxon.trans.XPathException


class SaxonApiException(cause: Throwable) extends Exception(cause) {

  def this(message: String) =
    this(new Exception(new XPathException(message)))

  def this(message: String, cause: Throwable) =
    this(new Exception(new XPathException(message, cause)))

  override def getMessage: String = getCause.getMessage

  def getErrorCode: QName =
    getCause match {
      case xpe: XPathException =>
        val code = xpe.getErrorCodeQName
        if (code == null)
          null
        else
          new QName(code)
      case _ =>
        null
    }

  def getLineNumber: Int =
    getCause match {
      case xpe: XPathException =>
        val loc = xpe.getLocator
        if (loc == null)
          -1
        else
          loc.getLineNumber
      case _ =>
        -1
    }

  def getSystemId: String =
    getCause match {
      case xpe: XPathException =>
        val loc = xpe.getLocator
        if (loc == null)
          null
        else
          loc.getSystemId
      case _ =>
        null
    }
}
