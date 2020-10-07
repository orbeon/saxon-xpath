package org.orbeon.saxon.model

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

class SchemaException(message: String)
  extends XPathException(message) {

  def this(message: String, locator: Location) {
    this(message)
    new XPathException(message, null, locator)
  }

  def this(exception: Throwable) = {
    this("")
    new XPathException(exception)
  }

  def this(message: String, exception: Throwable) = {

    this(message)
    new XPathException(message, exception)
  }

}
