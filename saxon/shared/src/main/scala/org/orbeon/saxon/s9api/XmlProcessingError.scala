////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.s9api.HostLanguage.HostLanguage


/**
  * The <b>XmlProcessingError</b> class contains information about an error detected during
  * compilation or execution of a stylesheet, query, XPath expression, or schema
  *
  * The interface extends `StaticError` so that
  * the methods `XsltCompiler` and {@link XQueryCompiler#setErrorList(List)}
  * continue to function. It is <b>not</b> the case, however, that every `XmlProcessingError`
  * is a static error.
  */
trait XmlProcessingError extends StaticError {
  def getHostLanguage: HostLanguage
  def isStaticError: Boolean
  def isTypeError: Boolean
  def getErrorCode: QName
  def getMessage: String
  def getLocation: Location
  override def getModuleUri: String = getLocation.getSystemId
  def isWarning: Boolean
  def getPath: String
  def getCause: Throwable
  def asWarning(): XmlProcessingError
  def isAlreadyReported: Boolean
  def setAlreadyReported(reported: Boolean): Unit
}
