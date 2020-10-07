////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.lib.ErrorReporter

import java.util.List




trait StaticError {

  /**
    * The error code, as a QName. May be null if no error code has been assigned
    *
    * @return QName
    */
  def getErrorCode: QName

  /**
    * Return the error message  associated with this error
    *
    * @return String
    */
  def getMessage: String

  def getLocation: Location

  /**
    * The URI of the query or stylesheet module in which the error was detected (as a string)
    * May be null if the location of the error is unknown, or if the error is not localized
    * to a specific module, or if the module in question has no known URI (for example, if
    * it was supplied as an anonymous Stream)
    *
    * @return String
    */
  def getModuleUri: String = getLocation.getSystemId

  /**
    * The coloumn number locating the error within a query or stylesheet module
    *
    * @return int
    */
  def getColumnNumber: Int = getLocation.getColumnNumber

  /**
    * The line number locating the error within a query or stylesheet module
    *
    * @return int
    */
  def getLineNumber: Int = getLocation.getLineNumber

  /**
    * Get a name identifying the kind of instruction, in terms meaningful to a user. This method
    * is not used in the case where the instruction name code is a standard name (&lt;1024).
    *
    * @return a name identifying the kind of instruction, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in explain() output displaying the instruction.
    */
  def getInstructionName: String = null

  /**
    * Indicate whether this error is being reported as a warning condition.
    * If so, applications may ignore the condition, though the results may not be as intended.
    *
    * @return boolean
    */
  def isWarning: Boolean

  /**
    * Indicate whether this condition is a type error.
    *
    * @return boolean
    */
  def isTypeError: Boolean

  /**
    * Get the absolute XPath expression that identifies the node within its document
    * where the error occurred, if available
    *
    * @return String - a path expression
    */
  def getPath: String = null

  def setFatal(message: String): Unit

  def getFatalErrorMessage: String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The <b>StaticError</b> interface is retained in Saxon 10.0 as a marker interface so that
  * the methods {@link XsltCompiler#setErrorList(List)} and {@link XQueryCompiler#setErrorList(List)}
  * continue to function.
  *
  * <p>The name is misleading, because all errors including dynamic errors implement this interface.</p>
  */
