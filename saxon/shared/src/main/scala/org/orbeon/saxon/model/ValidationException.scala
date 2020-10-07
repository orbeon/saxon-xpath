////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This exception indicates a failure when validating an instance against a type
 * defined in a schema. It may also be used when validating against a built-in type.
 */

package org.orbeon.saxon.model

import org.orbeon.saxon.om.{AbsolutePath, NodeInfo}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.Navigator


/**
 * Creates a new ValidationException with the given nested
 * exception.
 *
 * @param exception the nested exception
 */
class ValidationException(exception: Exception)
  extends XPathException(exception) {

  private var failure: ValidationFailure = _
  private var message: String = _

  /**
   * Creates a new ValidationException with the given message
   * and nested exception.
   *
   * @param message   the detail message for this exception
   * @param exception the nested exception
   */
  def this(message: String, exception: Exception) = {
    this(exception)
    this.initCause(exception)
    this.message = message
    if (failure != null) {
      failure.setMessage(message)
    }
  }

  /**
   * Create a new ValidationException from a message and a Locator.
   *
   * @param message The error or warning message.
   * @param locator The locator object for the error or warning.
   */
  def this(message: String, locator: Location) = {
    this(new Exception(message))
    this.message = message
    this.setLocator(locator)
    if (failure != null) {
      failure.setMessage(message)
    }
  }


  def this(failure: ValidationFailure) = {
    this(new Exception())
    this.failure = failure
  }

  /**
   * Returns the detail message string of this throwable.
   *
   * @return the detail message string of this { @code Throwable} instance
   *         (which may be { @code null}).
   */
  override def getMessage
  : String = // The message in the exception can't be updated, it can only be set from the constructor.
    if (failure != null) {
      failure.getMessage
    } else {
      this.message
    }

  def getValidationFailure: ValidationFailure =
    if (failure != null) {
      failure
    } else {
      val failure: ValidationFailure = new ValidationFailure(getMessage)
      failure.setErrorCodeQName(getErrorCodeQName)
      failure.setLocator(getLocator)
      failure
    }

  /**
   * Returns the String representation of this Exception
   *
   * @return the String representation of this Exception
   */
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder("ValidationException: ")
    val message: String = getMessage
    if (message != null) {
      sb.append(message)
    }
    sb.toString
  }

  /*@Nullable*/

  def getNode: NodeInfo =
    if (failure != null) {
      failure.getInvalidNode
    } else {
      null
    }

  /*@Nullable*/

  def getPath: String = {
    val ap: AbsolutePath = getAbsolutePath
    if (ap == null) {
      val node: NodeInfo = getNode
      if (node != null) {
        Navigator.getPath(node)
      } else {
        null
      }
    } else {
      ap.getPathUsingAbbreviatedUris
    }
  }

  def getAbsolutePath: AbsolutePath =
    if (failure != null) {
      failure.getPath
    } else {
      null
    }

}

