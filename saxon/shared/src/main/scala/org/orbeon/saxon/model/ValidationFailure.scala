////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import java.util.{ArrayList, Collections, List}

import javax.xml.transform.SourceLocator
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.lib.{Invalidity, NamespaceConstant}
import org.orbeon.saxon.om.{AbsolutePath, NodeInfo, StructuredQName}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.AtomicValue

import scala.beans.BeanProperty


/**
 * This exception indicates a failure when validating an instance against a type
 * defined in a schema.
 *
 * This class holds the same information as a ValidationException, except that it is not an exception,
 * and does not carry system overheads such as a stack trace. It is used because operations such as "castable",
 * and validation of values in a union, cause validation failures on a success path and it is costly to throw,
 * or even to create, exception objects on a success path.
 */
object ValidationFailure {

  /**
    * Creates a new ValidationFailure with the given nested
    * exception.
    *
    * @param exception the nested exception
    */
  def fromException(exception: Exception): ValidationFailure =
    exception match {
      case validationException: ValidationException =>
        validationException.getValidationFailure
      case xpathException: XPathException =>
        val failure = new ValidationFailure(
          exception.getMessage)
        if (xpathException.getErrorCodeQName == null)
          failure.setErrorCode("FORG0001")
        else
          failure.setErrorCodeQName(xpathException.getErrorCodeQName)
        failure.setLocator(xpathException.getLocator)
        failure
      case _ =>
        new ValidationFailure(exception.getMessage)
    }
}

/**
 * Creates a new ValidationException with the given message.
 *
 * @param message the message for this Exception
 */
class ValidationFailure(@BeanProperty var message: String)
    extends Location
    with ConversionResult
    with Invalidity {

  private var systemId     : String = _
  private var publicId     : String = _
  private var lineNumber   : Int    = -1
  private var columnNumber : Int    = -1

  @BeanProperty
  var path: AbsolutePath = _

  @BeanProperty
  var contextPath: AbsolutePath = _

  @BeanProperty
  var invalidNode: NodeInfo = _

  private var offendingNodes: List[NodeInfo] = _

  @BeanProperty
  var schemaPart: Int = -1

  @BeanProperty
  var constraintName: String = _

  private var clause: String = _

  @BeanProperty
  var schemaType: SchemaType = _

  /*@Nullable*/
  private var errorCode: StructuredQName = new StructuredQName("err", NamespaceConstant.ERR, "FORG0001")

  private var exception: ValidationException = _

  var hasBeenReported: Boolean = _

  def setConstraintReference(schemaPart: Int,
                             constraintName: String,
                             clause: String): Unit = {
    this.schemaPart = schemaPart
    this.constraintName = constraintName
    this.clause = clause
  }

  def setConstraintReference(e: ValidationFailure): Unit = {
    schemaPart = e.schemaPart
    constraintName = e.constraintName
    clause = e.clause
  }

  def getConstraintClauseNumber: String = clause

  /*@NotNull*/

  def getConstraintReference: String = constraintName + '.' + clause

  /*@Nullable*/

  def getConstraintReferenceMessage: String = {
    if (schemaPart == -1)
      return null

    "See http://www.w3.org/TR/xmlschema11-" + schemaPart +
      "/#" +
      constraintName +
      " clause " +
      clause
  }

  def addOffendingNode(node: NodeInfo): Unit = {
    if (offendingNodes == null)
      offendingNodes = new ArrayList[NodeInfo]()
    offendingNodes.add(node)
  }

  def getOffendingNodes: List[NodeInfo] =
    if (offendingNodes == null)
      Collections.emptyList()
    else
      offendingNodes

  /**
    * Returns the String representation of this Exception
    *
    * @return the String representation of this Exception
    */
  override def toString: String = {
    val sb = new FastStringBuffer("ValidationException: ")
    val message = getMessage
    if (message != null)
      sb.append(message)
    sb.toString
  }

  def getPublicId: String = {
    val loc = getLocator
    if (publicId == null && loc != null && loc != this)
      loc.getPublicId
    else
      publicId
  }

  def getSystemId: String = {
    val loc = getLocator
    if (systemId == null && loc != null && loc != this)
      loc.getSystemId
    else
      systemId
  }

  def getLineNumber: Int = {
    val loc = getLocator
    if (lineNumber == -1 && loc != null && loc != this)
      loc.getLineNumber
    else
      lineNumber
  }

  def getColumnNumber: Int = {
    val loc = getLocator
    if (columnNumber == -1 && loc != null && loc != this)
      loc.getColumnNumber
    else
      columnNumber
  }

  /**
    * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
    * should not be saved for later use. The result of this operation holds the same location information,
    * but in an immutable form.
    */
  def saveLocation: Location = new Loc(this)

  def setPublicId(id: String): Unit =
    publicId = id

  def setSystemId(id: String): Unit =
    systemId = id

  def setLineNumber(line: Int): Unit =
    lineNumber = line

  def setColumnNumber(column: Int): Unit =
    columnNumber = column

  def setLocator(locator: SourceLocator): Unit =
    if (locator != null) {
      this.publicId = locator.getPublicId
      this.systemId = locator.getSystemId
      this.lineNumber = locator.getLineNumber
      this.columnNumber = locator.getColumnNumber
    }

  def setSourceLocator(locator: SourceLocator): Unit =
    if (locator != null) {
      this.publicId = locator.getPublicId
      this.systemId = locator.getSystemId
      this.lineNumber = locator.getLineNumber
      this.columnNumber = locator.getColumnNumber
    }

  /*@NotNull*/
  def getLocator: Location = this

  def setErrorCode(errorCode: String): Unit =
    this.errorCode =
      if (errorCode == null)
        null
      else
        new StructuredQName("err", NamespaceConstant.ERR, errorCode)

  def setErrorCodeQName(errorCode: StructuredQName): Unit =
    this.errorCode = errorCode

  /**
    * Get the error code associated with the validity error. This is relevant only when validation
    * is run from within XSLT or XQuery, which define different error codes for validition errors
    *
    * @return the error code associated with the error, if any. The error is returned as a simple
    * string if it is in the standard error namespace, or as an EQName (that is Q{uri}local) otherwise.
    */
  def getErrorCode: String =
    if (errorCode == null)
      null
    else if (errorCode.hasURI(NamespaceConstant.ERR))
      errorCode.getLocalPart
    else
      errorCode.getEQName

  /*@Nullable*/
  def getErrorCodeQName: StructuredQName = errorCode

  /*@NotNull*/
  def makeException(): ValidationException = {
    if (exception != null) {
      exception.maybeSetLocation(this)
      return exception
    }
    val ve = new ValidationException(this)
    if (errorCode == null)
      ve.setErrorCode("FORG0001")
    else
      ve.setErrorCodeQName(errorCode)
    ve.setHasBeenReported(hasBeenReported)
    exception = ve
    ve
  }

  /*@NotNull*/
  def asAtomic(): AtomicValue = throw makeException()

  def setHasBeenReported(reported: Boolean): Unit = {
    hasBeenReported = reported
    if (exception != null)
      exception.setHasBeenReported(reported)
  }

  def getValidationLocationText: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    val valPath = getAbsolutePath
    if (valPath != null) {
      fsb.append("Validating ")
      fsb.append(valPath.getPathUsingPrefixes)
      if (valPath.getSystemId != null) {
        fsb.append(" in ")
        fsb.append(valPath.getSystemId)
      }
    }
    fsb.toString
  }

  def getContextLocationText: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    val contextPath = getContextPath
    if (contextPath != null) {
      fsb.append("Currently processing ")
      fsb.append(contextPath.getPathUsingPrefixes)
      if (contextPath.getSystemId != null) {
        fsb.append(" in ")
        fsb.append(contextPath.getSystemId)
      }
    }
    fsb.toString
  }

  def getAbsolutePath: AbsolutePath =
    if (path != null)
      path
    else
      null
}
