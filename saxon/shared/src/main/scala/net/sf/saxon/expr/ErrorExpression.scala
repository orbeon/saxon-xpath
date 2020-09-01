////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo
import net.sf.saxon.expr.parser.ExpressionTool
import net.sf.saxon.expr.parser.ExpressionVisitor
import net.sf.saxon.expr.parser.RebindingMap
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.s9api.XmlProcessingError
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.XmlProcessingIncident
import Expression._

/**
 * Error expression: this expression is generated when the supplied expression cannot be
 * parsed, and the containing element enables forwards-compatible processing. It defers
 * the generation of an error message until an attempt is made to evaluate the expression
 */
class ErrorExpression(var exception: XmlProcessingError)

/**
 * Constructor taking an exception. Creating exceptions is expensive, so this
 * constructor should be used only if the exception object already exists.
 *
 * @param exception the error to be thrown when this expression is evaluated
 */
  extends Expression {
  private var original: Expression = null

  /**
   * Create an ErrorExpression, which if evaluated, generates a dynamic error
   *
   * @param message     the error message
   * @param errorCode   the error code
   * @param isTypeError true if this is a type error
   */
  def this(message: String, errorCode: String, isTypeError: Boolean) {
    this(new XmlProcessingIncident(message, errorCode))
    exception.asInstanceOf[XmlProcessingIncident].setTypeError(isTypeError)
  }

  /**
   * This constructor is never executed, but it is used in the expression parser
   * as a dummy so that the Java compiler recognizes parsing methods as always returning
   * a non-null result.
   */
  def this() = this("Unspecified error", "XXXX9999", false)

  /**
   * Get the wrapped exception
   *
   * @return the exception to be thrown when the expression is evaluated
   */
  def getException = exception

  def isTypeError = exception.isTypeError

  def getMessage = exception.getMessage

  def getErrorCodeLocalPart = exception.getErrorCode.getLocalName

  /**
   * Set the original expression (for diagnostics)
   *
   * @param original the expression that this error expression replaces
   */
  def setOriginalExpression(original: Expression) = this.original = original

  /**
   * Type-check the expression.
   */
  /*@NotNull*/ @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo) = this

  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo) = this

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *                                                        { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod = EVALUATE_METHOD | ITERATE_METHOD

  /**
   * Evaluate the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  @throws[XPathException]
  override def evaluateItem(context: XPathContext) = if (exception != null) { // copy the exception for thread-safety, because we want to add context information
    val err = new XPathException(exception.getMessage)
    err.setLocation(exception.getLocation)
    err.maybeSetLocation(getLocation)
    if (exception.getErrorCode != null) err.setErrorCodeQName(exception.getErrorCode.getStructuredQName)
    err.maybeSetContext(context)
    err.setIsTypeError(exception.isTypeError)
    //            err.setIsStaticError(exception.isStaticError());
    //            err.setIsGlobalError(exception.isGlobalError());
    throw err
  }
  else {
    val err = XPathException.fromXmlProcessingError(exception)
    err.setLocation(getLocation)
    err.setXPathContext(context)
    throw err
  }

  /**
   * Iterate over the expression. This always throws the exception registered when the expression
   * was first parsed.
   */
  @throws[XPathException]
  override def iterate(context: XPathContext) = {
    evaluateItem(context)
    null // to fool the compiler
  }

  /**
   * Determine the data type of the expression, if possible
   *
   * @return Type.ITEM (meaning not known in advance)
   */
  override def getItemType = AnyItemType

  /**
   * Determine the static cardinality
   */
  override def computeCardinality = {
    StaticProperty.ALLOWS_ZERO_OR_MORE
    // we return a liberal value, so that we never get a type error reported
    // statically
  }

  override def copy(rebindings: RebindingMap) = {
    val e2 = new ErrorExpression(exception)
    e2.setOriginalExpression(original)
    ExpressionTool.copyLocationInfo(this, e2)
    e2
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in export() output displaying the expression.
   */
  override def getExpressionName = "errorExpr"

  override def toString = if (original != null) original.toString
  else "error(\"" + getMessage + "\")"

  override def toShortString = if (original != null) original.toShortString
  else "error(\"" + getMessage + "\")"

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(destination: ExpressionPresenter) = {
    destination.startElement("error", this)
    destination.emitAttribute("message", exception.getMessage)
    destination.emitAttribute("code", exception.getErrorCode.getLocalName)
    destination.emitAttribute("isTypeErr", if (exception.isTypeError) "0"
    else "1")
    destination.endElement
  }
}