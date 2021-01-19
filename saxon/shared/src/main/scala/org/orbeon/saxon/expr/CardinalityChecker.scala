////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.event.{Outputter, TypeCheckingFilter}
import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model.{ItemType, Type}
import org.orbeon.saxon.om.{Item, SequenceIterator, SequenceTool}
import org.orbeon.saxon.pattern.DocumentNodeTest
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.iter.ArrayIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{Cardinality, IntegerValue}

/**
 * A CardinalityChecker implements the cardinality checking of "treat as": that is,
 * it returns the supplied sequence, checking that its cardinality is correct
 */
object CardinalityChecker {
  /**
   * Factory method to construct a CardinalityChecker. The method may create an expression that combines
   * the cardinality checking with the functionality of the underlying expression class
   *
   * @param sequence    the base sequence whose cardinality is to be checked
   * @param cardinality the required cardinality
   * @param role        information to be used in error reporting
   * @return a new Expression that does the CardinalityChecking (not necessarily a CardinalityChecker)
   */
  def makeCardinalityChecker(sequence: Expression, cardinality: Int, role: RoleDiagnostic): Expression = {

    sequence match {
      case literal: Literal if Cardinality.subsumes(cardinality, SequenceTool.getCardinality(literal.getValue)) =>
        return literal
      case _ =>
    }

    val result =
      sequence match {
        case atomizer: Atomizer if ! Cardinality.allowsMany(cardinality) =>
          val base = atomizer.getBaseExpression
          new SingletonAtomizer(base, role, Cardinality.allowsZero(cardinality))
        case _ =>
          new CardinalityChecker(sequence, cardinality, role)
      }
    ExpressionTool.copyLocationInfo(sequence, result)
    result
  }

  /**
   * Show the first couple of items in a sequence in an error message
   *
   * @param seq iterator over the sequence
   * @param max maximum number of items to be shown
   * @return a message display of the contents of the sequence
   */
  def depictSequenceStart(seq: SequenceIterator, max: Int): String = try {
    val sb = new FastStringBuffer(FastStringBuffer.C64)
    var count = 0
    sb.append(" (")
    var next: Item = null
    while ({
      next = seq.next()
      next
    } != null) {
      if ({
        count += 1
        count - 1
      } > 0) sb.append(", ")
      if (count > max) {
        sb.append("...) ")
        return sb.toString
      }
      sb.cat(Err.depict(next))
    }
    sb.append(") ")
    sb.toString
  } catch {
    case e: XPathException =>
      ""
  }
}

final class CardinalityChecker private (
  val sequence            : Expression,
  val requiredCardinality : Int,
  var role                : RoleDiagnostic
) extends UnaryExpression(sequence) {

  override def getOperandRole: OperandRole = OperandRole.SAME_FOCUS_ACTION

  /**
   * Get the required cardinality
   *
   * @return the cardinality required by this checker
   */
  def getRequiredCardinality: Int = requiredCardinality

  /**
   * Type-check the expression
   */
  /*@NotNull*/
  @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val base = getBaseExpression
    if (requiredCardinality == StaticProperty.ALLOWS_ZERO_OR_MORE || Cardinality.subsumes(requiredCardinality, base.getCardinality)) return base
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor     an expression visitor
   * @param contextInfo the static type of "." at the point where this expression is invoked.
   *                    The parameter is set to null if it is known statically that the context item will be undefined.
   *                    If the type of the context item is not known statically, the argument is set to
   *                    { @link org.orbeon.saxon.model.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    val base = getBaseExpression
    if (requiredCardinality == StaticProperty.ALLOWS_ZERO_OR_MORE || Cardinality.subsumes(requiredCardinality, base.getCardinality))
      return base
    if ((base.getCardinality & requiredCardinality) == 0) {
      val err = new XPathException("The " + role.getMessage + " does not satisfy the cardinality constraints", role.getErrorCode)
      err.setLocation(getLocation)
      err.setIsTypeError(role.isTypeError)
      throw err
    }
    // do cardinality checking before item checking (may avoid the need for a mapping iterator)
    base match {
      case checker: ItemChecker =>
        val other = checker.getBaseExpression
        // change this -> checker -> other to checker -> this -> other
        setBaseExpression(other)
        checker.setBaseExpression(this)
        checker.setParentExpression(null)
        return checker
      case _ =>
    }
    this
  }

  /**
   * Set the error code to be returned (this is used when evaluating the functions such
   * as exactly-one which have their own error codes)
   *
   * @param code the error code to be used
   */
  def setErrorCode(code: String): Unit = role.setErrorCode(code)

  /**
   * Get the RoleLocator, which contains diagnostic information for use if the cardinality check fails
   *
   * @return the diagnostic information
   */
  def getRoleLocator: RoleDiagnostic = role

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  def getImplementationMethod: Int = {
    var m = ITERATE_METHOD | PROCESS_METHOD | ITEM_FEED_METHOD
    if (!Cardinality.allowsMany(requiredCardinality)) m |= EVALUATE_METHOD
    m
  }

  /**
   * For an expression that returns an integer or a sequence of integers, get
   * a lower and upper bound on the values of the integers that may be returned, from
   * static analysis. The default implementation returns null, meaning "unknown" or
   * "not applicable". Other implementations return an array of two IntegerValue objects,
   * representing the lower and upper bounds respectively. The values
   * UNBOUNDED_LOWER and UNBOUNDED_UPPER are used by convention to indicate that
   * the value may be arbitrarily large. The values MAX_STRING_LENGTH and MAX_SEQUENCE_LENGTH
   * are used to indicate values limited by the size of a string or the size of a sequence.
   *
   * @return the lower and upper bounds of integer values in the result, or null to indicate
   *         unknown or not applicable.
   */
  override def getIntegerBounds: Array[IntegerValue] = getBaseExpression.getIntegerBounds

  /**
   * Iterate over the sequence of values
   */
  @throws[XPathException]
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = getBaseExpression.iterate(context)
    // If the base iterator knows how many items there are, then check it now rather than wasting time
    if (base.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER)) {
      val count = base.asInstanceOf[LastPositionFinder].getLength
      if (count == 0 && !Cardinality.allowsZero(requiredCardinality)) typeError("An empty sequence is not allowed as the " + role.getMessage, role.getErrorCode, context)
      else if (count == 1 && requiredCardinality == StaticProperty.EMPTY) typeError("The only value allowed for the " + role.getMessage + " is an empty sequence", role.getErrorCode, context)
      else if (count > 1 && !Cardinality.allowsMany(requiredCardinality)) typeError("A sequence of more than one item is not allowed as the " + role.getMessage + CardinalityChecker.depictSequenceStart(base, 2), role.getErrorCode, context)
      return base
    }
    // Otherwise return an iterator that does the checking on the fly
    new CardinalityCheckingIterator(base, requiredCardinality, role, getLocation)
  }

  /**
   * Evaluate as an Item. For this class, this implies checking that the underlying
   * expression delivers a singleton.
   */
  /*@Nullable*/
  @throws[XPathException]
  override def evaluateItem(context: XPathContext): Item = {
    val iter = getBaseExpression.iterate(context)
    val first = iter.next()
    if (first == null) {
      if (!Cardinality.allowsZero(requiredCardinality))
        typeError("An empty sequence is not allowed as the " + role.getMessage, role.getErrorCode, context)
        return null
    } else {
      if (requiredCardinality == StaticProperty.EMPTY) {
        typeError("An empty sequence is required as the " + role.getMessage, role.getErrorCode, context)
        return null
      }
      val second = iter.next()
      if (second != null) {
        val leaders = Array[Item](first, second)
        typeError("A sequence of more than one item is not allowed as the " + role.getMessage + CardinalityChecker.depictSequenceStart(new ArrayIterator[Item](leaders), 2), role.getErrorCode, context)
        return null
      }
    }
    first
  }

  /**
   * Process the instruction, without returning any tail calls
   *
   * @param output  the destination for the result
   * @param context The dynamic context, giving access to the current node,
   */
  @throws[XPathException]
  override def process(output: Outputter, context: XPathContext): Unit = {
    var next = getBaseExpression
    var `type` = Type.ITEM_TYPE
    next match {
      case checker: ItemChecker =>
        `type` = checker.getRequiredType
        next = checker.getBaseExpression
      case _ =>
    }
    if ((next.getImplementationMethod & PROCESS_METHOD) != 0 && !`type`.isInstanceOf[DocumentNodeTest]) {
      val filter = new TypeCheckingFilter(output)
      filter.setRequiredType(`type`, requiredCardinality, role, getLocation)
      next.process(filter, context)
      try
        filter.finalCheck()
      catch {
        case e: XPathException =>
          e.maybeSetLocation(getLocation)
          throw e
      }
    } else {
      // Force pull-mode evaluation
      super.process(output, context)
    }
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   *
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   *         or Type.ITEM (meaning not known in advance)
   */
  override def getItemType: ItemType = getBaseExpression.getItemType

  /**
   * Determine the static cardinality of the expression
   */
  override def computeCardinality: Int = requiredCardinality

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties: Int = getBaseExpression.getSpecialProperties

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings variable bindings that need to be changed
   */
  def copy(rebindings: RebindingMap): Expression = {
    val c2 = new CardinalityChecker(getBaseExpression.copy(rebindings), requiredCardinality, role)
    ExpressionTool.copyLocationInfo(this, c2)
    c2
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any): Boolean = super.equals(other) && requiredCardinality == other.asInstanceOf[CardinalityChecker].requiredCardinality

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def computeHashCode: Int = super.computeHashCode ^ requiredCardinality

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter): Unit = {
    out.startElement("check", this)
    var occ = Cardinality.getOccurrenceIndicator(requiredCardinality)
    if (occ == "") occ = "1"
    out.emitAttribute("card", occ)
    out.emitAttribute("diag", role.save)
    getBaseExpression.`export`(out)
    out.endElement
  }

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form. For subclasses of Expression that represent XPath expressions, the result should always
   * be a string that parses as an XPath 3.0 expression.
   */
  override def toString: String = {
    val operand = getBaseExpression
    requiredCardinality match {
      case StaticProperty.ALLOWS_ONE =>
        "exactly-one(" + operand + ")"
      case StaticProperty.ALLOWS_ZERO_OR_ONE =>
        "zero-or-one(" + operand + ")"
      case StaticProperty.ALLOWS_ONE_OR_MORE =>
        "one-or-more(" + operand + ")"
      case StaticProperty.EMPTY =>
        "must-be-empty(" + operand + ")"
      case _ =>
        "check(" + operand + ")"
    }
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in explain() output displaying the expression.
   */
  override def getExpressionName = "CheckCardinality"
  override def toShortString: String = getBaseExpression.toShortString
  override def getStreamerName = "CardinalityChecker"
  override def setLocation(id: Location): Unit = super.setLocation(id)
}