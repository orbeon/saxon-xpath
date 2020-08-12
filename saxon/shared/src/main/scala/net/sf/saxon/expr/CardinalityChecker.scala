////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.event.Outputter
import net.sf.saxon.event.TypeCheckingFilter
import net.sf.saxon.expr.parser._
import Expression._
import net.sf.saxon.model.Type
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.om.SequenceTool
import net.sf.saxon.pattern.DocumentNodeTest
import net.sf.saxon.s9api.Location
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.ArrayIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.IntegerValue

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
      var result: Expression = null
      if (sequence.isInstanceOf[Literal] && Cardinality.subsumes(cardinality, SequenceTool.getCardinality(sequence.asInstanceOf[Literal].getValue))) return sequence
      if (sequence.isInstanceOf[Atomizer] && !Cardinality.allowsMany(cardinality)) {
        val base = sequence.asInstanceOf[Atomizer].getBaseExpression
        result = new SingletonAtomizer(base, role, Cardinality.allowsZero(cardinality))
      }
      else result = new CardinalityChecker(sequence, cardinality, role)
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
    var next:Item = null
    while ( {
      (next = seq.next) != null
    }) {
      if ( {
        count += 1; count - 1
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

final class CardinalityChecker private(val sequence: Expression, val cardinality: Int, var role: RoleDiagnostic)

/**
 * Private Constructor: use factory method
 *
 * @param sequence    the base sequence whose cardinality is to be checked
 * @param cardinality the required cardinality
 * @param role        information to be used in error reporting
 */
//computeStaticProperties();
//adoptChildExpression(sequence);
  extends UnaryExpression(sequence) {
  requiredCardinality = cardinality
  private var requiredCardinality = -1

  override  def getOperandRole = OperandRole.SAME_FOCUS_ACTION

  /**
   * Get the required cardinality
   *
   * @return the cardinality required by this checker
   */
  def getRequiredCardinality = requiredCardinality

  /**
   * Type-check the expression
   */
  /*@NotNull*/ @throws[XPathException]
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
   *                    { @link net.sf.saxon.model.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    val base = getBaseExpression
    if (requiredCardinality == StaticProperty.ALLOWS_ZERO_OR_MORE || Cardinality.subsumes(requiredCardinality, base.getCardinality)) return base
    if ((base.getCardinality & requiredCardinality) == 0) {
      val err = new XPathException("The " + role.getMessage + " does not satisfy the cardinality constraints", role.getErrorCode)
      err.setLocation(getLocation)
      err.setIsTypeError(role.isTypeError)
      throw err
    }
    // do cardinality checking before item checking (may avoid the need for a mapping iterator)
    if (base.isInstanceOf[ItemChecker]) {
      val checker = base.asInstanceOf[ItemChecker]
      val other = checker.getBaseExpression
      // change this -> checker -> other to checker -> this -> other
      setBaseExpression(other)
      checker.setBaseExpression(this)
      checker.setParentExpression(null)
      return checker
    }
    this
  }

  /**
   * Set the error code to be returned (this is used when evaluating the functions such
   * as exactly-one() which have their own error codes)
   *
   * @param code the error code to be used
   */
  def setErrorCode(code: String) = role.setErrorCode(code)

  /**
   * Get the RoleLocator, which contains diagnostic information for use if the cardinality check fails
   *
   * @return the diagnostic information
   */
  def getRoleLocator = role

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod = {
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
  override def getIntegerBounds = getBaseExpression.getIntegerBounds

  /**
   * Iterate over the sequence of values
   */
  @throws[XPathException]
  override def iterate(context: XPathContext): SequenceIterator = {
    val base = getBaseExpression.iterate(context)
    // If the base iterator knows how many items there are, then check it now rather than wasting time
    if (base.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER)) {
      val count = base.asInstanceOf[LastPositionFinder].getLength()
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
  /*@Nullable*/ @throws[XPathException]
  override def evaluateItem(context: XPathContext): Item = {
    val iter = getBaseExpression.iterate(context)
    val first = iter.next
    if (first == null) {
      if (!Cardinality.allowsZero(requiredCardinality)) typeError("An empty sequence is not allowed as the " + role.getMessage, role.getErrorCode, context)
      return null
    }
    else {
      if (requiredCardinality == StaticProperty.EMPTY) {
        typeError("An empty sequence is required as the " + role.getMessage, role.getErrorCode, context)
        return null
      }
      val second = iter.next
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
  override def process(output: Outputter, context: XPathContext) = {
    var next = getBaseExpression
    var `type` = Type.ITEM_TYPE
    if (next.isInstanceOf[ItemChecker]) {
      `type` = next.asInstanceOf[ItemChecker].getRequiredType
      next = next.asInstanceOf[ItemChecker].getBaseExpression
    }
    if ((next.getImplementationMethod & PROCESS_METHOD) != 0 && !`type`.isInstanceOf[DocumentNodeTest]) {
      val filter = new TypeCheckingFilter(output)
      filter.setRequiredType(`type`, requiredCardinality, role, getLocation)
      next.process(filter, context)
      try filter.finalCheck()
      catch {
        case e: XPathException =>
          e.maybeSetLocation(getLocation)
          throw e
      }
    }
    else { // Force pull-mode evaluation
      super.process(output, context)
    }
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   *
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   *         or Type.ITEM (meaning not known in advance)
   */
  override def getItemType = getBaseExpression.getItemType

  /**
   * Determine the static cardinality of the expression
   */
  override def computeCardinality = requiredCardinality

  /**
   * Get the static properties of this expression (other than its type). The result is
   * bit-signficant. These properties are used for optimizations. In general, if
   * property bit is set, it is true, but if it is unset, the value is unknown.
   */
  override def computeSpecialProperties = getBaseExpression.getSpecialProperties

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings variable bindings that need to be changed
   */
  override def copy(rebindings: RebindingMap) = {
    val c2 = new CardinalityChecker(getBaseExpression.copy(rebindings), requiredCardinality, role)
    ExpressionTool.copyLocationInfo(this, c2)
    c2
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any) = super.equals(other) && requiredCardinality == other.asInstanceOf[CardinalityChecker].requiredCardinality

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def computeHashCode = super.computeHashCode ^ requiredCardinality

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter) = {
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
  override def toString = {
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

  override def toShortString = getBaseExpression.toShortString

  override def getStreamerName = "CardinalityChecker"

  override def setLocation(id: Location) = super.setLocation(id)
}