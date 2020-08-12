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
import net.sf.saxon.expr.instruct.Block
import net.sf.saxon.expr.parser._
import net.sf.saxon.model._
import net.sf.saxon.om.Item
import Affinity._
import net.sf.saxon.pattern.CombinedNodeTest
import net.sf.saxon.pattern.DocumentNodeTest
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Cardinality
import scala.collection.JavaConverters._
import java.util
import Expression._

/**
 * A ItemChecker implements the item type checking of "treat as": that is,
 * it returns the supplied sequence, checking that all its items are of the correct type
 */
final class ItemChecker(val sequence: Expression, var requiredItemType: ItemType, var role: RoleDiagnostic) extends UnaryExpression(sequence) {
  /**
   * Get the required type
   *
   * @return the required type of the items in the sequence
   */
  def getRequiredType = requiredItemType

  override  def getOperandRole = OperandRole.SAME_FOCUS_ACTION

  /**
   * Get the RoleLocator (used to construct error messages)
   *
   * @return the RoleLocator
   */
  def getRoleLocator = role

  /**
   * Simplify an expression
   *
   */
  /*@NotNull*/ @throws[XPathException]
  override def simplify: Expression = {
    val operand = getBaseExpression.simplify
    if (requiredItemType.isInstanceOf[AnyItemType]) return operand
    setBaseExpression(operand)
    this
  }

  /**
   * Type-check the expression
   */
  @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val operand = getBaseExpression
    if (operand.isInstanceOf[Block]) { // Do the item-checking on each operand of the block separately (it might not be needed on all items)
      // This is particularly needed for streamability analysis of xsl:map
      val block = operand.asInstanceOf[Block]
      val checkedOperands = new util.ArrayList[Expression]

      for (o <- block.operands.asScala) {
        val checkedOp = new ItemChecker(o.getChildExpression, requiredItemType, role)
        checkedOperands.add(checkedOp)
      }
      val newBlock = new Block(checkedOperands.toArray(new Array[Expression](0)))
      ExpressionTool.copyLocationInfo(this, newBlock)
      return newBlock.typeCheck(visitor, contextInfo)
    }
    // When typeCheck is called a second time, we might have more information...
    val th = getConfiguration.getTypeHierarchy
    val card = operand.getCardinality
    if (card == StaticProperty.EMPTY) { //value is always empty, so no item checking needed
      return operand
    }
    val supplied = operand.getItemType
    val relation = th.relationship(requiredItemType, supplied)
    if ((relation eq Affinity.SAME_TYPE) || (relation eq Affinity.SUBSUMES)) return operand
    else if (relation eq Affinity.DISJOINT) if (requiredItemType == BuiltInAtomicType.STRING && th.isSubType(supplied, BuiltInAtomicType.ANY_URI)) { // URI promotion will take care of this at run-time
      return operand
    }
    else if (Cardinality.allowsZero(card)) if (!operand.isInstanceOf[Literal]) {
      val message = role.composeErrorMessage(requiredItemType, operand, th)
      visitor.getStaticContext.issueWarning("The only value that can pass type-checking is an empty sequence. " + message, getLocation)
    }
    else {
      val message = role.composeErrorMessage(requiredItemType, operand, th)
      val err = new XPathException(message)
      err.setErrorCode(role.getErrorCode)
      err.setLocation(this.getLocation)
      err.setIsTypeError(role.isTypeError)
      throw err
    }
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
   *                    { @link Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    val th = visitor.getConfiguration.getTypeHierarchy
    val rel = th.relationship(requiredItemType, getBaseExpression.getItemType)
    if ((rel eq Affinity.SAME_TYPE) || (rel eq Affinity.SUBSUMES)) return getBaseExpression
    this
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided. This implementation provides both iterate() and
   * process() methods natively.
   */
  override def getImplementationMethod = {
    var m = ITERATE_METHOD | PROCESS_METHOD | ITEM_FEED_METHOD
    if (!Cardinality.allowsMany(getCardinality)) m |= EVALUATE_METHOD
    m
  }

  /**
   * Get the (partial) name of a class that supports streaming of this kind of expression
   *
   * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
   *         or null if there is no such class
   */
  override def getStreamerName = "ItemChecker"

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
  /*@Nullable*/ override def getIntegerBounds = getBaseExpression.getIntegerBounds

  /**
   * Iterate over the sequence of values
   */
  @throws[XPathException]
  override def iterate(context: XPathContext) = {
    val base = getBaseExpression.iterate(context)
    new ItemMappingIterator(base, getMappingFunction(context), true)
  }

  /**
   * Get the mapping function used to implement this item check. This mapping function is applied
   * to each item in the input sequence.
   *
   * @param context The dynamic context used to evaluate the mapping function
   * @return the mapping function. This will be an identity mapping: the output sequence is the same
   *         as the input sequence, unless the dynamic type checking reveals an error.
   */
  def getMappingFunction(context: XPathContext) = new ItemTypeCheckingFunction(requiredItemType, role, getBaseExpression, context.getConfiguration)

  /**
   * Evaluate as an Item.
   */
  @throws[XPathException]
  override def evaluateItem(context: XPathContext): Item = {
    val th = context.getConfiguration.getTypeHierarchy
    val item = getBaseExpression.evaluateItem(context)
    if (item == null) return null
    if (requiredItemType.matches(item, th)) item
    else if (requiredItemType.getUType.subsumes(UType.STRING) && BuiltInAtomicType.ANY_URI.matches(item, th)) item
    else {
      val message = role.composeErrorMessage(requiredItemType, item, th)
      val errorCode = role.getErrorCode
      if ("XPDY0050" == errorCode) { // error in "treat as" assertion
        dynamicError(message, errorCode, context)
      }
      else typeError(message, errorCode, context)
      null
    }
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
    var card = StaticProperty.ALLOWS_ZERO_OR_MORE
    if (next.isInstanceOf[CardinalityChecker]) {
      card = next.asInstanceOf[CardinalityChecker].getRequiredCardinality
      next = next.asInstanceOf[CardinalityChecker].getBaseExpression
    }
    if ((next.getImplementationMethod & PROCESS_METHOD) != 0 && !requiredItemType.isInstanceOf[DocumentNodeTest]) {
      val filter = new TypeCheckingFilter(output)
      filter.setRequiredType(requiredItemType, card, role, getLocation)
      next.process(filter, context)
      filter.finalCheck()
    }
    else { // Force pull-mode evaluation
      super.process(output, context)
    }
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings variable bindings that need to be changed
   */
  override def copy(rebindings: RebindingMap) = {
    val exp = new ItemChecker(getBaseExpression.copy(rebindings), requiredItemType, role)
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
   * Determine the data type of the items returned by the expression
   */
  override def getItemType = {
    val operandType = getBaseExpression.getItemType
    val th = getConfiguration.getTypeHierarchy
    val relationship = th.relationship(requiredItemType, operandType)
    relationship match {
      case OVERLAPS =>
        if (requiredItemType.isInstanceOf[NodeTest] && operandType.isInstanceOf[NodeTest]) new CombinedNodeTest(requiredItemType.asInstanceOf[NodeTest], Token.INTERSECT, operandType.asInstanceOf[NodeTest])
        else { // we don't know how to intersect atomic types, it doesn't actually happen
          requiredItemType
        }
      case SUBSUMES => null
      case SAME_TYPE =>
        // shouldn't happen, but it doesn't matter
        operandType
      case SUBSUMED_BY =>null
      case _ =>
        requiredItemType
    }
  }

  /**
   * Get the static type of the expression as a UType, following precisely the type
   * inference rules defined in the XSLT 3.0 specification.
   *
   * @return the static item type of the expression according to the XSLT 3.0 defined rules
   * @param contextItemType the type of the context item
   */
  override def getStaticUType(contextItemType: UType) = UType.fromTypeCode(requiredItemType.getPrimitiveType)

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any) = super.equals(other) && (requiredItemType eq other.asInstanceOf[ItemChecker].requiredItemType)

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def computeHashCode = super.computeHashCode ^ requiredItemType.hashCode

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter) = {
    out.startElement("treat", this)
    out.emitAttribute("as", AlphaCode.fromItemType(requiredItemType))
    out.emitAttribute("diag", role.save)
    getBaseExpression.`export`(out)
    out.endElement
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in explain() output displaying the expression.
   */
  override def getExpressionName = "treatAs"

  /**
   * The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form, but there is no guarantee that the syntax will actually be true XPath.
   * In the case of XSLT instructions, the toString() method gives an abstracted view of the syntax
   */
  override def toString = {
    val typeDesc = requiredItemType.toString
    "(" + getBaseExpression + ") treat as " + typeDesc
  }

  override def toShortString = getBaseExpression.toShortString
}