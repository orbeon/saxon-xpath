////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.BooleanValue

class OrExpression(val p1: Expression, val p2: Expression)

/**
 * Construct a boolean OR expression
 *
 * @param p1 the first operand
 * @param p2 the second operand
 */
  extends BooleanExpression(p1, Token.OR, p2) {
  override  def preEvaluate = if (Literal.hasEffectiveBooleanValue(getLhsExpression, value = true) || Literal.hasEffectiveBooleanValue(getRhsExpression, value = true)) { // A or true() => true()
    // true() or B => true()
    Literal.makeLiteral(BooleanValue.TRUE, this)
  }
  else if (Literal.hasEffectiveBooleanValue(getLhsExpression, value = false)) { // false() or B => B
    forceToBoolean(getRhsExpression)
  }
  else if (Literal.hasEffectiveBooleanValue(getRhsExpression, value = false)) { // A or false() => A
    forceToBoolean(getLhsExpression)
  }
  else this

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor         an expression visitor
   * @param contextItemType the static type of "." at the point where this expression is invoked.
   *                        The parameter is set to null if it is known statically that the context item will be undefined.
   *                        If the type of the context item is not known statically, the argument is set to
   *                        { @link org.orbeon.saxon.model.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException if an error is discovered during this phase
   *                        (typically a type error)
   */
  /*@NotNull*/ @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e ne this) return e
    // If this is a top-level or-expression then try to replace multiple branches with a general comparison
    if (!getParentExpression.isInstanceOf[OrExpression]) {
      val e2 = visitor.obtainOptimizer.tryGeneralComparison(visitor, contextItemType, this)
      if (e2 != null && (e2 ne this)) return e2
    }
    this
  }

  override def getCost = { // Assume the RHS will be evaluated 50% of the time
    getLhsExpression.getCost + getRhsExpression.getCost / 2
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings
   */
  override def copy(rebindings: RebindingMap) = {
    val exp = new OrExpression(getLhsExpression.copy(rebindings), getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
   * Return the negation of this boolean expression, that is, an expression that returns true
   * when this expression returns false, and vice versa
   *
   * @return the negation of this expression
   */
  override def negate = { // Apply de Morgan's laws
    // not(A or B) => not(A) and not(B)
    val not0 = SystemFunction.makeCall("not", getRetainedStaticContext, getLhsExpression)
    val not1 = SystemFunction.makeCall("not", getRetainedStaticContext, getRhsExpression)
    val result = new AndExpression(not0, not1)
    ExpressionTool.copyLocationInfo(this, result)
    result
  }

  /**
   * Get the element name used to identify this expression in exported expression format
   *
   * @return the element name used to identify this expression
   */
  override  def tag = "or"

  /**
   * Evaluate as a boolean.
   */
  @throws[XPathException]
  override def effectiveBooleanValue(c: XPathContext) = getLhsExpression.effectiveBooleanValue(c) || getRhsExpression.effectiveBooleanValue(c)
}