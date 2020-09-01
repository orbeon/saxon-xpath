////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.Token

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue




abstract class CompareToConstant(p0: Expression)
    extends UnaryExpression(p0)
    with ComparisonExpression {

   var operator: Int = _

   override def getOperandRole(): OperandRole =
    OperandRole.SINGLE_ATOMIC

  def getLhsExpression(): Expression = getBaseExpression

  def getLhs(): Operand = getOperand

  def getRhsExpression(): Expression

  def getRhs(): Operand =
    new Operand(this, getRhsExpression, OperandRole.SINGLE_ATOMIC)

  def getComparisonOperator: Int = operator

  def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def computeSpecialProperties(): Int = StaticProperty.NO_NODES_NEWLY_CREATED

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    if (getLhsExpression.isInstanceOf[Literal]) {
      Literal.makeLiteral(BooleanValue.get(effectiveBooleanValue(null)), this)
    }
    this
  }

  /*@NotNull*/

  override def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

  def getSingletonOperator(): Int = operator

  def convertsUntypedToOther(): Boolean = true

  def interpretComparisonResult(c: Int): Boolean = operator match {
    case Token.FEQ => c == 0
    case Token.FNE => c != 0
    case Token.FGT => c > 0
    case Token.FLT => c < 0
    case Token.FGE => c >= 0
    case Token.FLE => c <= 0
    case _ =>
      throw new UnsupportedOperationException("Unknown operator " + operator)

  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements a comparison of a computed value to a literal constant using one of the operators
  * eq, ne, lt, gt, le, ge. The semantics are identical to ValueComparison, but this is a fast path for an
  * important common case. Different subclasses handle different types of constant.
  */
