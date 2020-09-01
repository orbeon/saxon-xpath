////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Item

import net.sf.saxon.om.LazySequence

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import Expression._




class ConsumingOperand(subExpression: Expression)
    extends UnaryExpression(subExpression) {

  /**
    * Get the usage (in terms of streamability analysis) of the single operand
    *
    * @return the operand usage
    */
   override def getOperandRole(): OperandRole =
    new OperandRole(0, OperandUsage.ABSORPTION)

  /*@NotNull*/

  override def getItemType(): ItemType = getBaseExpression.getItemType

  override def getIntrinsicDependencies(): Int =
    getBaseExpression.getIntrinsicDependencies

  override def computeCardinality(): Int = getBaseExpression.getCardinality

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: ConsumingOperand = new ConsumingOperand(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
    * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
    * This method indicates which of these methods is provided directly. The other methods will always be available
    * indirectly, using an implementation that relies on one of the other methods.
    *
    * @return the implementation method, for example {@link #ITERATE_METHOD} or {@link #EVALUATE_METHOD} or
    * {@link #PROCESS_METHOD}
    */
  override def getImplementationMethod(): Int =
    EVALUATE_METHOD | ITERATE_METHOD

  def evaluate(c: XPathContext): Sequence =
    if (c.getStackFrame.holdsDynamicValue()) {
      c.getStackFrame.popDynamicValue()
    } else {
      new LazySequence(getBaseExpression.iterate(c))
    }

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator =
    evaluate(context).iterate()

  override def evaluateItem(context: XPathContext): Item = evaluate(context).head()

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName(): String = "consume"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("consume", this)
    getBaseExpression.export(destination)
    destination.endElement()
  }

  override def toString: String =
    "consume(" + getBaseExpression.toString + ")"

  override def toShortString(): String =
    "consume(" + getBaseExpression.toShortString() + ")"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This expression is used as a proxy for the consuming operand of an expression such as an
  * arithmetic expression for which there is no explicit streaming support. The actual subexpression
  * is retained, so that it is accessible to compile time operations that walk the expression tree;
  * but at evaluation time, the subexpression is not evaluated in the conventional (pull) way,
  * rather its value is pushed up the tree as a consequence of template inversion.
  *
  * Previously (until 9.6) a SuppliedParameterReference was used for the purpose; but this caused
  * problems with allocation of local variable slots: see bug 2320.
  */
