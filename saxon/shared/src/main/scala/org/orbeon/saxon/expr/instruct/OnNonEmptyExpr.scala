////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor, RebindingMap}
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trace.ExpressionPresenter


/**
  * A compiled xsl:on-non-empty instruction.
  */
class OnNonEmptyExpr(base: Expression)
  extends UnaryExpression(base) {

  /**
    * Ask whether this expression is an instruction. In XSLT streamability analysis this
    * is used to distinguish constructs corresponding to XSLT instructions from other constructs.
    *
    * @return true if this construct originates as an XSLT instruction
    */
  override def isInstruction: Boolean = true

  /**
    * Get the usage (in terms of streamability analysis) of the single operand
    *
    * @return the operand usage
    */
  override def getOperandRole: OperandRole =
    new OperandRole(0, OperandUsage.TRANSMISSION)

  /*@NotNull*/
  def copy(rebindings: RebindingMap): Expression =
    new OnNonEmptyExpr(getBaseExpression.copy(rebindings))

  /**
    * Determine the intrinsic dependencies of an expression, that is, those which are not derived
    * from the dependencies of its subexpressions. For example, position has an intrinsic dependency
    * on the context position, while (position+1) does not. The default implementation
    * of the method returns 0, indicating "no dependencies".
    *
    * @return a set of bit-significant flags identifying the "intrinsic"
    * dependencies. The flags are documented in class org.orbeon.saxon.value.StaticProperty
    */
  override def getIntrinsicDependencies: Int =
    StaticProperty.HAS_SIDE_EFFECTS // suppress optimizations such as loop-lifting

  /**
    * Ask whether common subexpressions found in the operands of this expression can
    * be extracted and evaluated outside the expression itself. The result is irrelevant
    * in the case of operands evaluated with a different focus, which will never be
    * extracted in this way, even if they have no focus dependency.
    *
    * @return false for this kind of expression
    */
  override def allowExtractingCommonSubexpressions(): Boolean = false

  /*@NotNull*/
  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    this
  }

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    if (visitor.isOptimizeForStreaming) {
      visitor.obtainOptimizer().makeCopyOperationsExplicit(this, getOperand)
    }
    this
  }

  /**
    * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
    * This method indicates which of these methods is provided directly. The other methods will always be available
    * indirectly, using an implementation that relies on one of the other methods.
    *
    * @return the implementation method, for example {@link #ITERATE_METHOD} or {@link #EVALUATE_METHOD} or
    * {@link #PROCESS_METHOD}
    */
  def getImplementationMethod: Int = getBaseExpression.getImplementationMethod

  override def process(output: Outputter, context: XPathContext): Unit =
    getBaseExpression.process(output, context)

  override def iterate(context: XPathContext): SequenceIterator =
    getBaseExpression.iterate(context)

  override def getExpressionName: String = "onNonEmpty"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("onNonEmpty", this)
    getBaseExpression.export(out)
    out.endElement()
  }

  /**
    * Get the (partial) name of a class that supports streaming of this kind of expression
    *
    * @return the partial name of a class that can be instantiated to provide streaming support in Saxon-EE,
    * or null if there is no such class
    */
  override def getStreamerName: String = "OnNonEmpty"
}
