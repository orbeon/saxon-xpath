////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.SingletonIterator

import SingletonIntersectExpression._




object SingletonIntersectExpression {

  def containsNode(iter: SequenceIterator, m: NodeInfo): Boolean = {
    var n: NodeInfo = null
    while ((n = iter.next().asInstanceOf[NodeInfo]) != null) if (n == m) {
      iter.close()
      true
    }
    false
  }

}

/**
  * This expression is equivalent to (A intersect B) in the case where A has cardinality
  * zero-or-one. This is handled as a special case because the standard sort-merge algorithm
  * involves an unnecessary sort on B.
  */
class SingletonIntersectExpression(p1: Expression, op: Int, p2: Expression)
    extends VennExpression(p1, op, p2) {

  /**
    * Simplify the expression
    *
    */
  override def simplify(): Expression = this

  /**
    * Perform optimisation of an expression and its subexpressions.
    * <p>This method is called after all references to functions and variables have been resolved
    * to the declaration of the function or variable, and after all type checking has been done.</p>
    *
    * @param visitor         an expression visitor
    * @param contextItemType the static type of "." at the point where this expression is invoked.
    *                        The parameter is set to null if it is known statically that the context item will be undefined.
    *                        If the type of the context item is not known statically, the argument is set to
    *                        {@link net.sf.saxon.model.Type#ITEM_TYPE}
    * @return the original expression, rewritten if appropriate to optimize execution
    * @throws net.sf.saxon.trans.XPathException
    *          if an error is discovered during this phase
    *          (typically a type error)
    */
  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression =
    this

  /*@NotNull*/

  override def copy(rebindings: RebindingMap): Expression = {
    val exp: SingletonIntersectExpression = new SingletonIntersectExpression(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /*@NotNull*/

  override def iterate(c: XPathContext): SequenceIterator = {
    val m: NodeInfo = getLhsExpression.evaluateItem(c).asInstanceOf[NodeInfo]
    if (m == null) {
      EmptyIterator.getInstance
    }
    val iter: SequenceIterator = getRhsExpression.iterate(c)
    var n: NodeInfo = null
    while ((n = iter.next().asInstanceOf[NodeInfo]) != null) if (n == m) {
      SingletonIterator.makeIterator(m)
    }
    EmptyIterator.getInstance
  }

  override def effectiveBooleanValue(c: XPathContext): Boolean = {
    val m: NodeInfo = getLhsExpression.evaluateItem(c).asInstanceOf[NodeInfo]
    m != null && containsNode(getRhsExpression.iterate(c), m)
  }

  override def getExpressionName(): String = "singleton-intersect"

   override def displayOperator(): String = "among"

  /**
    * Get the element name used to identify this expression in exported expression format
    *
    * @return the element name used to identify this expression
    */
   override def tag(): String = "among"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
