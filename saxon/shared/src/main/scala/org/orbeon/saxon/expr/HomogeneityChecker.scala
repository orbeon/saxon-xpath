////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.expr.sort.DocumentSorter
import org.orbeon.saxon.model.Affinity
import org.orbeon.saxon.model.ErrorType
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.model.TypeHierarchy
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.pattern.AnyNodeTest
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.HomogeneityCheckerIterator

/**
 * This class is an expression that does a run-time check of the result of a "/" expression
 * to ensure that (a) the results consists entirely of atomic values and function items, or entirely of nodes,
 * and (b) if the results are nodes, then they are deduplicated and sorted into document order.
 */
class HomogeneityChecker(val base: Expression) extends UnaryExpression(base) {
  override  def getOperandRole = OperandRole.INSPECT

  /**
   * Type-check the expression. Default implementation for unary operators that accept
   * any kind of operand
   */
  /*@NotNull*/ @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    if (getBaseExpression.isInstanceOf[HomogeneityChecker])
      return getBaseExpression.typeCheck(visitor, contextInfo)
    getOperand.typeCheck(visitor, contextInfo)
    val th = visitor.getConfiguration.getTypeHierarchy
    val `type` = getBaseExpression.getItemType
    if (`type` == ErrorType)
      return Literal.makeEmptySequence
    val rel = th.relationship(`type`, AnyNodeTest.getInstance)
    if (rel eq Affinity.DISJOINT) { // expression cannot return nodes, so this checker is redundant
      // code deleted by bug 4298
      //            if (getBaseExpression() instanceof SlashExpression && ((SlashExpression) getBaseExpression()).getLeadingSteps() instanceof SlashExpression &&
      //                    (((SlashExpression) getBaseExpression()).getLeadingSteps().getSpecialProperties() & StaticProperty.ORDERED_NODESET) == 0) {
      //                DocumentSorter ds = new DocumentSorter(((SlashExpression) getBaseExpression()).getLeadingSteps());
      //                SlashExpression se = new SlashExpression(ds, ((SlashExpression) getBaseExpression()).getLastStep());
      //                ExpressionTool.copyLocationInfo(this, se);
      //                return se;
      //            } else {
      return getBaseExpression
      //            }
    } else if ((rel eq Affinity.SAME_TYPE) || (rel eq Affinity.SUBSUMED_BY)) { // expression always returns nodes, so replace this expression with a DocumentSorter
      val savedBase = getBaseExpression
      val parent = getParentExpression
      getOperand.detachChild()
      val ds = new DocumentSorter(savedBase)
      ExpressionTool.copyLocationInfo(this, ds)
      ds.setParentExpression(parent)
      //ds.verifyParentPointers();
      return ds
    }
    this
  }

  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    if (getBaseExpression.isInstanceOf[HomogeneityChecker]) return getBaseExpression.optimize(visitor, contextInfo)
    super.optimize(visitor, contextInfo)
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings
   */
  override def copy(rebindings: RebindingMap) = {
    val hc = new HomogeneityChecker(getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, hc)
    hc
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *                                                        { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod = Expression.ITERATE_METHOD

  /**
   * Iterate the path-expression in a given context
   *
   * @param context the evaluation context
   */
  @throws[XPathException]
  override def iterate(context: XPathContext) = { // This class delivers the result of the path expression in unsorted order,
    // without removal of duplicates. If sorting and deduplication are needed,
    // this is achieved by wrapping the path expression in a DocumentSorter
    val base = getBaseExpression.iterate(context)
    new HomogeneityCheckerIterator(base, getLocation)
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in explain() output displaying the expression.
   */
  override def getExpressionName = "homCheck"
}