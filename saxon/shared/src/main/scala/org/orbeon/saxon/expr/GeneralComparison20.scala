////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.{ExpressionTool, RebindingMap, Token}


/**
 * The class GeneralComparison20 specializes GeneralComparison for the case where
 * the comparison is done with 2.0 semantics (i.e. with backwards compatibility off).
 * It differs from the superclass in that it will never turn the expression into
 * a GeneralComparison10, which could lead to non-terminating optimizations
 */
class GeneralComparison20(p0: Expression, op: Int, p1: Expression) extends GeneralComparison(p0, op, p1) {

  /*@NotNull*/
  def copy(rebindings: RebindingMap): Expression = {
    val gc =
      new GeneralComparison20(
        getLhsExpression.copy(rebindings),
        op,
        getRhsExpression.copy(rebindings)
      )
    ExpressionTool.copyLocationInfo(this, gc)
    gc.setRetainedStaticContext(getRetainedStaticContext)
    gc.comparer              = comparer
    gc.singletonOperator     = singletonOperator
    gc.needsRuntimeCheck     = needsRuntimeCheck
    gc.comparisonCardinality = comparisonCardinality
    gc
  }

  override def getInverseComparison: GeneralComparison = {
    val gc =
      new GeneralComparison20(
        getRhsExpression,
        Token.inverse(op),
        getLhsExpression
      )
    gc.setRetainedStaticContext(getRetainedStaticContext)
    gc
  }
}
