////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ReversibleIterator




class LastItemExpression(base: Expression) extends SingleItemFilter(base) {

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: LastItemExpression = new LastItemExpression(
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
  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  /*@Nullable*/

  override def evaluateItem(context: XPathContext): Item = {
    var results: Item = null
    val forwards: SequenceIterator = getBaseExpression.iterate(context)
    if (forwards.isInstanceOf[ReversibleIterator]) {
      results = forwards.asInstanceOf[ReversibleIterator].getReverseIterator.next()
    } else {
      var current: Item = null
      while (true) {
        val item: Item = forwards.next()
        if (item == null) {
          results = current
        }
        current = item
        results = current
      }
    }
    results
  }

  override def getExpressionName: String = "lastOf"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A LastItemExpression returns the last item in the sequence returned by a given
  * base expression. The evaluation strategy is to read the input sequence with a one-item lookahead.
  */
