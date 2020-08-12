////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Cardinality




abstract class SingleItemFilter(base: Expression) extends UnaryExpression(base) {

   def getOperandRole(): OperandRole = OperandRole.SAME_FOCUS_ACTION

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    val base: Expression = getBaseExpression
    if (!Cardinality.allowsMany(base.getCardinality)) {
      base
    }
    super.optimize(visitor, contextInfo)
  }

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_ONE

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SingleItemFilter is an expression that selects zero or one items from a supplied sequence
  */
