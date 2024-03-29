////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor}


/**
  * An EagerLetExpression is the same as a LetExpression except that the variable is evaluated using
  * eager evaluation rather than lazy evaluation. This is used when performing diagnostic tracing.
  */
class EagerLetExpression extends LetExpression {

  /*@NotNull*/
  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e == this)
      this.evaluator = ExpressionTool.eagerEvaluator(getSequence)
    e
  }
}
