////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.flwor.Clause

import org.orbeon.saxon.expr.flwor.FLWORExpression

import org.orbeon.saxon.expr.flwor.TraceClause

import org.orbeon.saxon.expr.instruct.ComponentTracer

import org.orbeon.saxon.expr.instruct.TraceExpression

import org.orbeon.saxon.expr.parser.CodeInjector

import org.orbeon.saxon.expr.parser.ExpressionTool




/**
  * A code injector that wraps every expression (other than a literal) in a TraceExpression, which causes
  * a TraceListener to be notified when the expression is evaluated
  */
class TraceCodeInjector extends CodeInjector {

  override def inject(exp: Expression): Expression =
    if (!(exp.isInstanceOf[TraceExpression]) && isApplicable(exp)) {
      new TraceExpression(exp)
    } else {
      exp
    }

   def isApplicable(exp: Expression): Boolean = false

  override def process(component: TraceableComponent): Unit = {
    val newBody: Expression =
      ExpressionTool.injectCode(component.getBody, this)
    component.setBody(newBody)
    val trace: ComponentTracer = new ComponentTracer(component)
    component.setBody(trace)
  }

  override def injectClause(expression: FLWORExpression, clause: Clause): Clause =
    new TraceClause(expression, clause)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
