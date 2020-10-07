////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.parser

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.flwor.Clause

import org.orbeon.saxon.expr.flwor.FLWORExpression

import org.orbeon.saxon.expr.instruct.TraceExpression

import org.orbeon.saxon.trace.TraceableComponent




/**
  * A code injector can be used to add code to the expression tree (for example, diagnostic tracing code)
  * during the process of parsing and tree construction
  */
trait CodeInjector {

  def inject(exp: Expression): Expression = exp

  def process(component: TraceableComponent): Unit = ()

  def injectClause(expression: FLWORExpression, clause: Clause): Clause =
    clause

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
