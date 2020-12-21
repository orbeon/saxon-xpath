////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Abstraction representing any class that can act as the container
  * for an expression: either an Operand of a parent expression, or a
  * top-level construct such as a function or template or XQuery expression
  */
package org.orbeon.saxon.expr


trait ExpressionOwner {
  def getChildExpression: Expression
  def setChildExpression(expr: Expression): Unit
}