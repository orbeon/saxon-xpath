////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.sort.AtomicComparer




/**
  * Interface implemented by expressions that perform a comparison
  */
trait ComparisonExpression {

  def getAtomicComparer: AtomicComparer

  def getSingletonOperator: Int

  def getLhs: Operand

  def getRhs: Operand

  def getLhsExpression: Expression

  def getRhsExpression: Expression

  def convertsUntypedToOther(): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
