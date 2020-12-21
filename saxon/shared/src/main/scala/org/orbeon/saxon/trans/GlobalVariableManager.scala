////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.instruct.GlobalVariable




trait GlobalVariableManager {

  def getEquivalentVariable(select: Expression): GlobalVariable

  def addGlobalVariable(variable: GlobalVariable): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface provides access to a collection of global variables. This abstraction is used by the optimizer
  * to handle the rather different ways that global variables are managed in XSLT and XQuery, as a result of
  * XSLT packaging.
  */
// Copyright (c) 2018-2020 Saxonica Limited
