////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException




trait ExportAgent {

  def export(out: ExpressionPresenter): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An export agent performs the job of exporting an expression to a SEF file. Normally
  * the expression itself acts as its own export agent, and includes an export() method
  * to do the exporting. In a few cases, notably literals containing function items,
  * extra machinery is required to export a value, and a
  * {@link org.orbeon.saxon.functions.hof.UserFunctionReference.BoundUserFunction}
  * in particular includes custom export methods to handle different cases.
  */
