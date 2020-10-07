////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.trans.XPathException




trait TailCallReturner {

  /*@Nullable*/

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface represents an expression that is capable of being processed leaving tail calls for the
  * calling instruction to deal with.
  */
