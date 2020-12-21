////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.trans.XPathException




trait TailCall {

  def processLeavingTail(): TailCall

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface representing a Tail Call. This is a package of information passed back by a called
  * instruction to its caller, representing a call (and its arguments) that needs to be made
  * by the caller. This saves stack space by unwinding the stack before making the call.
  */
