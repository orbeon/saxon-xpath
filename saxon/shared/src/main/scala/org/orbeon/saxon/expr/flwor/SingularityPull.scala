////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext




/**
  * The "singularity" tuple stream delivers a single empty tuple. It is the base tuple stream
  * for the outermost for/let clause in a FLWOR expression
  */
class SingularityPull extends TuplePull {

  private var done: Boolean = false

  /**
    * Move on to the next tuple. Before returning, this method must set all the variables corresponding
    * to the "returned" tuple in the local stack frame associated with the context object
    *
    * @param context the dynamic evaluation context
    * @return true if another tuple has been generated; false if the tuple stream is exhausted. If the
    *         method returns false, the values of the local variables corresponding to this tuple stream
    *         are undefined.
    */
  override def nextTuple(context: XPathContext): Boolean =
    if (done) {
      false
    } else {
      done = true
      true
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
