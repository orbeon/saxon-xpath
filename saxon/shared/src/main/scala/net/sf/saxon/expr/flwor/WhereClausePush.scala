////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.trans.XPathException




/**
  * This class represents the tuple stream delivered as the output of a where clause in a
  * FLWOR expression: that is, it supplies all the tuples in its input stream that satisfy
  * a specified predicate. It does not change the values of any variables in the tuple stream.
  */
class WhereClausePush(outputter: Outputter,
                      var destination: TuplePush,
                      var predicate: Expression)
    extends TuplePush(outputter) {

  /*
   * Process the next tuple.
   */

  override def processTuple(context: XPathContext): Unit = {
    if (predicate.effectiveBooleanValue(context)) {
      destination.processTuple(context)
    }
  }

  /*
   * Notify the end of the tuple stream
   */

  override def close(): Unit = {
    destination.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
