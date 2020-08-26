////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.trans.XPathException




/**
  * Abtract class representing a tuple stream (used to evaluate a FLWOR expression) in pull mode
  * (where the consumer of tuples activates the provider of those tuples)
  */
abstract class TuplePull {

  def nextTuple(context: XPathContext): Boolean

  def close(): Unit = ()
// default implementation takes no action
// default implementation takes no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
