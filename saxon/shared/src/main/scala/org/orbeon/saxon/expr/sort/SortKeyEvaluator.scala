////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue




trait SortKeyEvaluator {

  /*@Nullable*/

  def evaluateSortKey(n: Int, context: XPathContext): AtomicValue

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Callback interface used to evaluate sort keys. An instance of this class is passed to the
  * SortedIterator, and is used whenever a sort key value needs to be computed.
  */
