////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.accum

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException




/**
  * Represents the values of an accumulator whose evaluation has failed. The error is retained
  * until referenced using accumulator-before() or accumulator-after().
  */
class FailedAccumulatorData(private var acc: Accumulator,
                            private var error: XPathException)
    extends IAccumulatorData {

  override def getAccumulator(): Accumulator = acc

  override def getValue(node: NodeInfo, postDescent: Boolean): Sequence =
    throw error

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
