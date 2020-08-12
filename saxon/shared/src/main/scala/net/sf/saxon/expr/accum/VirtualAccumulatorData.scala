////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.accum

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.wrapper.VirtualCopy




/**
  * Holds the values of an accumulator function for a virtual copy of a document (where the accumulator values
  * are copies of those on the underlying document)
  */
class VirtualAccumulatorData(private var realData: IAccumulatorData)
    extends IAccumulatorData {

  def getAccumulator(): Accumulator = realData.getAccumulator

  def getValue(node: NodeInfo, postDescent: Boolean): Sequence = {
    val original: NodeInfo = node.asInstanceOf[VirtualCopy].getOriginalNode
    realData.getValue(original, postDescent)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
