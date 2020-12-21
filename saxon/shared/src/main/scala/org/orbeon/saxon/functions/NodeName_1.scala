////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.QNameValue

import NodeName_1._




object NodeName_1 {

  def nodeName(node: NodeInfo): QNameValue = {
    if (node.getLocalPart.isEmpty) {
      return null
    }
    new QNameValue(node.getPrefix,
                   node.getURI,
                   node.getLocalPart,
                   BuiltInAtomicType.QNAME)
  }

}

class NodeName_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue =
    nodeName(item.asInstanceOf[NodeInfo])

  override def getCompilerName(): String = "NodeNameFnCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the node-name() function with a single argument
  */
