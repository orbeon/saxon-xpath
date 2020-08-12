////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.QNameValue

import NodeName_1._




object NodeName_1 {

  def nodeName(node: NodeInfo): QNameValue = {
    if (node.getLocalPart.isEmpty) {
      null
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
