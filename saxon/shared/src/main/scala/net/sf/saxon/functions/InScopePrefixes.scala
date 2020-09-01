////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceExtent

import net.sf.saxon.value.StringValue

import java.util.ArrayList

import java.util.List




class InScopePrefixes extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val element: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    val prefixes: Array[String] = element.getAllNamespaces.getPrefixArray
    val result: List[Item] = new ArrayList[Item]()
    for (s <- prefixes) {
      result.add(new StringValue(s))
    }
    result.add(new StringValue("xml"))
    SequenceExtent.makeSequenceExtent(result)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the XPath 2.0 function fn:in-scope-prefixes()
  */
