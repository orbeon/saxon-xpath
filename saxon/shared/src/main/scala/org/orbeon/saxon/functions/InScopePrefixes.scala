////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, NodeInfo, Sequence}
import org.orbeon.saxon.value.{SequenceExtent, StringValue}

import java.util.ArrayList


/**
  * This class implements the XPath 2.0 function fn:in-scope-prefixes()
  */
class InScopePrefixes extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {

    val element  = arguments(0).head.asInstanceOf[NodeInfo]
    val prefixes = element.getAllNamespaces.getPrefixArray
    val result   = new ArrayList[Item]

    for (s <- prefixes)
      result.add(new StringValue(s))
    result.add(new StringValue("xml"))
    SequenceExtent.makeSequenceExtent(result)
  }
}
