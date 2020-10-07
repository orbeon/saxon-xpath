////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.PendingUpdateList

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import java.net.URI

import java.net.URISyntaxException




/**
  * Implements the fn:put() function in XQuery Update 1.0.
  */
class Put extends SystemFunction {

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def isUpdatingExpression(): Boolean = true

      override def evaluatePendingUpdates(context: XPathContext,
                                          pul: PendingUpdateList): Unit = {
        val node: NodeInfo =
          getArg(0).evaluateItem(context).asInstanceOf[NodeInfo]
        val kind: Int = node.getNodeKind
        if (kind != Type.ELEMENT && kind != Type.DOCUMENT) {
          throw new XPathException(
            "Node in put() must be a document or element node",
            "FOUP0001",
            context)
        }
        val relative: String = getArg(1).evaluateItem(context).getStringValue
        var abs: String = null
        val resolved: URI =
          ResolveURI.makeAbsolute(relative, getStaticBaseUriString)
        abs = resolved.toString
        pul.addPutAction(node, abs, this)
      }
    }

  /**
    * Evaluate an updating expression, adding the results to a Pending Update List.
    * The default implementation of this method, which is used for non-updating expressions,
    * throws an UnsupportedOperationException
    *
    * @param context the XPath dynamic evaluation context
    * @param pul     the pending update list to which the results should be written
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    throw new XPathException("Dynamic evaluation of fn:put() is not supported")

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
