////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.Path_1._
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{AxisInfo, Item, NodeInfo}
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.util.{FastStringBuffer, Navigator}
import org.orbeon.saxon.value.{AtomicValue, StringValue}


object Path_1 {

  def makePath(node: NodeInfo, context: XPathContext): StringValue = {
    if (node.getNodeKind == Type.DOCUMENT) {
      return StringValue.makeStringValue("/")
    }
    var fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    val iter: AxisIterator = node.iterateAxis(AxisInfo.ANCESTOR_OR_SELF)
    var n: NodeInfo = null
    while ( {
      n = iter.next()
      n
    } != null) {
      if (n.getParent == null) {
        if (n.getNodeKind == Type.DOCUMENT) {
          return new StringValue(fsb)
        } else {
          fsb.prepend("Q{http://www.w3.org/2005/xpath-functions}root()")
          return new StringValue(fsb)
        }
      }
      val fsb2: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
      n.getNodeKind match {
        case Type.DOCUMENT => return new StringValue(fsb)
        case Type.ELEMENT =>
          fsb2.append("/Q{")
          fsb2.append(n.getURI)
          fsb2.append("}")
          fsb2.append(n.getLocalPart)
          fsb2.append("[" + Navigator.getNumberSimple(n, context) + "]")
          fsb2.append(fsb)
          fsb = fsb2
        case Type.ATTRIBUTE =>
          fsb2.append("/@")
          var attURI: String = n.getURI
          if ("" != attURI) {
            fsb2.append("Q{")
            fsb2.append(attURI)
            fsb2.append("}")
          }
          fsb2.append(n.getLocalPart)
          fsb2.append(fsb)
          fsb = fsb2
        case Type.TEXT =>
          fsb2.append("/text()[")
          fsb2.append(Navigator.getNumberSimple(n, context).toString + "]")
          fsb2.append(fsb)
          fsb = fsb2
        case Type.COMMENT =>
          fsb2.append("/comment()[")
          fsb2.append(Navigator.getNumberSimple(n, context).toString + "]")
          fsb2.append(fsb)
          fsb = fsb2
        case Type.PROCESSING_INSTRUCTION =>
          fsb2.append("/processing-instruction(")
          fsb2.append(n.getLocalPart)
          fsb2.append(")[")
          fsb2.append(Navigator.getNumberSimple(n, context).toString + "]")
          fsb2.append(fsb)
          fsb = fsb2
        case Type.NAMESPACE =>
          fsb2.append("/namespace::")
          if (n.getLocalPart.isEmpty) {
            fsb2.append("*[Q{" + NamespaceConstant.FN + "}local-name()=\"\"]")
          } else {
            fsb.append(n.getLocalPart)
          }
          fsb2.append(fsb)
          fsb = fsb2
        case _ => throw new AssertionError()

      }
    }
// should not reach here...
    fsb.prepend("Q{http://www.w3.org/2005/xpath-functions}root()")
    new StringValue(fsb)
  }

}

/**
  * Implement the fn:path function with one argument
  */
class Path_1 extends ScalarSystemFunction {
  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    makePath(arg.asInstanceOf[NodeInfo], context)
}