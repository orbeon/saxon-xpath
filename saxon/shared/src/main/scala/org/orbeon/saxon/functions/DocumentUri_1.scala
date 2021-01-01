////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.DocumentPool

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AnyURIValue

import org.orbeon.saxon.value.AtomicValue

import DocumentUri_1._




object DocumentUri_1 {

  def getDocumentURI(node: NodeInfo, c: XPathContext): AnyURIValue =
    if (node.getNodeKind == Type.DOCUMENT) {
      val o: Any = node.getTreeInfo.getUserData("saxon:document-uri")
      if (o.isInstanceOf[String]) {
        val res = if (o.toString.isEmpty) return null else return new AnyURIValue(o.toString)
        return res
      }
      val controller: Controller = c.getController
      assert(controller != null)
      val pool: DocumentPool = controller.getDocumentPool
      var docURI: String = pool.getDocumentURI(node)
      if (docURI == null) {
        docURI = node.getSystemId
      }
      if (docURI == null) {
        null
      } else if ("" == docURI) {
        null
      } else {
        new AnyURIValue(docURI)
      }
    } else {
      null
    }

}

class DocumentUri_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue =
    getDocumentURI(item.asInstanceOf[NodeInfo], context)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the document-uri() function
  */
