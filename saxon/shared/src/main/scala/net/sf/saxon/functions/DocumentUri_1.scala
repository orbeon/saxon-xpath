////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.Type

import net.sf.saxon.om.DocumentPool

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.AtomicValue

import DocumentUri_1._




object DocumentUri_1 {

  def getDocumentURI(node: NodeInfo, c: XPathContext): AnyURIValue =
    if (node.getNodeKind == Type.DOCUMENT) {
      val o: Any = node.getTreeInfo.getUserData("saxon:document-uri")
      if (o.isInstanceOf[String]) {
        if (o.toString.isEmpty) null else new AnyURIValue(o.toString)
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
