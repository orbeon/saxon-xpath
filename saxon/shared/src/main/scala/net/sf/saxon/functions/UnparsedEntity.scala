////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.Type

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.StringValue

import UnparsedEntity._




object UnparsedEntity {

  var URI: Int = 0

  var PUBLIC_ID: Int = 1

  class UnparsedEntityUri extends UnparsedEntity {

    def getOp(): Int = URI

  }

  class UnparsedEntityPublicId extends UnparsedEntity {

    def getOp(): Int = PUBLIC_ID

  }

}

abstract class UnparsedEntity extends SystemFunction with Callable {

  def getOp: Int

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val operation: Int = getOp
    val arg0: String = arguments(0).head.getStringValue
    var doc: NodeInfo = null
    if (getArity == 1) {
      val it: Item = context.getContextItem
      if (it.isInstanceOf[NodeInfo]) {
        doc = it.asInstanceOf[NodeInfo].getRoot
      }
      if (doc == null || doc.getNodeKind != Type.DOCUMENT) {
        val code: String = if (operation == URI) "XTDE1370" else "XTDE1380"
        throw new XPathException(
          "In function " + getFunctionName.getDisplayName +
            ", the context item must be a node in a tree whose root is a document node",
          code,
          context)
      }
    } else {
      doc = arguments(1).head.asInstanceOf[NodeInfo]
      if (doc != null) {
        doc = doc.getRoot
      }
      if (doc == null || doc.getNodeKind != Type.DOCUMENT) {
        val code: String = if (operation == URI) "XTDE1370" else "XTDE1380"
        throw new XPathException(
          "In function " + getFunctionName.getDisplayName + ", the second argument must be a document node",
          code,
          context)
      }
    }
    val ids: Array[String] = doc.getTreeInfo.getUnparsedEntity(arg0)
    val result: String = if (ids == null) "" else ids(operation)
    if (operation == URI) new AnyURIValue(result) else new StringValue(result)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implements the unparsed-entity-uri() function defined in XSLT 1.0
  * and the unparsed-entity-public-id() function defined in XSLT 2.0
  */
