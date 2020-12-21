////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.AbstractFunction

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ManualIterator




/**
  * A Callable that wraps another Callable and a Dynamic Context, in effect acting as a closure that
  * executes the original callable with a saved context.
  */
class SystemFunctionWithBoundContextItem(private var target: SystemFunction,
                                         context: XPathContext)
    extends AbstractFunction {

  var contextItem: Item = context.getContextItem

  if (contextItem.isInstanceOf[NodeInfo] && contextItem.isStreamed) {
// causing an XPDY0002 when the function is actually called
    contextItem = null
  }

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val c2: XPathContext = context.newMinorContext()
    c2.setCurrentIterator(new ManualIterator(contextItem))
    target.call(c2, arguments)
  }

  /**
    * Get the arity of the function
    *
    * @return the number of arguments in the function signature
    */
  override def getArity: Int = target.getArity

  /**
    * Get the item type of the function item
    *
    * @return the function item's type
    */
  override def getFunctionItemType: FunctionItemType =
    target.getFunctionItemType

  /**
    * Get the name of the function, or null if it is anonymous
    *
    * @return the function name, or null for an anonymous inline function
    */
  override def getFunctionName: StructuredQName = target.getFunctionName

  /**
    * Get a description of this function for use in error messages. For named functions, the description
    * is the function name (as a lexical QName). For others, it might be, for example, "inline function",
    * or "partially-applied ends-with function".
    *
    * @return a description of the function for use in error messages
    */
  override def getDescription: String = target.getDescription

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
