////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{ContextItemExpression, Expression, ItemMappingIterator, XPathContext}
import org.orbeon.saxon.expr.instruct.CopyOf
import org.orbeon.saxon.lib.Validation
import org.orbeon.saxon.om.{LazySequence, NodeInfo, Sequence, SequenceIterator}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.wrapper.VirtualCopy


/**
  * XSLT 3.0 function copy-of(). This compiles into an xsl:copy-of instruction, except when called dynamically.
  */
class CopyOfFn extends SystemFunction {

 override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality

  /**
    * Evaluate the expression (used only for dynamic calls)
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val in: Sequence =
      if (arguments.length == 0) context.getContextItem else arguments(0)
    val input: SequenceIterator = in.iterate()
    val output: SequenceIterator = new ItemMappingIterator(
      input,
      (item) =>
        if (!(item.isInstanceOf[NodeInfo])) {
          item
        } else {
          var vc: VirtualCopy =
            VirtualCopy.makeVirtualCopy(item.asInstanceOf[NodeInfo])
          vc.getTreeInfo.setCopyAccumulators(true)
          vc
      }
    )
// TODO: set the base URI
    new LazySequence(output)
  }

  /**
    * Make an expression that either calls this function, or that is equivalent to a call
    * on this function
    *
    * @param arguments the supplied arguments to the function call
    * @return either a function call on this function, or an expression that delivers
    * the same result
    */
  override def makeFunctionCall(arguments: Expression*): Expression = {
    var arg: Expression = null
    arg =
      if (arguments.length == 0) new ContextItemExpression() else arguments(0)
    val fn: CopyOf = new CopyOf(arg, true, Validation.PRESERVE, null, false)
    fn.setCopyAccumulators(true)
    fn.setSchemaAware(false)
    fn
  }

}
