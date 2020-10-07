////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the function fn:fold-right(), which is a standard function in XQuery 1.1
  */
// Copyright (c) 2018-2020 Saxonica Limited
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.Reverse

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.model.AnyFunctionType

import org.orbeon.saxon.model.AnyItemType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException




class FoldRightFn extends SystemFunction {

  /**
    * Get the return type, given knowledge of the actual arguments
    *
    * @param args the actual arguments supplied
    * @return the best available item type that the function will return
    */
  override def getResultItemType(args: Array[Expression]): ItemType = {
// Item type of the result is the same as the result item type of the function
    val functionArgType: ItemType = args(2).getItemType
    if (functionArgType.isInstanceOf[AnyFunctionType]) {
// will always be true once the query has been successfully type-checked
      functionArgType
        .asInstanceOf[AnyFunctionType]
        .getResultType
        .getPrimaryType
    } else {
      AnyItemType
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    evalFoldRight(arguments(2).head.asInstanceOf[Function],
                  arguments(1).materialize(),
                  arguments(0).iterate(),
                  context)

  private def evalFoldRight(function: Function,
                            zero: Sequence,
                            base: SequenceIterator,
                            context: XPathContext): Sequence = {
    val reverseBase: SequenceIterator = Reverse.getReverseIterator(base)
    val args: Array[Sequence] = Array.ofDim[Sequence](2)
    var item: Item = null
    var zeroVar : Sequence = zero
    while (({
      item = reverseBase.next()
      item
    }) != null) {
      args(0) = item
      args(1) = zeroVar.materialize()
      try {
        zeroVar = SystemFunction.dynamicCall(function, context, args)
      }
      catch {
        case e: XPathException => {
          e.maybeSetContext(context)
          throw e
        }

      }
    }
    zeroVar
  }

}


