////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class implements the function fn:fold-left(), which is a standard function in XPath 3.0
 */
// Copyright (c) 2013-2020 Saxonica Limited
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.{Expression, XPathContext}
import org.orbeon.saxon.functions.{Fold, FoldingFunction, SystemFunction}
import org.orbeon.saxon.model.{AnyFunctionType, AnyItemType, ItemType}
import org.orbeon.saxon.om.{Function, GroundedValue, Item, Sequence}


class FoldLeftFn extends FoldingFunction {

  def getFold(context: XPathContext, arguments: Sequence*): Fold = {
    val arg0 = arguments(0)
    new FoldLeftFold(context, arg0.materialize(), arguments(1).head.asInstanceOf[Function])
  }

  class FoldLeftFold(private var context: XPathContext,
                     zero: GroundedValue,
                     private var function: Function)
    extends Fold {

    private var data: Sequence = zero
    private var counter: Int = 0

    def processItem(item: Item): Unit = {

      val args = Array.ofDim[Sequence](2)
      args(0) = data
      args(1) = item

      // sequence. We don't want to do this every time because it involves allocating memory.
      val result = SystemFunction.dynamicCall(function, context, args)
      data =
        if ( {counter += 1; counter - 1} % 32 == 0)
          result.materialize()
        else
          result
    }

    // The result can be returned as a LazySequence. Since we are passing it to a user-defined
    // function which can read it repeatedly, we need at the very least to wrap it in a MemoSequence.
    // But wrapping MemoSequences too deeply can cause a StackOverflow when the unwrapping finally
    // takes place; so to avoid this, we periodically ground the value as a real in-memory concrete
    // The result can be returned as a LazySequence. Since we are passing it to a user-defined
    // function which can read it repeatedly, we need at the very least to wrap it in a MemoSequence.
    // But wrapping MemoSequences too deeply can cause a StackOverflow when the unwrapping finally
    // takes place; so to avoid this, we periodically ground the value as a real in-memory concrete

    def isFinished: Boolean = false
    def result(): Sequence = data
  }

  /**
   * Get the return type, given knowledge of the actual arguments
   *
   * @param args the actual arguments supplied
   * @return the best available item type that the function will return
   */
  override def getResultItemType(args: Array[Expression]): ItemType = {
    // Item type of the result is the same as the result item type of the argument function
    val functionArgType: ItemType = args(2).getItemType
    if (functionArgType.isInstanceOf[AnyFunctionType]) {
      // will always be true once the query has been successfully type-checked
      args(2).getItemType.asInstanceOf[AnyFunctionType].getResultType.getPrimaryType
    } else {
      AnyItemType
    }
  }

  def call(context: XPathContext, args: Array[Sequence]): Sequence = call(context, args)
}


