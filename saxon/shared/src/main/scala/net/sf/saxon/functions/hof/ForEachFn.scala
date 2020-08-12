////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the function fn:for-each() (formerly fn:map), which is a standard function in XQuery 3.0
  */
// Copyright (c) 2018-2020 Saxonica Limited
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.MappingFunction

import net.sf.saxon.expr.MappingIterator

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException




class ForEachFn extends SystemFunction {

  /**
    * Get the return type, given knowledge of the actual arguments
    *
    * @param args the actual arguments supplied
    * @return the best available item type that the function will return
    */
  override def getResultItemType(args: Array[Expression]): ItemType = {
// Item type of the result is the same as the result item type of the function
    val fnType: ItemType = args(1).getItemType
    if (fnType.isInstanceOf[SpecificFunctionType]) {
      fnType.asInstanceOf[SpecificFunctionType].getResultType.getPrimaryType
    } else {
      AnyItemType.getInstance
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(
      evalMap(arguments(1).head().asInstanceOf[Function],
              arguments(0).iterate(),
              context))

  private def evalMap(function: Function,
                      base: SequenceIterator,
                      context: XPathContext): SequenceIterator = {
    val map: MappingFunction = new MappingFunction() {
      private val args: Array[Sequence] = new Array[Sequence](1)

      def map(item: Item): SequenceIterator = {
        args(0) = item
        SystemFunction.dynamicCall(function, context, args).iterate()
      }
    }
    new MappingIterator(base, map)
  }

}


