package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.ItemMappingFunction

import org.orbeon.saxon.expr.ItemMappingIterator

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

class FilterFn extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(
      evalFilter(arguments(1).head.asInstanceOf[Function],
        arguments(0).iterate(),
        context))

  private def evalFilter(function: Function,
                         base: SequenceIterator,
                         context: XPathContext): SequenceIterator = {
    val map: ItemMappingFunction = new ItemMappingFunction() {
      private val args: Array[Sequence] = new Array[Sequence](1)

      def mapItem(item: Item): Item = {
        args(0) = item
        var result: BooleanValue = SystemFunction.dynamicCall(function, context, args)
          .head
          .asInstanceOf[BooleanValue]
        if (result.getBooleanValue) item else null
      }
    }
    new ItemMappingIterator(base, map)
  }

  override def getStreamerName: String = "FilterFn"

}
