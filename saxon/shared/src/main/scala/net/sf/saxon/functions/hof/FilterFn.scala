package net.sf.saxon.functions.hof

import net.sf.saxon.expr.ItemMappingFunction

import net.sf.saxon.expr.ItemMappingIterator

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

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

  override def getStreamerName(): String = "FilterFn"

}
