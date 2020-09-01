package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ReversibleIterator

import net.sf.saxon.value.SequenceExtent

import Reverse._

object Reverse {

  def getReverseIterator[T <: Item](
                                     forwards: SequenceIterator): SequenceIterator =
    if (forwards.isInstanceOf[ReversibleIterator]) {
      forwards.asInstanceOf[ReversibleIterator].getReverseIterator
    } else {
      val extent: SequenceExtent = new SequenceExtent(forwards)
      extent.reverseIterate()
    }

}

class Reverse extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    val baseProps: Int = arguments(0).getSpecialProperties
    if ((baseProps & StaticProperty.REVERSE_DOCUMENT_ORDER) !=
      0) {
      (baseProps & ~StaticProperty.REVERSE_DOCUMENT_ORDER) |
        StaticProperty.ORDERED_NODESET
    } else if ((baseProps & StaticProperty.ORDERED_NODESET) != 0) {
      (baseProps & ~StaticProperty.ORDERED_NODESET) | StaticProperty.REVERSE_DOCUMENT_ORDER
    } else {
      baseProps
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(getReverseIterator(arguments(0).iterate()))

  override def makeOptimizedFunctionCall(
                                 visitor: ExpressionVisitor,
                                 contextInfo: ContextItemStaticInfo,
                                 arguments: Expression*): Expression = {
    if (arguments(0).getCardinality == StaticProperty.ALLOWS_ZERO_OR_ONE) {
      arguments(0)
    }
    super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
  }

  override def getStreamerName: String = "Reverse"

}
