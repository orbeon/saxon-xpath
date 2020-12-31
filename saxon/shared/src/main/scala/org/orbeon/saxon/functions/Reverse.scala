package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ReversibleIterator

import org.orbeon.saxon.value.SequenceExtent

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
      return arguments(0)
    }
    super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
  }

  override def getStreamerName: String = "Reverse"

}
