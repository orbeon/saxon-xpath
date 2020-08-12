package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.GroupIterator

import net.sf.saxon.expr.sort.MergeInstr

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}


class CurrentMergeKey extends SystemFunction with Callable {

  @BeanProperty
  var controllingInstruction: MergeInstr = null

  override def makeFunctionCall(arguments: Expression*): Expression =
    new SystemFunctionCall(this, arguments.toArray) {
      override def getScopingExpression(): Expression =
        getControllingInstruction
    }

  def iterate(c: XPathContext): SequenceIterator = {
    val gi: GroupIterator = c.getCurrentMergeGroupIterator
    if (gi == null) {
      throw new XPathException("There is no current merge key", "XTDE3510")
    }
    gi.getCurrentGroupingKey.iterate()
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(iterate(context))

}
