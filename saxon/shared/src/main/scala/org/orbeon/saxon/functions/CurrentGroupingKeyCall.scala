package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.expr.sort.GroupIterator

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om._

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

class CurrentGroupingKeyCall extends Expression with Callable {

  override def getScopingExpression(): Expression =
    CurrentGroupCall.findControllingInstruction(this)

   override def computeCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_MORE

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def getItemType: ItemType = BuiltInAtomicType.ANY_ATOMIC

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("currentGroupingKey")
    out.endElement()
  }

  override def copy(rebindings: RebindingMap): Expression =
    new CurrentGroupingKeyCall()

  override def getIntrinsicDependencies: Int = StaticProperty.DEPENDS_ON_CURRENT_GROUP

  override def iterate(c: XPathContext): SequenceIterator = {
    val gi: GroupIterator = c.getCurrentGroupIterator
    val result: AtomicSequence =
      if (gi == null) null else gi.getCurrentGroupingKey
    if (result == null) {
      val err =
        new XPathException("There is no current grouping key", "XTDE1071")
      err.setLocation(getLocation)
      throw err
    }
    result.iterate()
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(iterate(context))

}
