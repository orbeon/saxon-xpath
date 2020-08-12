package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser.ContextItemStaticInfo
import net.sf.saxon.expr.parser.ExpressionTool
import net.sf.saxon.expr.parser.ExpressionVisitor
import net.sf.saxon.expr.parser.RebindingMap
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.tree.iter.LookaheadIteratorImpl
import net.sf.saxon.value.EmptySequence

class OuterForExpression extends ForExpression {

  override  def getRangeVariableCardinality(): Int =
    StaticProperty.ALLOWS_ZERO_OR_ONE

  override def optimize(visitor: ExpressionVisitor,
               contextItemType: ContextItemStaticInfo): Expression = {
    val sequence0: Expression = getSequence
    getSequenceOp.optimize(visitor, contextItemType)
    val action0: Expression = getAction
    getActionOp.optimize(visitor, contextItemType)
    if (sequence0 != getSequence || action0 != getAction) {
      optimize(visitor, contextItemType)
    }
    this
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val forExp: OuterForExpression = new OuterForExpression()
    ExpressionTool.copyLocationInfo(this, forExp)
    forExp.setRequiredType(requiredType)
    forExp.setVariableQName(variableName)
    forExp.setSequence(getSequence.copy(rebindings))
    rebindings.put(this, forExp)
    val newAction: Expression = getAction.copy(rebindings)
    forExp.setAction(newAction)
    forExp.variableName = variableName
    forExp
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val base: SequenceIterator = getSequence.iterate(context)
    val ahead: LookaheadIterator =
      LookaheadIteratorImpl.makeLookaheadIterator(base)
    if (ahead.hasNext) {
      val map: MappingFunction =
        new expr.ForExpression.MappingAction(context, getLocalSlotNumber, getAction)
      new MappingIterator(ahead, map)
    } else {
      context.setLocalVariable(getLocalSlotNumber, EmptySequence.getInstance)
      getAction.iterate(context)
    }
  }

  override def process(output: Outputter, context: XPathContext): Unit = {
    val base: SequenceIterator = getSequence.iterate(context)
    val position: Int = 1
    val slot: Int = getLocalSlotNumber
    val ahead: LookaheadIterator =
      LookaheadIteratorImpl.makeLookaheadIterator(base)
    if (ahead.hasNext) {
      while (true) {
        val item: Item = ahead.next()
        if (item == null) //break
        context.setLocalVariable(slot, item)
        getAction.process(output, context)
      }
    } else {
      context.setLocalVariable(getLocalSlotNumber, EmptySequence.getInstance)
      getAction.process(output, context)
    }
  }

  override def getExpressionName(): String = "outerFor"

  override def evaluatePendingUpdates(context: XPathContext,
                             pul: PendingUpdateList): Unit = {
    val base: SequenceIterator = getSequence.iterate(context)
    val position: Int = 1
    val slot: Int = getLocalSlotNumber
    val ahead: LookaheadIterator =
      LookaheadIteratorImpl.makeLookaheadIterator(base)
    if (ahead.hasNext) {
      while (true) {
        val item: Item = ahead.next()
        if (item == null) //break
        context.setLocalVariable(slot, item)
        getAction.evaluatePendingUpdates(context, pul)
      }
    } else {
      context.setLocalVariable(getLocalSlotNumber, EmptySequence.getInstance)
      getAction.evaluatePendingUpdates(context, pul)
    }
  }

  override  def explainSpecializedAttributes(out: ExpressionPresenter): Unit = {
    out.emitAttribute("outer", "true")
  }

}
