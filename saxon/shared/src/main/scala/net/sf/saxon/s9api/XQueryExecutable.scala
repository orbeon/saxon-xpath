package net.sf.saxon.s9api

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.query.XQueryExpression
import net.sf.saxon.trace.ExpressionPresenter
import OccurrenceIndicator._
import net.sf.saxon.utils.Configuration

class XQueryExecutable(var processor: Processor,
                       var exp: XQueryExpression) {

  def load(): XQueryEvaluator = new XQueryEvaluator(processor, exp)

  def getResultItemType: ItemType = {
    val it: net.sf.saxon.model.ItemType = exp.getExpression.getItemType
    new ConstructedItemType(it, processor)
  }

  def getResultCardinality: OccurrenceIndicator = {
    val card: Int = exp.getExpression.getCardinality
    OccurrenceIndicator.getOccurrenceIndicator(card)
  }

  def isUpdateQuery: Boolean = exp.isUpdateQuery

  def explain(destination: Destination): Unit = {
    val config: Configuration = processor.getUnderlyingConfiguration
    val pipe: PipelineConfiguration = config.makePipelineConfiguration
    exp.explain(
      new ExpressionPresenter(
        config,
        destination
          .getReceiver(pipe, config.obtainDefaultSerializationProperties)))
  }

  def getUnderlyingCompiledQuery: XQueryExpression = exp

}
