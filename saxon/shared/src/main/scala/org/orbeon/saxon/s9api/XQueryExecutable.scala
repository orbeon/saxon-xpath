package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.PipelineConfiguration
import org.orbeon.saxon.query.XQueryExpression
import org.orbeon.saxon.trace.ExpressionPresenter
import OccurrenceIndicator._
import org.orbeon.saxon.utils.Configuration

class XQueryExecutable(var processor: Processor,
                       var exp: XQueryExpression) {

  def load(): XQueryEvaluator = new XQueryEvaluator(processor, exp)

  def getResultItemType: ItemType = {
    val it: org.orbeon.saxon.model.ItemType = exp.getExpression.getItemType
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
