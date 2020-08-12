////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.lib.EnvironmentVariableResolver

import net.sf.saxon.lib.Feature

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import scala.jdk.CollectionConverters._

import net.sf.saxon.value.SequenceExtent

import net.sf.saxon.value.StringValue

import java.util.ArrayList

import java.util.List


class AvailableEnvironmentVariables extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val resolver: EnvironmentVariableResolver = context.getConfiguration
      .getConfigurationProperty(Feature.ENVIRONMENT_VARIABLE_RESOLVER)
    val myList: List[Item] = new ArrayList[Item]()
    if (context.getConfiguration.getBooleanProperty(
      Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      for (s <- resolver.getAvailableEnvironmentVariables.asScala) {
        myList.add(new StringValue(s))
      }
    }
    new SequenceExtent(myList)
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      // Suppress early evaluation
      override def preEvaluate(visitor: ExpressionVisitor): Expression = this
    }

}
