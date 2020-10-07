////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.lib.EnvironmentVariableResolver

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.value.SequenceExtent

import org.orbeon.saxon.value.StringValue

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
