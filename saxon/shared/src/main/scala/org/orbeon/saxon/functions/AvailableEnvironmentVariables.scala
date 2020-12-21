////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, SystemFunctionCall, XPathContext}
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.lib.{EnvironmentVariableResolver, Feature}
import org.orbeon.saxon.om.{Item, Sequence}

//import scala.collection.compat._
import java.util.{ArrayList, List}

import org.orbeon.saxon.value.{SequenceExtent, StringValue}

import scala.jdk.CollectionConverters._


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
