package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.EnvironmentVariableResolver

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value.StringValue

import EnvironmentVariable._

object EnvironmentVariable {

  private def getVariable(environVar: StringValue,
                          context: XPathContext): StringValue = {
    val resolver: EnvironmentVariableResolver = context.getConfiguration
      .getConfigurationProperty(Feature.ENVIRONMENT_VARIABLE_RESOLVER)
    val environVarName: String = environVar.getStringValue
    var environValue: String = ""
    if (context.getConfiguration.getBooleanProperty(
      Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      try {
        environValue = resolver.getEnvironmentVariable(environVarName)
        if (environValue == null) {
          return null
        }
      } catch {
        case e@(_: SecurityException | _: NullPointerException) => {}

      }
    }
    new StringValue(environValue)
  }

}

class EnvironmentVariable extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[StringValue] = new ZeroOrOne(getVariable(arguments(0).head.asInstanceOf[StringValue], context))

}
