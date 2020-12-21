package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.EnvironmentVariable._
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.om.{Sequence, ZeroOrOne}
import org.orbeon.saxon.value.StringValue


object EnvironmentVariable {

  private def getVariable(environVar: StringValue, context: XPathContext): StringValue = {
    val resolver = context.getConfiguration.getConfigurationProperty(Feature.ENVIRONMENT_VARIABLE_RESOLVER)
    val environVarName = environVar.getStringValue
    var environValue = ""
    if (context.getConfiguration.getBooleanProperty(Feature.ALLOW_EXTERNAL_FUNCTIONS))
      try {
        environValue = resolver.getEnvironmentVariable(environVarName)
        if (environValue == null)
          return null
      } catch {
        case _: SecurityException | _: NullPointerException =>
      }
    new StringValue(environValue)
  }
}

class EnvironmentVariable extends SystemFunction {
  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[StringValue] =
    new ZeroOrOne(getVariable(arguments(0).head.asInstanceOf[StringValue], context))
}
