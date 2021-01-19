package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Literal

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.regex.RegularExpression

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

abstract class RegexFunction
  extends SystemFunction
    with StatefulSystemFunction {

  @BeanProperty
  var staticRegex: RegularExpression = _

  private def tryToBindRegularExpression(arguments: Array[Expression]): Unit = {
    if (arguments(1).isInstanceOf[Literal] &&
      arguments(arguments.length - 1).isInstanceOf[Literal]) {
      try {
        val config: Configuration = getRetainedStaticContext.getConfiguration
        val re: String = arguments(1)
          .asInstanceOf[Literal]
          .value
          .getStringValue
        val flags: String = arguments(arguments.length - 1)
          .asInstanceOf[Literal]
          .value
          .getStringValue
        var hostLang: String = "XP30"
        if (config.getXsdVersion == Configuration.XSD11) {
          hostLang += "/XSD11"
        }
        val warnings: List[String] = new ArrayList[String](1)
        staticRegex =
          config.compileRegularExpression(re, flags, hostLang, warnings) // required changes in configuration file
        if (!allowRegexMatchingEmptyString() && staticRegex.matches("")) {
          staticRegex = null
        }
      } catch {
        case _: XPathException =>

      }
    }
  }

  override def copy(): RegexFunction = {
    val copy: RegexFunction = SystemFunction
      .makeFunction(getFunctionName.getLocalPart,
        getRetainedStaticContext,
        getArity)
      .asInstanceOf[RegexFunction]
    copy.staticRegex = staticRegex
    copy
  }

  def allowRegexMatchingEmptyString(): Boolean

  override def makeFunctionCall(arguments: Expression*): Expression = {
    tryToBindRegularExpression(arguments.toArray)
    super.makeFunctionCall(arguments: _*)
  }

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    tryToBindRegularExpression(arguments.toArray)
    super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
  }

  def getRegularExpression(args: Array[Sequence]): RegularExpression = {
    if (staticRegex != null) {
      return staticRegex
    }
    val config: Configuration = getRetainedStaticContext.getConfiguration
    val regexArg: StringValue = args(1).head.asInstanceOf[StringValue]
    val re: String = regexArg.getStringValue
    val flags: String = args(args.length - 1).head.getStringValue
    var hostLang: String = "XP30"
    if (config.getXsdVersion == Configuration.XSD11) {
      hostLang += "/XSD11"
    }
    val warnings = new ArrayList[String](1)
    val regex    = config.compileRegularExpression(re, flags, hostLang, warnings)
    if (! allowRegexMatchingEmptyString() && regex.matches(""))
      throw new XPathException(
        "The regular expression must not be one that matches a zero-length string",
        "FORX0003"
      )
    regex
  }

}
