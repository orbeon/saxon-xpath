package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.flwor.ClauseInfo

import org.orbeon.saxon.expr.instruct.GlobalVariable

import org.orbeon.saxon.expr.instruct.UserFunction

import org.orbeon.saxon.functions.Trace

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.query.XQueryExpression

import org.orbeon.saxon.trans.Mode


class XQueryTraceListener extends AbstractTraceListener {

   def getOpeningAttributes: String = ""

   def tag(info: Traceable): String =
    info match {
      case _: TraceableComponent =>
        info match {
          case _: GlobalVariable   => "variable"
          case _: UserFunction     => "function"
          case _: XQueryExpression => "query"
          case _                   => "misc"
        }
      case _: Trace =>
        "fn:trace"
      case clauseInfo: ClauseInfo =>
        clauseInfo.getClause.getClauseKey.toString
      case expression: Expression =>
        var s: String = expression.getExpressionName
        if (s.startsWith("xsl:"))
          s = s.substring(4)
        s match {
          case "value-of" => "text"
          case "LRE"      => "element"
          case "ATTR"     => "attribute"
          case _          => s
        }
      case _ =>
        null
    }

  override def startRuleSearch(): Unit = ()
  override def endRuleSearch(rule: AnyRef, mode: Mode, item: Item): Unit = ()
}