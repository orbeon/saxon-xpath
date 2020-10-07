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

   def getOpeningAttributes(): String = ""

   def tag(info: Traceable): String =
    if (info.isInstanceOf[TraceableComponent]) {
      if (info.isInstanceOf[GlobalVariable]) {
        "variable"
      } else if (info.isInstanceOf[UserFunction]) {
        "function"
      } else if (info.isInstanceOf[XQueryExpression]) {
        "query"
      } else {
        "misc"
      }
    } else if (info.isInstanceOf[Trace]) {
      "fn:trace"
    } else if (info.isInstanceOf[ClauseInfo]) {
      info.asInstanceOf[ClauseInfo].getClause.getClauseKey.toString
    } else if (info.isInstanceOf[Expression]) {
      var s: String = info.asInstanceOf[Expression].getExpressionName
      if (s.startsWith("xsl:")) {
        s = s.substring(4)
      }
      s match {
        case "value-of" => "text"
        case "LRE" => "element"
        case "ATTR" => "attribute"
        case _ => s

      }
    } else {
      null
    }

  override def startRuleSearch(): Unit = ()

  override def endRuleSearch(rule: AnyRef, mode: Mode, item: Item): Unit = ()

}