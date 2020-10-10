package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.instruct.Instruction
import org.orbeon.saxon.expr.parser.CodeInjector
import org.orbeon.saxon.lib.Logger
import org.orbeon.saxon.lib.StandardDiagnostics
import org.orbeon.saxon.lib.StandardLogger
import org.orbeon.saxon.lib.TraceListener
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.Mode
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.StringValue
import org.orbeon.saxon.value.Whitespace
import java.util.Map
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import org.orbeon.saxon.utils.{Controller, Version}

object AbstractTraceListener {

  private val spaceBuffer: StringBuffer = new StringBuffer("                ")

  def spaces(n: Int): String = {
    while (spaceBuffer.length < n) spaceBuffer.append(
      AbstractTraceListener.spaceBuffer)
    spaceBuffer.substring(0, n)
  }
}

abstract class AbstractTraceListener
  extends StandardDiagnostics
    with TraceListener {

  var indent: Int = 0

  private var detail: Int = 2

  var out: Logger = new StandardLogger()

  def getCodeInjector: CodeInjector = new TraceCodeInjector()

  def setLevelOfDetail(level: Int): Unit =
    this.detail = level

  def open(controller: Controller): Unit = {
    out.info(
      "<trace " + "saxon-version=\"" + Version.getProductVersion +
        "\" " +
        getOpeningAttributes +
        '>')
    indent += 1
  }

  def getOpeningAttributes: String

  def close(): Unit = {
    indent -= 1
    out.info("</trace>")
  }

  override def enter(info: Traceable,
                     properties: Map[String, Any],
                     context: XPathContext): Unit = {
    if (isApplicable(info)) {
      val loc: Location = info.getLocation
      val tagStr: String = tag(info)
      val file: String = abbreviateLocationURI(loc.getSystemId)
      val msg: StringBuilder = new StringBuilder(
        AbstractTraceListener.spaces(indent) + '<' + tagStr)
      for ((key, value) <- properties.asScala) {
        var _val: AnyRef = value.asInstanceOf[AnyRef]
        _val match {
          case structuredQName: StructuredQName =>
            _val = structuredQName.getDisplayName
          case stringValue: StringValue =>
            _val = stringValue.getStringValue
          case _ =>
        }
        if (_val != null) {
          msg
            .append(' ')
            .append(key)
            .append("=\"")
            .append(escape(_val.toString))
            .append('"')
        }
      }
      msg.append(" line=\"").append(loc.getLineNumber).append('"')
      val col: Int = loc.getColumnNumber
      if (col >= 0) {
        msg.append(" column=\"").append(loc.getColumnNumber).append('"')
      }
      msg.append(" module=\"").append(escape(file)).append('"')
      msg.append(">")
      out.info(msg.toString)
      indent += 1
    }
  }

  def escape(in: String): String = {
    if (in == null) {
      return ""
    }
    val collapsed: CharSequence = Whitespace.collapseWhitespace(in)
    val sb = new FastStringBuffer(collapsed.length + 10)
    for (i <- 0 until collapsed.length) {
      val c: Char = collapsed.charAt(i)
      if (c == '<')
        sb.append("&lt;")
      else if (c == '>')
        sb.append("&gt;")
      else if (c == '&')
        sb.append("&amp;")
      else if (c == '\"')
        sb.append("&#34;")
      else if (c == '\n')
        sb.append("&#xA;")
      else if (c == '\r')
        sb.append("&#xD;")
      else if (c == '\t')
        sb.append("&#x9;")
      else
        sb.cat(c)
    }
    sb.toString
  }

  override def leave(info: Traceable): Unit =
    if (isApplicable(info)) {
      val tagStr: String = tag(info)
      indent -= 1
      out.info(AbstractTraceListener.spaces(indent) + "</" + tagStr + '>')
    }

  def isApplicable(info: Traceable): Boolean = level(info) <= detail

  def tag(info: Traceable): String

  def level(info: Traceable): Int =
    info match {
      case _: TraceableComponent =>
        1
      case _: Instruction =>
        2
      case _ =>
        3
    }

  def startCurrentItem(item: Item): Unit = {
    item match {
      case nodeInfo: NodeInfo if detail > 0 =>
        out.info(
          AbstractTraceListener.spaces(indent) + "<source node=\"" +
            Navigator.getPath(nodeInfo) +
            "\" line=\"" +
            nodeInfo.getLineNumber +
            "\" file=\"" +
            abbreviateLocationURI(nodeInfo.getSystemId) +
            "\">")
      case _ =>
    }
    indent += 1
  }

  def endCurrentItem(item: Item): Unit = {
    indent -= 1
    item match {
      case nodeInfo: NodeInfo if detail > 0 =>
        out.info(
          AbstractTraceListener.spaces(indent) + "</source><!-- " +
            Navigator.getPath(nodeInfo) +
            " -->")
      case _ =>
    }
  }

  def setOutputDestination(stream: Logger): Unit =
    out = stream

  def getOutputDestination: Logger = out

  override def endRuleSearch(rule: AnyRef, mode: Mode, item: Item): Unit = ()
  override def startRuleSearch(): Unit = ()
}
