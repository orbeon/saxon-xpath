package net.sf.saxon.trace

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.instruct.Instruction
import net.sf.saxon.expr.parser.CodeInjector
import net.sf.saxon.lib.Logger
import net.sf.saxon.lib.StandardDiagnostics
import net.sf.saxon.lib.StandardLogger
import net.sf.saxon.lib.TraceListener
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.Mode
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.tree.util.Navigator
import net.sf.saxon.value.StringValue
import net.sf.saxon.value.Whitespace
import java.util.Map
import scala.jdk.CollectionConverters._
import net.sf.saxon.utils.{Controller, Version}

object AbstractTraceListener {

  private var spaceBuffer: StringBuffer = new StringBuffer("                ")

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

  def setLevelOfDetail(level: Int): Unit = {
    this.detail = level
  }

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
        var `val`: AnyRef = value.asInstanceOf[AnyRef]
        if (`val`.isInstanceOf[StructuredQName]) {
          `val` = `val`.asInstanceOf[StructuredQName].getDisplayName
        } else if (`val`.isInstanceOf[StringValue]) {
          `val` = `val`.asInstanceOf[StringValue].getStringValue
        }
        if (`val` != null) {
          msg
            .append(' ')
            .append(key)
            .append("=\"")
            .append(escape(`val`.toString))
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
    val sb: FastStringBuffer = new FastStringBuffer(collapsed.length + 10)
    for (i <- 0 until collapsed.length) {
      val c: Char = collapsed.charAt(i)
      if (c == '<') {
        sb.append("&lt;")
      } else if (c == '>') {
        sb.append("&gt;")
      } else if (c == '&') {
        sb.append("&amp;")
      } else if (c == '\"') {
        sb.append("&#34;")
      } else if (c == '\n') {
        sb.append("&#xA;")
      } else if (c == '\r') {
        sb.append("&#xD;")
      } else if (c == '\t') {
        sb.append("&#x9;")
      } else {
        sb.cat(c)
      }
    }
    sb.toString
  }

  override def leave(info: Traceable): Unit = {
    if (isApplicable(info)) {
      val tagStr: String = tag(info)
      indent -= 1
      out.info(AbstractTraceListener.spaces(indent) + "</" + tagStr + '>')
    }
  }

  def isApplicable(info: Traceable): Boolean = level(info) <= detail

  def tag(info: Traceable): String

  def level(info: Traceable): Int = {
    if (info.isInstanceOf[TraceableComponent]) {
      return 1
    }
    if (info.isInstanceOf[Instruction]) {
      2
    } else {
      3
    }
  }

  def startCurrentItem(item: Item): Unit = {
    if (item.isInstanceOf[NodeInfo] && detail > 0) {
      val curr: NodeInfo = item.asInstanceOf[NodeInfo]
      out.info(
        AbstractTraceListener.spaces(indent) + "<source node=\"" +
          Navigator.getPath(curr) +
          "\" line=\"" +
          curr.getLineNumber +
          "\" file=\"" +
          abbreviateLocationURI(curr.getSystemId) +
          "\">")
    }
    indent += 1
  }

  def endCurrentItem(item: Item): Unit = {
    indent -= 1
    if (item.isInstanceOf[NodeInfo] && detail > 0) {
      val curr: NodeInfo = item.asInstanceOf[NodeInfo]
      out.info(
        AbstractTraceListener.spaces(indent) + "</source><!-- " +
          Navigator.getPath(curr) +
          " -->")
    }
  }

  def setOutputDestination(stream: Logger): Unit = {
    out = stream
  }

  def getOutputDestination: Logger = out

  override def endRuleSearch(rule: AnyRef, mode: Mode, item: Item): Unit = ()

  override def startRuleSearch(): Unit = ()

}
