package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.instruct.TemplateRule
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.Item

class ModeTraceListener extends AbstractTraceListener {

  private var stack: List[Item] = Nil

  def getOpeningAttributes: String =
    "xmlns:xsl=\"" + NamespaceConstant.XSLT + '\"'

  override def startCurrentItem(item: Item): Unit =
    if (stack.isEmpty || stack.head != item) {
      super.startCurrentItem(item)
      stack ::= item
    }

  override def endCurrentItem(item: Item): Unit =
    if (stack.head == item) {
      super.endCurrentItem(item)
      stack = stack.tail
    }

  def enter(info: Traceable, context: XPathContext): Unit =
    info match {
      case templateRule: TemplateRule =>
        val file: String = abbreviateLocationURI(info.getLocation.getSystemId)
        val msg = AbstractTraceListener.spaces(indent) + "<rule match=\"" +
          escape(templateRule.getMatchPattern.toString) +
          '"' +
          " line=\"" +
          info.getLocation.getLineNumber +
          '"' +
          " module=\"" +
          escape(file) +
          '"' +
          '>'
        out.info(msg)
        indent = indent + 1
      case _ =>
    }

  override def leave(info: Traceable): Unit =
    if (info.isInstanceOf[TemplateRule]) {
      indent -= 1
      out.info(AbstractTraceListener.spaces(indent) + "</rule>")
    }

  def tag(info: Traceable): String = ""
}