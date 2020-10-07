package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.instruct.TemplateRule

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om.Item

import java.util.Stack

class ModeTraceListener extends AbstractTraceListener {

  private var stack: Stack[Item] = new Stack()

  def getOpeningAttributes(): String =
    "xmlns:xsl=\"" + NamespaceConstant.XSLT + '\"'

  override def startCurrentItem(item: Item): Unit = {
    if (stack.empty() || stack.peek() != item) {
      super.startCurrentItem(item)
      stack.push(item)
    }
  }

  override def endCurrentItem(item: Item): Unit = {
    if (stack.peek() == item) {
      super.endCurrentItem(item)
      stack.pop()
    }
  }

  def enter(info: Traceable, context: XPathContext): Unit = {
    if (info.isInstanceOf[TemplateRule]) {
      val file: String = abbreviateLocationURI(info.getLocation.getSystemId)
      val msg = AbstractTraceListener.spaces(indent) + "<rule match=\"" +
        escape(info.asInstanceOf[TemplateRule].getMatchPattern.toString) +
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
    }
  }

  override def leave(info: Traceable): Unit = {
    if (info.isInstanceOf[TemplateRule]) {
      indent -= 1
      out.info(AbstractTraceListener.spaces(indent) + "</rule>")
    }
  }

  override def tag(info: Traceable): String = ""

}