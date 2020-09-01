package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import Lang._

import scala.util.control.Breaks._

object Lang {

  def isLang(arglang: String, target: NodeInfo): Boolean = {
    var doclang: String = null
    var node: NodeInfo = target
    breakable {
      while (node != null) {
        doclang = node.getAttributeValue(NamespaceConstant.XML, "lang")
        if (doclang != null) {
          break()
        }
        node = node.getParent
        if (node == null) false
      }
    }
    if (doclang == null) return false
    while (true) {
      if (arglang.equalsIgnoreCase(doclang)) return true
      val hyphen: Int = doclang.lastIndexOf("-")
      if (hyphen < 0) return false
      doclang = doclang.substring(0, hyphen)
    }
    false
  }

}

class Lang extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    var target: NodeInfo = null
    target =
      if (arguments.length > 1) arguments(1).head().asInstanceOf[NodeInfo]
      else getAndCheckContextItem(context)
    val arg0Val: Item = arguments(0).head()
    val testLang: String = if (arg0Val == null) "" else arg0Val.getStringValue
    BooleanValue.get(isLang(testLang, target))
  }

  private def getAndCheckContextItem(context: XPathContext): NodeInfo = {
    var target: NodeInfo = null
    val current: Item = context.getContextItem
    if (current == null) {
      val err = new XPathException(
        "The context item for lang() is absent")
      err.setErrorCode("XPDY0002")
      err.setXPathContext(context)
      throw err
    }
    if (!(current.isInstanceOf[NodeInfo])) {
      val err = new XPathException(
        "The context item for lang() is not a node")
      err.setErrorCode("XPTY0004")
      err.setXPathContext(context)
      throw err
    }
    target = current.asInstanceOf[NodeInfo]
    target
  }

}
