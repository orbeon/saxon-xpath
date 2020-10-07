package org.orbeon.saxon.s9api

import org.orbeon.saxon.lib.ErrorReporter

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.s9api.streams.XdmStream

import org.orbeon.saxon.sxpath.XPathDynamicContext

import org.orbeon.saxon.sxpath.XPathExpression

import org.orbeon.saxon.sxpath.XPathVariable

import org.orbeon.saxon.trans.XPathException

import javax.xml.transform.URIResolver

import java.util.Map


class XPathSelector(private var exp: XPathExpression,
                    private var declaredVariables: Map[StructuredQName, XPathVariable])
  extends java.lang.Iterable[XdmItem] {

  private val dynamicContext: XPathDynamicContext = exp.createDynamicContext()

  def setContextItem(item: XdmItem): Unit = {
    if (item == null) {
      throw new NullPointerException("contextItem")
    }
    if (!exp.getInternalExpression.getPackageData.isSchemaAware) {
      val it: Item = item.getUnderlyingValue.head
      if (it.isInstanceOf[NodeInfo] && it
        .asInstanceOf[NodeInfo]
        .getTreeInfo
        .isTyped) {
        throw new SaxonApiException(
          "The supplied node has been schema-validated, but the XPath expression was compiled without schema-awareness")
      }
    }
    dynamicContext.setContextItem(item.getUnderlyingValue)
  }

  def getContextItem: XdmItem =
    XdmItem.wrapItem(dynamicContext.getContextItem)

  def setVariable(name: QName, value: XdmValue): Unit = {
    val qn: StructuredQName = name.getStructuredQName
    val `var`: XPathVariable = declaredVariables.get(qn)
    if (`var` == null) {
      throw new SaxonApiException(
        new XPathException("Variable has not been declared: " + name))
    }
    dynamicContext.setVariable(`var`, value.getUnderlyingValue)
  }

  def setURIResolver(resolver: URIResolver): Unit = {
    dynamicContext.setURIResolver(resolver)
  }

  def getURIResolver: URIResolver = dynamicContext.getURIResolver

  def setErrorReporter(reporter: ErrorReporter): Unit = {
    dynamicContext.setErrorReporter(reporter)
  }

  def evaluate(): XdmValue = {
    var value: Sequence = null
    value = exp.iterate(dynamicContext).materialize()
    XdmValue.wrap(value)
  }

  def evaluateSingle(): XdmItem = {
    val i = exp.evaluateSingle(dynamicContext)
    if (i == null)
      null
    else
      XdmValue.wrap(i).asInstanceOf[XdmItem]
  }

  def iterator: XdmSequenceIterator[XdmItem] =
    new XdmSequenceIterator(exp.iterate(dynamicContext))

  def stream(): XdmStream[_ <: XdmItem] = iterator.asInstanceOf[XdmItem].stream()

  def effectiveBooleanValue(): Boolean =
    exp.effectiveBooleanValue(dynamicContext)

  def getUnderlyingXPathContext: XPathDynamicContext = dynamicContext

}
