////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.SystemFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.QNameValue

import net.sf.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List

import scala.jdk.CollectionConverters._


class AvailableSystemProperties extends SystemFunction {

  /**
   * Evaluate the expression (dynamic evaluation)
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as SequenceIterators
   * @return the result of the evaluation, in the form of a SequenceIterator
   * @throws XPathException
   * if a dynamic error occurs during the evaluation of the expression
   */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val myList: List[QNameValue] = new ArrayList[QNameValue]()
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "version"))
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "vendor"))
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "vendor-url"))
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "product-name"))
    myList.add(
      new QNameValue("xsl", NamespaceConstant.XSLT, "product-version"))
    myList.add(
      new QNameValue("xsl", NamespaceConstant.XSLT, "is-schema-aware"))
    myList.add(
      new QNameValue("xsl", NamespaceConstant.XSLT, "supports-serialization"))
    myList.add(
      new QNameValue("xsl",
        NamespaceConstant.XSLT,
        "supports-backwards-compatibility"))
    myList.add(
      new QNameValue("xsl", NamespaceConstant.XSLT, "supports-namespace-axis"))
    myList.add(
      new QNameValue("xsl", NamespaceConstant.XSLT, "supports-streaming"))
    myList.add(
      new QNameValue("xsl",
        NamespaceConstant.XSLT,
        "supports-dynamic-evaluation"))
    myList.add(
      new QNameValue("xsl",
        NamespaceConstant.XSLT,
        "supports-higher-order-functions"))
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "xpath-version"))
    myList.add(new QNameValue("xsl", NamespaceConstant.XSLT, "xsd-version"))
    if (context.getConfiguration.getBooleanProperty(
      Feature.ALLOW_EXTERNAL_FUNCTIONS)) {
      for (s <- System.getProperties.keySet.asScala) {
        myList.add(new QNameValue("", "", s.toString))
      }
    }
    SequenceExtent.makeSequenceExtent(myList)
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      // Suppress early evaluation
      override def preEvaluate(visitor: ExpressionVisitor): Expression = this
    }

}
