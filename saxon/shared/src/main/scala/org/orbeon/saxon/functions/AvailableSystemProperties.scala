////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.QNameValue

import org.orbeon.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List

//import scala.collection.compat._
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
