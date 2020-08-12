////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.xpath

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.Executable

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.functions.Number_1

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.wrapper.VirtualNode

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.DoubleValue

import net.sf.saxon.value.NumericValue

import org.xml.sax.InputSource

import javax.xml.namespace.QName

import javax.xml.transform.ErrorListener

import javax.xml.transform.sax.SAXSource

import javax.xml.xpath.XPathConstants

import javax.xml.xpath.XPathExpression

import javax.xml.xpath.XPathExpressionException

import scala.beans.{BeanProperty, BooleanBeanProperty}

class XPathExpressionImpl (private var expression: Expression,
                                     private var executable: Executable)
  extends XPathExpression {

  private var config: Configuration = executable.getConfiguration

  private var atomizer: Expression = _

  @BeanProperty
  var stackFrameMap: SlotManager = _

  def getConfiguration(): Configuration = config

  /*@Nullable*/

  def evaluate(node: AnyRef, qName: QName): AnyRef = {
    var contextItem: Item = null
    var node1:AnyRef = node
    if (node1.isInstanceOf[ZeroOrOne[_ <: Item]]) {
      node1 = node1.asInstanceOf[ZeroOrOne[_ <: Item]].head()
    }
    if (node1.isInstanceOf[TreeInfo]) {
      node1 = node1.asInstanceOf[TreeInfo].getRootNode
    }
    if (node1.isInstanceOf[NodeInfo]) {
      if (!node1.asInstanceOf[NodeInfo].getConfiguration.isCompatible(config)) {
        throw new XPathExpressionException(
          "Supplied node must be built using the same or a compatible Configuration")
      }
      if (node1.isInstanceOf[TreeInfo] && node1.asInstanceOf[TreeInfo].isTyped &&
        !executable.isSchemaAware) {
        throw new XPathExpressionException(
          "The expression was compiled to handled untyped data, but the input is typed")
      }
      contextItem = node1.asInstanceOf[NodeInfo]
    } else if (node1.isInstanceOf[Item]) {
      contextItem = node1.asInstanceOf[Item]
    } else {
      val converter: JPConverter =
        JPConverter.allocate(node1.getClass, null, config)
      var `val`: Sequence = null
      `val` = converter.convert(node1, new EarlyEvaluationContext(config))
      if (`val`.isInstanceOf[NodeInfo]) {
        if (!`val`
          .asInstanceOf[NodeInfo]
          .getConfiguration
          .isCompatible(config)) {
          throw new XPathExpressionException(
            "Supplied node must be built using the same or a compatible Configuration")
        }
        if (`val`
          .asInstanceOf[NodeInfo]
          .getTreeInfo
          .isTyped && !executable.isSchemaAware) {
          throw new XPathExpressionException(
            "The expression was compiled to handled untyped data, but the input is typed")
        }
        contextItem = `val`.asInstanceOf[NodeInfo]
      } else {
        throw new XPathExpressionException(
          "Cannot locate an object model implementation for nodes of class " +
            node1.getClass.getName)
      }
    }
    val context: XPathContextMajor =
      new XPathContextMajor(contextItem, executable)
    context.openStackFrame(stackFrameMap)
    if (qName == XPathConstants.BOOLEAN) {
      expression.effectiveBooleanValue(context).asInstanceOf[AnyRef]
    } else if (qName == XPathConstants.STRING) {
      val iter: SequenceIterator = expression.iterate(context)
      val first: Item = iter.next()
      if (first == null) {
        ""
      }
      first.getStringValue
    } else if (qName == XPathConstants.NUMBER) {
      if (atomizer == null) {
        atomizer = Atomizer.makeAtomizer(expression, null)
      }
      val iter: SequenceIterator = atomizer.iterate(context)
      val first: Item = iter.next()
      if (first == null) {
        java.lang.Double.NaN
      }
      if (first.isInstanceOf[NumericValue]) {
        first.asInstanceOf[NumericValue].getDoubleValue.asInstanceOf[AnyRef]
      } else {
        val v: DoubleValue =
          Number_1.convert(first.asInstanceOf[AtomicValue], getConfiguration)
        v.getDoubleValue.asInstanceOf[AnyRef]
      }
    } else if (qName == XPathConstants.NODE) {
      val iter: SequenceIterator = expression.iterate(context)
      val first: Item = iter.next()
      if (first.isInstanceOf[VirtualNode]) {
        first.asInstanceOf[VirtualNode].getRealNode
      }
      if (first == null || first.isInstanceOf[NodeInfo]) {
        first
      }
      throw new XPathExpressionException("Expression result is not a node")
    } else if (qName == XPathConstants.NODESET) {
      context.openStackFrame(stackFrameMap)
      val iter: SequenceIterator = expression.iterate(context)
      val extent: GroundedValue = iter.materialize()
      val converter: PJConverter =
        PJConverter.allocateNodeListCreator(config, node1)
      converter.convert(extent, classOf[AnyRef], context).asInstanceOf[AnyRef]
    } else {
      throw new IllegalArgumentException(
        "qName: Unknown type for expected result")
    }
  }

  /*@NotNull*/

  def evaluate(node: AnyRef): String =
    evaluate(node, XPathConstants.STRING).asInstanceOf[String]

  /*@Nullable*/

  def evaluate(inputSource: InputSource, qName: QName): AnyRef = {
    if (qName == null) {
      throw new NullPointerException("qName")
    }
    var doc: NodeInfo = null
    if (inputSource != null) {
      doc = config.buildDocumentTree(new SAXSource(inputSource)).getRootNode
    }
    evaluate(doc, qName)
  }

  /*@NotNull*/

  def evaluate(inputSource: InputSource): String = {
    if (inputSource == null) {
      throw new NullPointerException("inputSource")
    }
    val doc: NodeInfo =
      config.buildDocumentTree(new SAXSource(inputSource)).getRootNode
    evaluate(doc, XPathConstants.STRING).asInstanceOf[String]
  }

  def getInternalExpression(): Expression = expression

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * <p>The JAXP XPathExpression interface represents a compiled XPath expression that can be repeatedly
 * evaluated. This class is Saxon's implementation of that interface.</p>
 * <p>The class also includes some methods retained from Saxon's original XPath API. When these methods
 * are used, the object contains the context node and other state, so it is not thread-safe.</p>
 *
 * @author Michael H. Kay
 */
