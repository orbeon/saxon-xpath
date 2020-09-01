////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.xpath

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.instruct.Executable

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.model.SchemaException

import net.sf.saxon.model.Type

import org.xml.sax.InputSource

import javax.xml.namespace.NamespaceContext

import javax.xml.namespace.QName

import javax.xml.transform.Source

import javax.xml.xpath._

import scala.beans.{BeanProperty, BooleanBeanProperty}

class XPathEvaluator /**
 * Construct an XPathEvaluator with a specified configuration.
 *
 * @param config the configuration to be used. If schema-aware XPath expressions are to be used,
 *               this must be an EnterpriseConfiguration.
 */
(private var config: Configuration)
  extends XPath {

  @BeanProperty
  var staticContext: JAXPXPathStaticContext = new JAXPXPathStaticContext(
    config)

  def this() = this(Configuration.newConfiguration)

  def getConfiguration: Configuration = config

  def reset(): Unit = {
    staticContext = new JAXPXPathStaticContext(config)
  }

  def setXPathVariableResolver(
                                xPathVariableResolver: XPathVariableResolver): Unit = {
    staticContext.setXPathVariableResolver(xPathVariableResolver)
  }

  /**
   * Get the resolver for XPath variables
   *
   * @return the resolver, if one has been set
   */
  def getXPathVariableResolver(): XPathVariableResolver =
    staticContext.getXPathVariableResolver

  def setXPathFunctionResolver(
                                xPathFunctionResolver: XPathFunctionResolver): Unit = {
    staticContext.setXPathFunctionResolver(xPathFunctionResolver)
  }

  /*@Nullable*/

  def getXPathFunctionResolver(): XPathFunctionResolver =
    staticContext.getXPathFunctionResolver

  def setNamespaceContext(namespaceContext: NamespaceContext): Unit = {
    staticContext.setNamespaceContext(namespaceContext)
  }

  def getNamespaceContext(): NamespaceContext =
    staticContext.getNamespaceContext

  def importSchema(source: Source): Unit = {
    staticContext.importSchema(source)
    staticContext.setSchemaAware(true)
  }

  /*@NotNull*/

  def compile(expr: String): XPathExpression = {

    if (expr == null)
      throw new NullPointerException("expr")

    val exec: Executable = new Executable(getConfiguration)
    var exp: Expression = ExpressionTool.make(expr, staticContext, 0, -1, null)
    val visitor: ExpressionVisitor = ExpressionVisitor.make(staticContext)
    val contextItemType: ContextItemStaticInfo =
      getConfiguration.makeContextItemStaticInfo(Type.ITEM_TYPE, maybeUndefined = true)
    exp = exp
      .typeCheck(visitor, contextItemType)
      .optimize(visitor, contextItemType)
    val map: SlotManager = staticContext.getConfiguration.makeSlotManager
    ExpressionTool.allocateSlots(exp, 0, map)
    val xpe: XPathExpressionImpl = new XPathExpressionImpl(exp, exec)
    xpe.setStackFrameMap(map)
    xpe
  }

  def evaluate(expr: String, node: AnyRef, qName: QName): AnyRef = {
    val exp: XPathExpression = compile(expr)
    exp.evaluate(node, qName)
  }

  def evaluate(expr: String, node: AnyRef): String = {
    val exp: XPathExpression = compile(expr)
    exp.evaluate(node)
  }

  def evaluate(expr: String, inputSource: InputSource, qName: QName): AnyRef = {
    if (expr == null) {
      throw new NullPointerException("expr")
    }
    if (inputSource == null) {
      throw new NullPointerException("inputSource")
    }
    if (qName == null) {
      throw new NullPointerException("qName")
    }
    val exp: XPathExpression = compile(expr)
    exp.evaluate(inputSource, qName)
  }

  def evaluate(expr: String, inputSource: InputSource): String = {
    if (expr == null) {
      throw new NullPointerException("expr")
    }
    if (inputSource == null) {
      throw new NullPointerException("inputSource")
    }
    val exp: XPathExpression = compile(expr)
    exp.evaluate(inputSource)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * <p>XPathEvaluator implements the JAXP API for standalone XPath processing (that is,
 * executing XPath expressions in the absence of an XSLT stylesheet). It is an implementation
 * of the JAXP 1.3 XPath interface, with additional methods provided (a) for backwards
 * compatibility (b) to give extra control over the XPath evaluation, and (c) to support
 * later XPath versions (2.0, 3.0, 3.1).</p>
 * <p>The JAXP API is designed at one level to be object-model independent, but in other
 * respects (especially in Java SE 9) it is designed specifically with DOM in mind. The Saxon
 * implementation makes its own decisions about how to handle non-DOM nodes. Specifically,
 * when an expression with return type {@link XPathConstants#NODE} is evaluated, Saxon
 * returns the underlying node from the native object model; when the return type is given
 * as {@link XPathConstants#NODESET}, the nodes are delivered as a DOM {@link org.w3c.dom.NodeList}
 * if they are DOM nodes, or as a {@link java.util.List} otherwise.</p>
 * <p>For an alternative XPath API, offering more complete access to Saxon capabilities,
 * see {@link net.sf.saxon.s9api.XPathCompiler}.</p>
 * <p>Note that the <code>XPathEvaluator</code> links to a Saxon {@link Configuration}
 * object. By default a new <code>Configuration</code> is created automatically. In many
 * applications, however, it is desirable to share a configuration. The default configuration
 * is not schema aware. All source documents used by XPath expressions under this evaluator
 * must themselves be built using the <code>Configuration</code> used by this evaluator.</p>
 */
