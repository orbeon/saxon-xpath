package org.orbeon.saxon

import org.orbeon.saxon.expr.parser.OptimizerOptions
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.sxpath.{IndependentContext, XPathEvaluator}
import org.orbeon.saxon.utils.Configuration
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable.ListBuffer

trait XPathTestBase extends AnyFunSpec {

  val configuration = {
    val config = new Configuration
    config.optimizerOptions = new OptimizerOptions("vmtl")
    config.setDefaultRegexEngine("S")
    config
  }

  def eval(xpath: String, ctx: Item = null): Option[Item] = {
    val evaluator = new XPathEvaluator(configuration)
    val staticContext = evaluator.staticContext.asInstanceOf[IndependentContext]

    staticContext.declareNamespace("fn",    NamespaceConstant.FN)
    staticContext.declareNamespace("xs",    NamespaceConstant.SCHEMA)
    staticContext.declareNamespace("math",  NamespaceConstant.MATH)
    staticContext.declareNamespace("map",   NamespaceConstant.MAP_FUNCTIONS)
    staticContext.declareNamespace("array", NamespaceConstant.ARRAY_FUNCTIONS)

    val xpe = evaluator.createExpression(xpath)
    val dc = xpe.createDynamicContext
    if (ctx != null)
      dc.setContextItem(ctx)

    Option(xpe.evaluateSingle(dc))
  }

  def evalAll(xpath: String, ctx: Item = null): List[Item] = {
    val evaluator = new XPathEvaluator(configuration)
    val staticContext = evaluator.staticContext.asInstanceOf[IndependentContext]

    staticContext.declareNamespace("fn",    NamespaceConstant.FN)
    staticContext.declareNamespace("xs",    NamespaceConstant.SCHEMA)
    staticContext.declareNamespace("math",  NamespaceConstant.MATH)
    staticContext.declareNamespace("map",   NamespaceConstant.MAP_FUNCTIONS)
    staticContext.declareNamespace("array", NamespaceConstant.ARRAY_FUNCTIONS)

    val xpe = evaluator.createExpression(xpath)
    val dc = xpe.createDynamicContext
    if (ctx != null)
      dc.setContextItem(ctx)

    val iter = xpe.iterate(dc)
    val buf = new ListBuffer[Item]()
    var item = iter.next()
    while (item != null) {
      buf += item
      item = iter.next()
    }
    iter.close()
    buf.toList
  }

  def evalString(xpath: String, ctx: Item = null): String =
    eval(xpath, ctx).map(_.getStringValue).orNull

  def evalStrings(xpath: String, ctx: Item = null): List[String] =
    evalAll(xpath, ctx).map(_.getStringValue)

  def check(xpath: String, expected: String, ctx: Item = null): Unit = {
    it(s"must evaluate `$xpath` to `$expected`") {
      assert(evalString(xpath, ctx) == expected)
    }
  }

  def checkStrings(xpath: String, expected: List[String], ctx: Item = null): Unit = {
    it(s"must evaluate `$xpath` to $expected") {
      assert(evalStrings(xpath, ctx) == expected)
    }
  }
}
