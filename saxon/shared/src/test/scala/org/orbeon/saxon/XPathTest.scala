package org.orbeon.saxon



import org.orbeon.saxon.jaxp.SaxonTransformerFactory
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.sxpath.{IndependentContext, XPathEvaluator}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Int64Value
import org.xml.sax.helpers.AttributesImpl


import org.scalatest.funspec.AnyFunSpec

class XPathTest extends AnyFunSpec {

  val ExplainExpressions = false

  def compileAndRunExpression(xpath: String, ctx: Item, config: Configuration): Item = {

    val evaluator = new XPathEvaluator(config)

    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("fn",   NamespaceConstant.FN)
    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("math", NamespaceConstant.MATH)

    val xpe = evaluator.createExpression(xpath)

    if (ExplainExpressions) {
      import _root_.java.io.PrintStream
      import org.orbeon.saxon.lib.StandardLogger

      xpe.getInternalExpression.explain(new StandardLogger(new PrintStream(System.out)))
    }

    val dc = xpe.createDynamicContext
    dc.setContextItem(ctx)

    xpe.evaluateSingle(dc)
  }

//  val doc = dom.Document(dom.Element("root"))

  describe("Minimalistic expression compilation and execution") {

    val config = new Configuration

    val int = Int64Value.makeDerived(2020, BuiltInAtomicType.INT)

    val doc = {
      val treeBuilder = om.TreeModel.TINY_TREE.makeBuilder(config.makePipelineConfiguration)
//      val treeBuilder = om.TreeModel.LINKED_TREE.makeBuilder(config.makePipelineConfiguration)

      val handler = {
        val handler = new SaxonTransformerFactory(config).newTransformerHandler
        handler.setResult(treeBuilder)
        handler
      }

      handler.startDocument()
      handler.startElement("", "root", "root", new AttributesImpl)
      handler.startElement("", "name", "name", new AttributesImpl)
      val t = "World"
      handler.characters(t.toCharArray, 0, t.length)
      handler.endElement("", "name", "name")
      handler.endElement("", "root", "root")
      handler.endDocument()

      treeBuilder.getCurrentRoot
    }

    val Expected = List(
      ("."                                          , int, "2020"),
      ("42"                                         , int, "42"),
      ("42 + 1"                                     , int, "43"),
      ("2 + 3 * 4 - 5 * 2"                          , int, "4"),
      ("(2 + 3) * 4 - 5 * 2"                        , int, "10"),
      ("'To be, or not to be, that is the question'", int, "To be, or not to be, that is the question"),
      ("let $a := 42 return $a"                     , int, "42"),
      ("let $a := 1, $b := 2 return $a + $b"        , int, "3"),
      ("3.1415"                                     , int, "3.1415"),
      ("fn:concat('To be', ', or not to be')"       , int, "To be, or not to be"),
      ("'To be' || ', or not to be'"                , int, "To be, or not to be"),
      ("math:cos(0)"                                , int, "1"),
      ("math:cos(math:pi())"                        , int, "-1"),
      ("""let $fn := function($v) { $v * 2 }
         return $fn(7)"""                           , int, "14"),
      ("""
        let $f :=
          function($seq, $delim) {
            fn:fold-left($seq, '', fn:concat(?, $delim, ?))
          },
          $paf := $f(?, '.')
        return
          $paf(1 to 5)
        """, int, ".1.2.3.4.5"),
      ("""string(/)""", doc, "World"),
      ("""string(/*/*)""", doc, "World"),
      ("""normalize-space(' abc ')""", doc, "abc"),
      ("""normalize-space(()))""", doc, ""),
//      ("""normalize-space(/)""", doc, ""),
//      ("""string(/root/name)""", doc, "World"),
//      ("""string((if (normalize-space(/root/name) = '') then '' else concat('Hello, ', /root/name, '!'))[1]))""", doc, "xxxx"),
    )

    for ((in, ctx, out) <- Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in, ctx, config).getStringValue)
      }
  }
}