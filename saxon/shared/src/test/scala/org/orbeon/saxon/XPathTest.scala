package org.orbeon.saxon

import org.orbeon.saxon.expr.parser.OptimizerOptions
import org.orbeon.saxon.jaxp.SaxonTransformerFactory
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.sxpath.{IndependentContext, XPathEvaluator}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Int64Value
import org.scalatest.funspec.AnyFunSpec
import org.xml.sax.helpers.AttributesImpl


class XPathTest extends AnyFunSpec {

  val ExplainExpressions = false

  val Configuration = new Configuration
  Configuration.optimizerOptions = new OptimizerOptions("vmt") // FIXME: temporarily remove the "l" option which fails

  def compileAndRunExpression(xpath: String, ctx: Item, isAVT: Boolean): Item = {

    val evaluator = new XPathEvaluator(Configuration)

    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("fn",   NamespaceConstant.FN)
    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("math", NamespaceConstant.MATH)

    val xpe =
      if (isAVT)
        evaluator.createValueTemplateExpression(xpath)
      else
        evaluator.createExpression(xpath)

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

    val int = Int64Value.makeDerived(2020, BuiltInAtomicType.INT)

    val doc = {
      val treeBuilder = om.TreeModel.TINY_TREE.makeBuilder(Configuration.makePipelineConfiguration)
//      val treeBuilder = om.TreeModel.LINKED_TREE.makeBuilder(config.makePipelineConfiguration)

      val handler = {
        val handler = new SaxonTransformerFactory(Configuration).newTransformerHandler
        handler.setResult(treeBuilder)
        handler
      }

      def writeText(t: String) =
        handler.characters(t.toCharArray, 0, t.length)

      val EmptyAtts = new AttributesImpl

      handler.startDocument()
      handler.startElement("", "root", "root", EmptyAtts)
      handler.startElement("", "first-name", "first-name", EmptyAtts)
      writeText("Wile")
      handler.endElement("", "first-name", "first-name")
      handler.startElement("", "middle-name", "middle-name", EmptyAtts)
      writeText("E.")
      handler.endElement("", "middle-name", "middle-name")
      handler.startElement("", "last-name", "last-name", EmptyAtts)
      writeText("Coyote")
      handler.endElement("", "last-name", "last-name")
      handler.endElement("", "root", "root")
      handler.endDocument()

      treeBuilder.getCurrentRoot
    }

    val docElem = doc.children.next()

    val Expected = List(
      ("."                                          , int, false, "2020"),
      ("42"                                         , int, false, "42"),
      ("42 + 1"                                     , int, false, "43"),
      ("2 + 3 * 4 - 5 * 2"                          , int, false, "4"),
      ("(2 + 3) * 4 - 5 * 2"                        , int, false, "10"),
      ("'To be, or not to be, that is the question'", int, false, "To be, or not to be, that is the question"),
      ("let $a := 42 return $a"                     , int, false, "42"),
      ("let $a := 1, $b := 2 return $a + $b"        , int, false, "3"),
      ("3.1415"                                     , int, false, "3.1415"),
      ("fn:concat('To be', ', or not to be')"       , int, false, "To be, or not to be"),
      ("'To be' || ', or not to be'"                , int, false, "To be, or not to be"),
      ("math:cos(0)"                                , int, false, "1"),
      ("math:cos(math:pi())"                        , int, false, "-1"),
      ("""let $fn := function($v) { $v * 2 }
         return $fn(7)"""                           , int, false, "14"),
      ("""
        let $f :=
          function($seq, $delim) {
            fn:fold-left($seq, '', fn:concat(?, $delim, ?))
          },
          $paf := $f(?, '.')
        return
          $paf(1 to 5)
        """, int, false, ".1.2.3.4.5"),
      ("""string(/)""",                                            doc,     false, "WileE.Coyote"),
      ("""normalize-space(' abc ')""",                             doc,     false, "abc"),
      ("""normalize-space(())""",                                  doc,     false, ""),
      ("""string(/*[1])""",                                        doc,     false, "WileE.Coyote"),
      ("""normalize-space(/)""",                                   doc,     false, "WileE.Coyote"),
      ("""string(/*/*[1])""",                                      doc,     false, "Wile"),
      ("""string(*[1])""",                                         docElem, false, "Wile"),
      ("""string(*[3])""",                                         docElem, false, "Coyote"),
      ("""string(/*/*[3])""",                                      doc,     false, "Coyote"),
      ("""string-join(for $i in * return string($i), '')""",       doc,     false, "WileE.Coyote"),
      ("""string-join((*[1]/string(), *[3]/string()), ' and ')""", docElem, false, "Wile and Coyote"),
      ("""*[3]/root()/*/*[1]""",                                   docElem, false, "Wile"),
      ("""There are {41 + 1} {/}s""",                              docElem, true,  "There are 42 WileE.Coyotes"),
      ("""for $n in name() return count($n)""",                    docElem, false, "1")
//      ("""There are {41 + 1} {*[3]}s""",                           docElem, true,  "There are 42 Coyotes"),
//      ("""string-join(for $i in * return string($i), '/')""",      doc,     false, "Wile/E./Coyote"), // FIXME: doesn't include '/'
//      ("""string(/root/first-name)""",                             doc,     false, "Wile"), // FIXME: returns blank
//      ("""string((if (normalize-space(/root/name) = '') then '' else concat('Hello, ', /root/name, '!'))[1]))""", doc, "xxxx"),
    )

    for ((in, ctx, isAVT, out) <- Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in, ctx, isAVT).getStringValue)
      }
  }
}