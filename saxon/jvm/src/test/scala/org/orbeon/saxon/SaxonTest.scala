package org.orbeon.saxon

import net.sf.saxon.`type`.BuiltInAtomicType
import net.sf.saxon.expr.parser.OptimizerOptions
import net.sf.saxon.jaxp.SaxonTransformerFactory
import net.sf.saxon.lib.{NamespaceConstant, StandardLogger}
import net.sf.saxon.sxpath.{IndependentContext, XPathEvaluator}
import net.sf.saxon.value.Int64Value
import net.sf.saxon.{Configuration, om => som}
import org.scalatest.funspec.AnyFunSpec
import org.xml.sax.helpers.AttributesImpl

import _root_.java.io.ByteArrayOutputStream
import javax.xml.transform.OutputKeys
import javax.xml.transform.stream.StreamResult
import scala.jdk.CollectionConverters._


class SaxonTest extends AnyFunSpec {

  val ExplainExpressions = false

  val Configuration = new Configuration {
    optimizerOptions = new OptimizerOptions("vmt") // FIXME: temporarily remove the "l" option which fails
    setDefaultRegexEngine("J") // the "S" (Saxon) engine is broken at this time
  }

  def compileAndRunExpression(xpath: String, ctx: som.Item, isAVT: Boolean): Option[som.Item] = {

    val evaluator = new XPathEvaluator(Configuration)

    val staticContext = evaluator.getStaticContext.asInstanceOf[IndependentContext]

    val FnPrefix = "foo"
    val FnUri    = "http://example.org/foo"

    staticContext.declareNamespace("fn",     NamespaceConstant.FN)
    staticContext.declareNamespace("math",   NamespaceConstant.MATH)
    staticContext.declareNamespace(FnPrefix, FnUri)

    val xpe =
      if (isAVT)
        ???//evaluator.createValueTemplateExpression(xpath)
      else
        evaluator.createExpression(xpath)

    if (ExplainExpressions) {
      import _root_.java.io.PrintStream

      xpe.getInternalExpression.explain(new StandardLogger(new PrintStream(System.out)))
    }

    val dc = xpe.createDynamicContext
    dc.setContextItem(ctx)

    Option(xpe.evaluateSingle(dc))
  }

//  val doc = dom.Document(dom.Element("root"))

  describe("Minimalistic expression compilation and execution") {

    val int = Int64Value.makeDerived(2020, BuiltInAtomicType.INT)

    val doc = {
      val treeBuilder = som.TreeModel.TINY_TREE.makeBuilder(Configuration.makePipelineConfiguration)
//      val treeBuilder = om.TreeModel.LINKED_TREE.makeBuilder(Configuration.makePipelineConfiguration)

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

    val docElem = doc.children.asScala.head

    val doc2 = {
      val treeBuilder = som.TreeModel.TINY_TREE.makeBuilder(Configuration.makePipelineConfiguration)

      val handler = {
        val handler = new SaxonTransformerFactory(Configuration).newTransformerHandler
        handler.setResult(treeBuilder)
        handler
      }

      def writeText(t: String) =
        handler.characters(t.toCharArray, 0, t.length)

      val EmptyAtts = new AttributesImpl

      val NsAtts = new AttributesImpl
      NsAtts.addAttribute("http://example.org/foo", "bar", "foo:bar", "CDATA", "baz")

      handler.startDocument()
      handler.startElement("", "_", "_", EmptyAtts)

      handler.startElement("", "input-date-prop", "input-date-prop", EmptyAtts)
      writeText("[M]/[D]/[Y]")
      handler.endElement("", "input-date-prop", "input-date-prop")

      handler.startElement("", "format-en", "format-en", EmptyAtts)
      writeText("MDY")
      handler.endElement("", "format-en", "format-en")

      handler.startElement("", "format-lang", "format-lang", EmptyAtts)
      writeText("MDY")
      handler.endElement("", "format-lang", "format-lang")

      handler.startElement("", "with-att", "with-att", NsAtts)
      handler.endElement("", "with-att", "with-att")

      handler.endElement("", "_", "_")
      handler.endDocument()

      treeBuilder.getCurrentRoot
    }

    val doc3 = {
      val treeBuilder = som.TreeModel.TINY_TREE.makeBuilder(Configuration.makePipelineConfiguration)

      val handler = {
        val handler = new SaxonTransformerFactory(Configuration).newTransformerHandler
        handler.setResult(treeBuilder)
        handler
      }

      def writeText(t: String) =
        handler.characters(t.toCharArray, 0, t.length)

      val EmptyAtts = new AttributesImpl

      handler.startDocument()
      handler.startElement("", "_", "_", EmptyAtts)

      handler.startElement("", "item", "item", EmptyAtts)

      handler.startElement("", "bar", "bar", EmptyAtts)
      writeText("1300")
      handler.endElement("", "bar", "bar")

      handler.startElement("", "type", "type", EmptyAtts)
      writeText("foo")
      handler.endElement("", "type", "type")

      handler.endElement("", "item", "item")

      handler.endElement("", "_", "_")
      handler.endDocument()

      treeBuilder.getCurrentRoot
    }

    val Expected = List(
//      ("."                                          , int, false, "2020"),
//      ("42"                                         , int, false, "42"),
//      ("42 + 1"                                     , int, false, "43"),
//      ("2 + 3 * 4 - 5 * 2"                          , int, false, "4"),
//      ("(2 + 3) * 4 - 5 * 2"                        , int, false, "10"),
//      ("'To be, or not to be, that is the question'", int, false, "To be, or not to be, that is the question"),
//      ("let $a := 42 return $a"                     , int, false, "42"),
//      ("let $a := 1, $b := 2 return $a + $b"        , int, false, "3"),
//      ("3.1415"                                     , int, false, "3.1415"),
//      ("fn:concat('To be', ', or not to be')"       , int, false, "To be, or not to be"),
//      ("'To be' || ', or not to be'"                , int, false, "To be, or not to be"),
//      ("math:cos(0)"                                , int, false, "1"),
//      ("math:cos(math:pi())"                        , int, false, "-1"),
//      ("""let $fn := function($v) { $v * 2 }
//          return $fn(7)"""                          , int, false, "14"),
//      ("""let $f :=
//            function($seq, $delim) {
//              fn:fold-left($seq, '', fn:concat(?, $delim, ?))
//            },
//            $paf := $f(?, '.')
//          return
//            $paf(1 to 5)
//       """, int, false, ".1.2.3.4.5"),
//      ("""string(/)""",                                            doc,     false, "WileE.Coyote"),
//      ("""normalize-space(' abc ')""",                             doc,     false, "abc"),
//      ("""normalize-space(())""",                                  doc,     false, ""),
//      ("""string(/*[1])""",                                        doc,     false, "WileE.Coyote"),
//      ("""normalize-space(/)""",                                   doc,     false, "WileE.Coyote"),
//      ("""string(/*/*[1])""",                                      doc,     false, "Wile"),
//      ("""string(*[1])""",                                         docElem, false, "Wile"),
//      ("""string(*[3])""",                                         docElem, false, "Coyote"),
//      ("""string(/*/*[3])""",                                      doc,     false, "Coyote"),
//      ("""string-join(for $i in * return string($i), '')""",       doc,     false, "WileE.Coyote"),
//      ("""string-join((*[1]/string(), *[3]/string()), ' and ')""", docElem, false, "Wile and Coyote"),
//      ("""string-join(for $i in * return string($i), '/')""",      docElem, false, "Wile/E./Coyote"),
//      ("""*[3]/root()/*/*[1]""",                                   docElem, false, "Wile"),
//      ("""There are {41 + 1} {/}s""",                              docElem, true,  "There are 42 WileE.Coyotes"),
//      ("""for $n in name() return count($n)""",                    docElem, false, "1"),
//      ("if (true()) then 'x' else .",                              int,     false, "x"),
//      ("if (false()) then 'x' else .",                             int,     false, "2020"),
//      ("(*[1])/name(.) = 'first-name'",                            docElem, false, "true"),
//      ("(*[1])/name() = 'first-name'",                             docElem, false, "true"),
//      ("""There are {41 + 1} {*[3]}s""",                           docElem, true,  "There are 42 Coyotes"),
//      (""" "foo" """,                                              doc,     false, "foo"),
//      (""" 'foo' """,                                              doc,     false, "foo"),
//      ("""for $parent in .[
//            lower-case(string(*[1])) = ('wile', 'road')
//          ]
//          return
//            concat($parent/*[1]/string(), $parent/*[3]/string())
//        """, docElem, false, "WileCoyote"),
//      ("""string(first-name)""",                                   docElem, false, "Wile"),
//      ("""string(/root/first-name)""",                             doc,     false, "Wile"),
//      (
//        """
//          |let $format-en   := 'b',
//          |    $format-lang := format-lang/string()
//          |return
//          |    concat($format-en, $format-lang)
//          |
//          |""".stripMargin,                                        doc2.children.next(), false, "bMDY"),
      (
        """
          |let $format      := string(input-date-prop),
          |    $cleaned     := translate($format, '[01]', ''),
          |    $duplicate   := replace(replace(replace($cleaned,
          |                        'M', 'MM'),
          |                        'D', 'DD'),
          |                        'Y', 'YYYY'),
          |    $format-en   := format-en,
          |    $format-lang := format-lang/string(),
          |    $translated  := translate($duplicate, $format-en, $format-lang)
          |return
          |    $translated
          |
          |""".stripMargin,                                         doc2.children.asScala.head, false, "MM/DD/YYYY"),
      ("with-att/@*[local-name() = 'bar']",                         doc2.children.iterator.next(), false, "baz"),
      ("with-att/@*[namespace-uri(.) = 'http://example.org/foo']",  doc2.children.iterator.next(), false, "baz"),
      ("with-att/@*[namespace-uri() = 'http://example.org/foo']",   doc2.children.iterator.next(), false, "baz"),
      ("""count(*[last()]/preceding-sibling::*)""".stripMargin,     docElem,                       false, "2"),
      ("""xs:double(item[type = 'foo']/*[local-name() = 'bar'])""", doc3.children.iterator.next(), false, "1300"),
      ("""(2 - xs:double(8) + 9)""",                                docElem,                       false, "3"),
      (
        """(
          |  (2 - xs:double(8) + 9) *
          |  (
          |    (4 - 3) div (xs:double(8) - 2)
          |  ) + 2
          |) div 1000""".stripMargin, docElem, false, "0.0025"),
      ("""round(xs:decimal(10.4))""",                               docElem, false, "10"),
      ("""/*//*[local-name() = 'foo']""",                           docElem, false, null),
      ("""format-time(xs:time('15:07:23'),                    '[H01]:[m01]:[s01]',               'en', (), ())""", docElem, false, "15:07:23"),
      ("""format-time(xs:time('15:07:23'),                    '[h]:[m]:[s] [P,*-2]',             'en', (), ())""", docElem, false, "3:07:23 pm"),
      ("""format-date(xs:date('2022-12-06'),                  '[M]/[D]/[Y]',                     'en', (), ())""", docElem, false, "12/6/2022"),
      ("""format-dateTime(xs:dateTime('2022-12-06T15:07:23'), '[M]/[D]/[Y] [h]:[m]:[s] [P,*-2]', 'en', (), ())""", docElem, false, "12/6/2022 3:07:23 pm"),
      ("""format-dateTime(xs:dateTime('2022-12-06T15:07:23'), '[Y0001]-[M01]-[D01]T[H01]:[m01]:[s01]', 'en', (), 'Europe/London')""".stripMargin, docElem, false, "2022-12-06T15:07:23"),
      ("""format-dateTime(xs:dateTime('2023-11-02T19:04:01-07:00'), '[Y0001]-[M01]-[D01]T[H01]:[m01]:[s01][Z]', 'en', (), 'Europe/London')""".stripMargin, docElem, false, "2023-11-03T02:04:01+00:00"),
    )

    for ((in, ctx, isAVT, out) <- Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in, ctx, isAVT).map(_.getStringValue).orNull)
      }
  }

  // TODO: don't duplicate from `XPathTest`
  describe("XML serialization") {

    def serializeToByteArray: Array[Byte] = {

      val th = new SaxonTransformerFactory(Configuration).newTransformerHandler

      val t = th.getTransformer

      t.setOutputProperty(OutputKeys.METHOD, "xml")
      t.setOutputProperty(OutputKeys.ENCODING, "utf-8")
      t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")

      val os = new ByteArrayOutputStream
      th.setResult(new StreamResult(os))

      def writeText(t: String): Unit =
        th.characters(t.toCharArray, 0, t.length)

      th.startDocument()
      th.startElement("", "root", "root", new AttributesImpl)
      writeText("""! ” # $ % & ’ ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~""")
      th.endElement("", "root", "root")
      th.endDocument()

      os.toByteArray
    }

    it("must serialize and escape special characters") {
      val Expected = """<root>! ” # $ % &amp; ’ ( ) * + , - . / : ; &lt; = &gt; ? @ [ \ ] ^ _ ` { | } ~</root>"""
      assert(new String(serializeToByteArray, "utf-8") == Expected)
    }
  }
}