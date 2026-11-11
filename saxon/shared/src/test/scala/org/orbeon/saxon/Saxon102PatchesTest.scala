package org.orbeon.saxon

import org.orbeon.saxon.functions.registry.{BuiltInFunctionSet, XPath30FunctionSet}
import org.orbeon.saxon.jaxp.SaxonTransformerFactory
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{AlphaCode, AnyFunctionType}
import org.orbeon.saxon.om.{NodeInfo, StructuredQName, TreeModel}
import org.orbeon.saxon.serialize.AdaptiveEmitter
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
import org.orbeon.saxon.tree.util.ProcInstParser
import org.orbeon.saxon.utils.Version
import org.orbeon.saxon.value.Int64Value
import org.xml.sax.helpers.AttributesImpl

import _root_.java.io.StringWriter
import _root_.java.util.Properties
import javax.xml.transform.sax.TransformerHandler

class Saxon102PatchesTest extends XPathTestBase {

  private val hasXmlParser: Boolean =
    try {
      Version.platform.loadParser()
      true
    } catch {
      case _: UnsupportedOperationException => false
    }

  private val doc: NodeInfo = {
    val treeBuilder = TreeModel.TINY_TREE.makeBuilder(configuration.makePipelineConfiguration)
    val handler: TransformerHandler = new SaxonTransformerFactory(configuration).newTransformerHandler
    handler.setResult(treeBuilder)

    def makeAtts(pairs: (String, String)*): AttributesImpl = {
      val atts = new AttributesImpl
      for ((name, value) <- pairs)
        atts.addAttribute("", name, name, "CDATA", value)
      atts
    }

    handler.startDocument()
    handler.startElement("", "r", "r", new AttributesImpl)

    handler.startElement("", "a", "a", makeAtts("id" -> "1"))
    handler.characters("1".toCharArray, 0, 1)
    handler.endElement("", "a", "a")

    handler.startElement("", "b", "b", makeAtts("id" -> "2"))
    handler.characters("2".toCharArray, 0, 1)
    handler.endElement("", "b", "b")

    handler.endElement("", "r", "r")
    handler.endDocument()

    treeBuilder.getCurrentRoot
  }

  describe("Saxon 10.1 to 10.2 Upstream Patches") {

    describe("Version") {
      it("must report Saxon 10.8 version") {
        assert(Version.getProductVersion == "10.8")
        assert(Version.getStructuredVersionNumber.sameElements(Array(10, 8, 31510)))
      }
    }

    describe("Bug #4556 & #4717: NumericType (525b0c58, 00cf3217)") {
      check("10 instance of xs:numeric", "true")
      check("10.5 instance of xs:numeric", "true")
      check("10e0 instance of xs:numeric", "true")
      check("'abc' instance of xs:numeric", "false")
    }

    describe("Bug #4558: AlphaCode.fromItemType for AnyFunctionType (c89e30f2)") {
      it("must return 'F' for AnyFunctionType without throwing an exception") {
        assert(AlphaCode.fromItemType(AnyFunctionType) == "F")
      }
    }

    describe("Bug #4600: Adaptive serialization of sequences (f98065b9)") {
      it("must serialize empty and non-empty sequences via AdaptiveEmitter") {
        val pipe = configuration.makePipelineConfiguration
        val props = new Properties()
        props.setProperty("method", "adaptive")
        props.setProperty("item-separator", ";")

        val sw1 = new StringWriter()
        val emitter1 = new AdaptiveEmitter(pipe, sw1)
        emitter1.setOutputProperties(props)
        val normalizer1 = configuration.getSerializerFactory.makeSequenceNormalizer(emitter1, props)
        normalizer1.open()
        normalizer1.close()
        assert(sw1.toString == "")

        val sw2 = new StringWriter()
        val emitter2 = new AdaptiveEmitter(pipe, sw2)
        emitter2.setOutputProperties(props)
        val normalizer2 = configuration.getSerializerFactory.makeSequenceNormalizer(emitter2, props)
        normalizer2.open()
        normalizer2.append(Int64Value.makeIntegerValue(1))
        normalizer2.append(Int64Value.makeIntegerValue(2))
        normalizer2.append(Int64Value.makeIntegerValue(3))
        normalizer2.close()
        assert(sw2.toString == "1;2;3")
      }
    }

    describe("Bug #4610, #4645, #4730: SlashExpression and HomogeneityChecker (fe77a876, a35007da, 6586d55b)") {
      checkStrings(
        "/r/(b, a)/@id/string()",
        List("1", "2"),
        doc
      )
      checkStrings(
        "(/r/(b, a))/string()",
        List("1", "2"),
        doc
      )
      checkStrings(
        "/r/(b, a)/(string() || '!')",
        List("1!", "2!"),
        doc
      )
    }

    describe("Bug #4582: format-dateTime language and fractional seconds (28964b25)") {
      check("format-date(xs:date('2020-05-01'), '[Y0001]-[M01]-[D01]', 'en-US', (), ())", "2020-05-01")
      check("format-date(xs:date('2020-05-01'), '[Y0001]-[M01]-[D01]', 'en-GB', (), ())", "2020-05-01")
      check("format-dateTime(xs:dateTime('2020-05-01T12:00:00.2564'), '[f001]')", "256")
      check("format-dateTime(xs:dateTime('2020-05-01T12:00:00.25'), '[f001]')", "250")
      check("format-dateTime(xs:dateTime('2020-05-01T12:00:00.25'), '[f1##]')", "25")
    }

    describe("Bug #4662 & #4643: EQName whitespace handling (7b88e6fe, c10300c0)") {
      check("Q{ http://www.w3.org/2005/xpath-functions }concat('a', 'b')", "ab")
      check("let $Q{  }x := 42 return $Q{}x", "42")
      check("Q{ http://www.w3.org/2005/xpath-functions }true()", "true")
      check("/Q{}r/Q{}a/@id/string()", "1", doc)
      check("/Q{  }r/Q{  }a/@id/string()", "1", doc)
      checkError("Q{http://example.com/{a}foo()", "XPST0003")
    }

    describe("Bug #4668: ProcInstParser pseudo-attribute parsing (6384a774, 0f24f60d)") {
      it("must parse valid pseudo-attributes and throw XPathException on malformed input") {
        if (hasXmlParser) {
          assert(ProcInstParser.getPseudoAttribute("href='test.xsl' type=\"text/xsl\"", "href") == "test.xsl")
          assert(ProcInstParser.getPseudoAttribute("href='test.xsl' type=\"text/xsl\"", "type") == "text/xsl")
          assert(ProcInstParser.getPseudoAttribute("href='a&#x20;b'", "href") == "a b")
          val ex = intercept[XPathException] {
            ProcInstParser.getPseudoAttribute("unclosed=\"abc", "unclosed")
          }
          assert(ex.getErrorCodeLocalPart == SaxonErrorCode.SXCH0005)
        }
      }
    }

    describe("Bug #4688: format-number exponential notation rounding to 10 (2f6fecc2)") {
      check("format-number(9.99, '0.0e0')", "1.0e1")
      check("format-number(999.9, '0.0e0')", "1.0e3")
    }

    describe("Bug #4692: MapType and SpecificFunctionType relationship (1184db5d)") {
      check("map {'a': 1} instance of function(xs:string) as item()*", "true")
      check("map {'a': 1} instance of function(xs:anyAtomicType) as item()*", "true")
    }

    describe("Bug #4721: parse-xml and parse-xml-fragment NEW property (237a738b)") {
      it("must mark parse-xml and parse-xml-fragment with CProperty.NEW and preserve distinct node identity") {
        val fnSet = XPath30FunctionSet.getInstance
        val parseXmlEntry = fnSet.getFunctionDetails("parse-xml", 1)
        val parseXmlFragEntry = fnSet.getFunctionDetails("parse-xml-fragment", 1)
        assert((parseXmlEntry.properties & BuiltInFunctionSet.NEW) != 0)
        assert((parseXmlFragEntry.properties & BuiltInFunctionSet.NEW) != 0)

        if (hasXmlParser) {
          assert(evalString("parse-xml('<a/>') is parse-xml('<a/>')") == "false")
          assert(evalString("parse-xml-fragment('<a/>') is parse-xml-fragment('<a/>')") == "false")
          assert(evalString("count(for $i in 1 to 3 return parse-xml('<a/>'))") == "3")
        }
      }
    }

    describe("Bug #4557: fn:apply first argument cardinality ONE (8126e879)") {
      checkError("apply((), [1])", "XPTY0004")
    }

    describe("Bug #4765: ForExpression where-clause promotion (a2490f66)") {
      checkStrings(
        "let $a := 1, $b := 2 return for $x in (10, 20) return if ($a = 1 and $b = 2) then $x else ()",
        List("10", "20")
      )
      checkStrings(
        "let $a := 1, $b := 9 return for $x in (10, 20) return if ($a = 1 and $b = 2) then $x else ()",
        Nil
      )
    }

    describe("Bug #4772: Diagnostics for map/array function call arguments (d2b458c0)") {
      checkError("let $a := [10, 20, 30] return $a('not-an-integer')", "XPTY0004")
      checkError("let $m := map {'a': 1} return $m(())", "XPTY0004")
    }

    describe("Bug #4775: Configuration.getSystemFunction (e5cb4f89)") {
      it("must retrieve built-in XPath 3.1 functions dynamically in Saxon-HE") {
        val concatFn = configuration.getSystemFunction(
          new StructuredQName("fn", NamespaceConstant.FN, "concat"),
          2
        )
        assert(concatFn != null)
        assert(concatFn.getArity == 2)
      }
    }
  }
}
