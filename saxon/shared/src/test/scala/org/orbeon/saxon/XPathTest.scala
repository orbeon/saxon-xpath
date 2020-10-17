package org.orbeon.saxon

import java.io.PrintStream

import org.orbeon.saxon.lib.{NamespaceConstant, StandardLogger}
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.sxpath.{IndependentContext, XPathEvaluator}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.Int64Value

//import org.orbeon.dom

import org.scalatest.funspec.AnyFunSpec

class XPathTest extends AnyFunSpec {

  def compileAndRunExpression(xpath: String): Item = {

    val evaluator = new XPathEvaluator(new Configuration)

    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("fn",   NamespaceConstant.FN)
    evaluator.staticContext.asInstanceOf[IndependentContext].declareNamespace("math", NamespaceConstant.MATH)

    val xpe = evaluator.createExpression(xpath)

    val dc = xpe.createDynamicContext
    dc.setContextItem(Int64Value.makeDerived(2020, BuiltInAtomicType.INT))

    xpe.evaluateSingle(dc)
  }

//  val doc = dom.Document(dom.Element("root"))

  describe("Minimalistic expression compilation and execution") {

    val Expected = List(
      "."                                           -> "2020",
      "42"                                          -> "42",
      "42 + 1"                                      -> "43",
      "2 + 3 * 4 - 5 * 2"                           -> "4",
      "(2 + 3) * 4 - 5 * 2"                         -> "10",
      "'To be, or not to be, that is the question'" -> "To be, or not to be, that is the question",
      "let $a := 42 return $a"                      -> "42",
      "let $a := 1, $b := 2 return $a + $b"         -> "3",
      "3.1415"                                      -> "3.1415",
      "fn:concat('To be', ', or not to be')"        -> "To be, or not to be",
      "'To be' || ', or not to be'"                 -> "To be, or not to be",
      "math:cos(0)"                                 -> "1",
      "math:cos(math:pi())"                         -> "-1",
      """let $fn := function($v) { $v * 2 }
         return $fn(7)"""                           -> "14",
      """
        let $f :=
          function($seq, $delim) {
            fn:fold-left($seq, '', fn:concat(?, $delim, ?))
          },
          $paf := $f(?, '.')
        return
          $paf(1 to 5)
        """ -> ".1.2.3.4.5",
    )

    for ((in, out) <- Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in).getStringValue)
      }
  }
}