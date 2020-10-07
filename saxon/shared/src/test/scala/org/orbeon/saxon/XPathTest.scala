package org.orbeon.saxon

import org.orbeon.saxon.s9api.{XdmAtomicValue, XdmItem}

import org.scalatest.funspec.AnyFunSpec

class XPathTest extends AnyFunSpec {

  def compileAndRunExpression(xpath: String): XdmItem = {
    val p = new s9api.Processor
    val c = p.newXPathCompiler()
    c.declareNamespace("fn", "http://www.w3.org/2005/xpath-functions")
    val executable = c.compile(xpath)
    val selector = executable.load()
    selector.setContextItem(new XdmAtomicValue(2020))
    selector.evaluateSingle()
  }

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
//      """
//        let $f :=
//          function ($seq, $delim) {
//            fn:fold-left($seq, '', fn:concat(?, $delim, ?))
//          },
//          $paf := $f(?, ".")
//        return
//          $paf(1 to 5)
//        """ -> "TODO",
    )

    for ((in, out) <- Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in).toString)
      }
  }
}