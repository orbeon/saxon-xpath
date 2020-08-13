package net.sf.saxon

import net.sf.saxon.s9api.{XdmAtomicValue, XdmItem}

import org.scalatest.funspec.AnyFunSpec

class XPathTest extends AnyFunSpec {

  def compileAndRunExpression(xpath: String): XdmItem = {
    val p = new s9api.Processor
    val c = p.newXPathCompiler()
    val executable = c.compile(xpath)
    val selector = executable.load()
    selector.setContextItem(new XdmAtomicValue(1))
    selector.evaluateSingle()
  }

  describe("Minimalistic expression compilation and execution") {

    val Expected = List(
      "42"     → "42",
      "42 + 1" → "43",
//      "3.1415" → "43",
      "'To be, or not to be, that is the question'" → "To be, or not to be, that is the question",
//      "concat('To be', ', or not to be')" → "To be, or not to be",
//      "'To be' || ', or not to be'" → "To be, or not to be",
    )

    for ((in, out) ← Expected)
      it(s"must evaluate `$in`") {
        assert(out == compileAndRunExpression(in).toString)
      }
  }
}