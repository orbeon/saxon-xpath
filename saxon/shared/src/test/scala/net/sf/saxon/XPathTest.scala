package net.sf.saxon

import net.sf.saxon.s9api.XdmAtomicValue
import utest._

object XPathTest extends TestSuite {

  def tests = Tests {
    test("Minimalistic expression compilation and execution") {
      val p = new s9api.Processor
      val c = p.newXPathCompiler()
      val executable = c.compile("42")
      val selector = executable.load()
      selector.setContextItem(new XdmAtomicValue(1))
      val result = selector.evaluateSingle()
      println(s"result `${result.toString}`")
    }
  }
}