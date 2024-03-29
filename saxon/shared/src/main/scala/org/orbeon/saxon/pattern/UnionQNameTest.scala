package org.orbeon.saxon.pattern

import java.util.{ArrayList, List}

import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.tree.util.FastStringBuffer

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class UnionQNameTest(testList: List[QNameTest]) extends QNameTest {

  var tests: List[QNameTest] = new ArrayList[QNameTest](testList)

  def matches(qname: StructuredQName): Boolean =
    tests.asScala.exists(_.matches(qname))

  override def toString: String = {
    var started: Boolean = false
    val fsb = new FastStringBuffer(FastStringBuffer.C256)
    for (qt <- tests.asScala) {
      if (started) {
        fsb.append("|")
      } else {
        started = true
      }
      fsb.append(qt.toString)
    }
    fsb.toString
  }

  override def exportQNameTest: String =
    tests.asScala.iterator.map(_.exportQNameTest).mkString(" ")

  override def generateJavaScriptNameTest(targetVersion: Int): String = {
    val fsb = new FastStringBuffer(256)
    var started: Boolean = false
    for (qt <- tests.asScala) {
      if (started) {
        fsb.append("||")
      } else {
        started = true
      }
      val test: String = qt.generateJavaScriptNameTest(targetVersion)
      fsb.append("(" + test + ")")
    }
    fsb.toString
  }
}
