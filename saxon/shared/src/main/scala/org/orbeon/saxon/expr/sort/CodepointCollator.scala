package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.sort.CodepointCollator._
import org.orbeon.saxon.lib.{NamespaceConstant, StringCollator, SubstringMatcher}
import org.orbeon.saxon.regex.UnicodeString


object CodepointCollator {

  private val theInstance: CodepointCollator = new CodepointCollator()

  val getInstance: CodepointCollator = theInstance

  def compareCS(a: CharSequence, b: CharSequence): Int =
    a match {
      case unicodeString: UnicodeString if b.isInstanceOf[UnicodeString] =>
        unicodeString.compareTo(b.asInstanceOf[UnicodeString])
      case _ =>
        val alen: Int = a.length
        val blen: Int = b.length
        var i: Int = 0
        var j: Int = 0
        while (true) {
          if (i == alen) {
            if (j == blen) return 0 else return -1
          }
          if (j == blen) return +1
          i += 1
          var nexta: Int = a.charAt(i).toInt
          if (nexta >= 55296 && nexta <= 56319) {
            i += 1
            nexta = ((nexta - 55296) * 1024) + (a.charAt(i).toInt - 56320) + 65536
          }
          j += 1
          var nextb: Int = b.charAt(j).toInt
          if (nextb >= 55296 && nextb <= 56319) {
            j += 1
            nextb = ((nextb - 55296) * 1024) + (b.charAt(j).toInt - 56320) + 65536
          }
          val c: Int = nexta - nextb
          if (c != 0) return c
        }
        0
    }

}

class CodepointCollator extends StringCollator with SubstringMatcher {

  def getCollationURI: String = NamespaceConstant.CODEPOINT_COLLATION_URI

  def compareStrings(a: CharSequence, b: CharSequence): Int = compareCS(a, b)

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    s1 match {
      case str: String =>
        str.contentEquals(s2)
      case _: UnicodeString =>
        s1 == UnicodeString.makeUnicodeString(s2)
      case _ =>
        s1.length == s2.length && s1.toString == s2.toString
    }

  def contains(s1: String, s2: String): Boolean = s1.contains(s2)

  def endsWith(s1: String, s2: String): Boolean = s1.endsWith(s2)

  def startsWith(s1: String, s2: String): Boolean = s1.startsWith(s2)

  def substringAfter(s1: String, s2: String): String = {
    val i: Int = s1.indexOf(s2)
    if (i < 0) {
      return ""
    }
    s1.substring(i + s2.length)
  }

  def substringBefore(s1: String, s2: String): String = {
    val j: Int = s1.indexOf(s2)
    if (j < 0) {
      return ""
    }
    s1.substring(0, j)
  }

  def getCollationKey(s: CharSequence): AtomicMatchKey =
    UnicodeString.makeUnicodeString(s)
}
