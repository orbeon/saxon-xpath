package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.lib.{NamespaceConstant, StringCollator, SubstringMatcher}
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.tree.util.FastStringBuffer

object HTML5CaseBlindCollator {
  val getInstance: HTML5CaseBlindCollator = new HTML5CaseBlindCollator
}

class HTML5CaseBlindCollator extends StringCollator with SubstringMatcher {

  def getCollationURI: String =
    NamespaceConstant.HTML5_CASE_BLIND_COLLATION_URI

  def compareStrings(a: CharSequence, b: CharSequence): Int = compareCS(a, b)

  private def compareCS(a: CharSequence, b: CharSequence): Int = {
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
      j += 1
      var nextb: Int = b.charAt(j).toInt
      if (nexta >= 'a' && nexta <= 'z') {
        nexta += 'A' - 'a'
      }
      if (nextb >= 'a' && nextb <= 'z') {
        nextb += 'A' - 'a'
      }
      val c: Int = nexta - nextb
      if (c != 0) return c
    }
    0
  }

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    compareCS(s1, s2) == 0

  def contains(s1: String, s2: String): Boolean =
    normalize(s1).contains(normalize(s2))

  def endsWith(s1: String, s2: String): Boolean =
    normalize(s1).endsWith(normalize(s2))

  def startsWith(s1: String, s2: String): Boolean =
    normalize(s1).startsWith(normalize(s2))

  def substringAfter(s1: String, s2: String): String = {
    val i: Int = normalize(s1).indexOf(normalize(s2))
    if (i < 0) {
      return ""
    }
    s1.substring(i + s2.length)
  }

  def substringBefore(s1: String, s2: String): String = {
    val j: Int = normalize(s1).indexOf(normalize(s2))
    if (j < 0) {
      return ""
    }
    s1.substring(0, j)
  }

  def getCollationKey(s: CharSequence): AtomicMatchKey =
    UnicodeString.makeUnicodeString(normalize(s))

  private def normalize(cs: CharSequence): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(cs.length)
    for (i <- 0 until cs.length) {
      val c: Char = cs.charAt(i)
      if ('a' <= c && c <= 'z') {
        fsb.cat((c + 'A' - 'a').toChar)
      } else {
        fsb.cat(c)
      }
    }
    fsb.toString
  }
}
