package net.sf.saxon.expr.sort

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.lib.SubstringMatcher

import net.sf.saxon.regex.UnicodeString

import CodepointCollator._

object CodepointCollator {

  private var theInstance: CodepointCollator = new CodepointCollator()

  def getInstance(): CodepointCollator = theInstance

  def compareCS(a: CharSequence, b: CharSequence): Int =
    if (a.isInstanceOf[UnicodeString] && b.isInstanceOf[UnicodeString]) {
      a.asInstanceOf[UnicodeString].compareTo(b.asInstanceOf[UnicodeString])
    } else {
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

  def getCollationURI(): String = NamespaceConstant.CODEPOINT_COLLATION_URI

  def compareStrings(a: CharSequence, b: CharSequence): Int = compareCS(a, b)

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    if (s1.isInstanceOf[String]) {
      s1.asInstanceOf[String].contentEquals(s2)
    } else if (s1.isInstanceOf[UnicodeString]) {
      s1 == UnicodeString.makeUnicodeString(s2)
    } else {
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
