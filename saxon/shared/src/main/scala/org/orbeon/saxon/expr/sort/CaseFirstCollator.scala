package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.trans.XPathException

object CaseFirstCollator {

  def makeCaseOrderedCollator(uri: String,
                              stringCollator: StringCollator,
                              caseOrder: String): StringCollator = {
    var strCollator = stringCollator
    caseOrder match {
      case "lower-first" =>
        strCollator = new CaseFirstCollator(strCollator, false, uri)
      case "upper-first" =>
        strCollator = new CaseFirstCollator(strCollator, true, uri)
      case _ =>
        throw new XPathException(
          "case-order must be lower-first, upper-first, or #default")

    }
    strCollator
  }
}

class CaseFirstCollator(base: StringCollator,
                        private var upperFirst: Boolean,
                        collationURI: String)
  extends StringCollator {

  private var baseCollator: StringCollator = base

  private var uri: String = collationURI

  def getCollationURI(): String = uri

  def compareStrings(a: CharSequence, b: CharSequence): Int = {
    val diff: Int = baseCollator.compareStrings(a, b)
    if (diff != 0) {
      return diff
    }
    var i: Int = 0
    var j: Int = 0
    while (true) {
      while (i < a.length && j < b.length && a.charAt(i) == b.charAt(j)) {
        i += 1;
        j += 1
      }
      while (i < a.length && !java.lang.Character.isLetter(a.charAt(i))) {
        i += 1
      }
      while (j < b.length && !java.lang.Character.isLetter(b.charAt(j))) {
        j += 1
      }
      if (i >= a.length) return 0
      if (j >= b.length) return 0

      val aFirst: Boolean = if (upperFirst) {
        i += 1
        java.lang.Character.isUpperCase(a.charAt(i))
      }
      else {
        i += 1
        java.lang.Character.isLowerCase(a.charAt(i))
      }
      val bFirst: Boolean = if (upperFirst) {
        j += 1
        java.lang.Character.isUpperCase(b.charAt(j))
      }
      else {
        j += 1
        java.lang.Character.isLowerCase(b.charAt(j))
      }
      if (aFirst && !bFirst) return -1
      if (bFirst && !aFirst) return +1
    }
    0
  }

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    compareStrings(s1, s2) == 0

  def getCollationKey(s: CharSequence): AtomicMatchKey =
    baseCollator.getCollationKey(s)

}
