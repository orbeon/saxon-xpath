package org.orbeon.saxon.expr.sort

import java.math.BigInteger
import java.util.regex.{Matcher, Pattern}

import org.orbeon.saxon.expr.sort.AlphanumericCollator._
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.tree.util.FastStringBuffer


object AlphanumericCollator {
  private val pattern: Pattern = Pattern.compile("\\d+")
  val PREFIX: String = "http://saxon.sf.net/collation/alphaNumeric?base="
}

class AlphanumericCollator(private var baseCollator: StringCollator)
    extends StringCollator {

  /**
    * Get the collation URI. It must be possible to use this collation URI to reconstitute the collation
    *
    * @return a collation URI that can be used to reconstruct the collation when an XSLT package is reloaded.
    */
  def getCollationURI: String = // it possible to reconstitute the collation easily
    PREFIX + baseCollator.getCollationURI

  def compareStrings(cs1: CharSequence, cs2: CharSequence): Int = {
    val s1: String = cs1.toString
    val s2: String = cs2.toString
    var pos1: Int = 0
    var pos2: Int = 0
    val m1: Matcher = pattern.matcher(s1)
    val m2: Matcher = pattern.matcher(s2)
    while (true) {
      val b1: Boolean = m1.find(pos1)
      val b2: Boolean = m2.find(pos2)
      val m1start: Int = if (b1) m1.start() else s1.length
      val m2start: Int = if (b2) m2.start() else s2.length
      var c: Int = baseCollator.compareStrings(s1.substring(pos1, m1start),
                                               s2.substring(pos2, m2start))
      if (c != 0)  return c
      if (b1 && !b2) return +1
      else if (b2 && !b1) return -1
       else if (!b1)  return 0

      val n1: BigInteger = new BigInteger(s1.substring(m1start, m1.end()))
      val n2: BigInteger = new BigInteger(s2.substring(m2start, m2.end()))
      c = n1.compareTo(n2)
      if (c != 0) return c
      pos1 = m1.end()
      pos2 = m2.end()
    }
    0
  }

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    compareStrings(s1, s2) == 0

  def getCollationKey(cs: CharSequence): AtomicMatchKey = {
// The string is normalized by removing leading zeros in a numeric component
    val s: String = cs.toString
    val sb: FastStringBuffer = new FastStringBuffer(s.length * 2)
    var pos1: Int = 0
    val m1: Matcher = pattern.matcher(s)
    while (true) {
      val b1: Boolean = m1.find(pos1)
      val m1start: Int = if (b1) m1.start() else s.length
      sb.append(baseCollator.getCollationKey(s.substring(pos1, m1start)).toString)
      if (!b1)  return UnicodeString.makeUnicodeString(sb)

      val n1: Int = java.lang.Integer.parseInt(s.substring(m1start, m1.end()))
      sb.append(n1.toString)
      pos1 = m1.end()
    }
    null
  }

}
