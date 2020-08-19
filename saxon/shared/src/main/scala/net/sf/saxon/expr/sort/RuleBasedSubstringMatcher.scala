package net.sf.saxon.expr.sort

import net.sf.saxon.lib.SubstringMatcher
import net.sf.saxon.tree.util.FastStringBuffer
import java.text.CollationElementIterator
import java.text.RuleBasedCollator
import java.util.Comparator
import scala.util.control.Breaks._

object RuleBasedSubstringMatcher {

  def main(args: Array[String]): Unit = {
    val rules: String =
      " ='-'='*'< a < b < c < d < e < f < g < h < i < j < k < l < m < n < o < p < q < r < s < t < u < v < w < x < y < z"
    val collator: RuleBasedCollator = new RuleBasedCollator(rules)
    for (i <- 0 until args.length) {
      System.err.println(args(i))
      val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
      val iter: CollationElementIterator =
        collator.getCollationElementIterator(args(i))
      breakable {
        while (true) {
          val e: Int = iter.next()
          if (e == -1) break
          sb.append(s"$e" + " ")
        }
      }
      System.err.println(sb.toString)
    }
  }

}

class RuleBasedSubstringMatcher(uri: String, collator: RuleBasedCollator)
  extends SimpleCollation(uri, collator.asInstanceOf[Comparator[CharSequence]])
    with SubstringMatcher {

  private def getRuleBasedCollator(): RuleBasedCollator =
    getComparator.asInstanceOf[RuleBasedCollator]

  def contains(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationContains(iter1, iter2, null, false)
  }

  def endsWith(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationContains(iter1, iter2, null, true)
  }

  def startsWith(s1: String, s2: String): Boolean = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    collationStartsWith(iter1, iter2)
  }

  def substringAfter(s1: String, s2: String): String = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    val ia: Array[Int] = Array.ofDim[Int](2)
    val ba: Boolean = collationContains(iter1, iter2, ia, false)
    if (ba) {
      s1.substring(ia(1))
    } else {
      ""
    }
  }

  def substringBefore(s1: String, s2: String): String = {
    val collator: RuleBasedCollator = getRuleBasedCollator
    val iter1: CollationElementIterator =
      collator.getCollationElementIterator(s1)
    val iter2: CollationElementIterator =
      collator.getCollationElementIterator(s2)
    val ib: Array[Int] = Array.ofDim[Int](2)
    val bb: Boolean = collationContains(iter1, iter2, ib, false)
    if (bb) {
      s1.substring(0, ib(0))
    } else {
      ""
    }
  }

  private def collationStartsWith(s0: CollationElementIterator,
                                  s1: CollationElementIterator): Boolean = {
    while (true) {
      var e0: Int = 0
      var e1: Int = 0
      do e1 = s1.next() while (e1 == 0)
      if (e1 == -1) return true
      do e0 = s0.next() while (e0 == 0)
      if (e0 != e1) return false
    }
    false
  }

  private def collationContains(s0: CollationElementIterator,
                                s1: CollationElementIterator,
                                offsets: Array[Int],
                                matchAtEnd: Boolean): Boolean = {
    var e0: Int = 0
    var e1: Int = 0
    do e1 = s1.next() while (e1 == 0);
    if (e1 == -1) return true
    e0 = -1
    while (true) {
      while (e0 != e1) {
        do e0 = s0.next() while (e0 == 0);
        if (e0 == -1) return false
      }
      val start: Int = s0.getOffset
      if (collationStartsWith(s0, s1)) {
        if (matchAtEnd) {
          do e0 = s0.next() while (e0 == 0);
          if (e0 == -1) return true
        } else {
          if (offsets != null) {
            offsets(0) = start - 1
            offsets(1) = s0.getOffset
          }
          return true
        }
      }
      s0.setOffset(start)
      if (s0.getOffset != start) {
        s0.next()
      }
      s1.reset()
      e0 = -1
      do e1 = s1.next() while (e1 == 0);
    }
    false
  }

  override def getCollationKey(s: CharSequence): AtomicMatchKey =
    new CollationMatchKey(getRuleBasedCollator.getCollationKey(s.toString))

}
