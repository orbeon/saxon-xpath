//package org.orbeon.saxon.expr.sort
//
//import java.text.RuleBasedCollator
//import java.util.Comparator
//
//import org.orbeon.saxon.lib.{StringCollator, SubstringMatcher}
//import org.orbeon.saxon.utils.{Platform, Version}
//
//import scala.beans.BeanProperty
//
//object SimpleCollation {
//  private val platform: Platform = Version.platform
//}
//
//class SimpleCollation(private var uri: String,
//                      @BeanProperty var comparator: Comparator[CharSequence])
//  extends StringCollator {
//
//  def getCollationURI: String = uri
//
//  def compareStrings(o1: CharSequence, o2: CharSequence): Int =
//    comparator.compare(o1, o2)
//
//  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
//    comparator.compare(s1, s2) == 0
//
//  def getCollationKey(s: CharSequence): AtomicMatchKey = {
//    // ORBEON: Collations
//    ???
////    platform.getCollationKey(this, s.toString)
//  }
//
//  def getSubstringMatcher: SubstringMatcher = {
//    comparator match {
//      case substringMatcher: SubstringMatcher =>
//        return substringMatcher
//      case _ =>
//    }
//    if (comparator.isInstanceOf[RuleBasedCollator])
//      return new RuleBasedSubstringMatcher(uri, comparator.asInstanceOf[RuleBasedCollator])
//    null
//  }
//
//}
