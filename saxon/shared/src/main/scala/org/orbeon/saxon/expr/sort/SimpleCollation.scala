package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.utils.Platform

import org.orbeon.saxon.utils.Version

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.lib.SubstringMatcher

import java.text.RuleBasedCollator

import java.util.Comparator

import SimpleCollation._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object SimpleCollation {

  private var platform: Platform = Version.platform

}

class SimpleCollation(private var uri: String,
                      @BeanProperty var comparator: Comparator[CharSequence])
  extends StringCollator {

  def getCollationURI(): String = uri

  def compareStrings(o1: CharSequence, o2: CharSequence): Int =
    comparator.compare(o1, o2)

  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean =
    comparator.compare(s1, s2) == 0

  def getCollationKey(s: CharSequence): AtomicMatchKey =
    platform.getCollationKey(this, s.toString)

  def getSubstringMatcher: SubstringMatcher = {
    if (comparator.isInstanceOf[SubstringMatcher]) {
      comparator.asInstanceOf[SubstringMatcher]
    }
    if (comparator.isInstanceOf[RuleBasedCollator]) {
      new RuleBasedSubstringMatcher(uri,
        comparator.asInstanceOf[RuleBasedCollator])
    }
    null
  }

}