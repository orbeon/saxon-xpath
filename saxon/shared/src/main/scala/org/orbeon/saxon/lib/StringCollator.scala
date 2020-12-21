package org.orbeon.saxon.lib

import org.orbeon.saxon.expr.sort.AtomicMatchKey

trait StringCollator {
  def getCollationURI: String
  def compareStrings(o1: CharSequence, o2: CharSequence): Int
  def comparesEqual(s1: CharSequence, s2: CharSequence): Boolean
  def getCollationKey(s: CharSequence): AtomicMatchKey
}