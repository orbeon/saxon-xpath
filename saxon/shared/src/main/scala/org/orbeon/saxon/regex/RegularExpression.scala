package org.orbeon.saxon.regex

import java.util.function.Function

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.AtomicValue


trait RegularExpression {
  def matches(input: CharSequence): Boolean
  def containsMatch(input: CharSequence): Boolean
  def tokenize(input: CharSequence): AtomicIterator[_ <: AtomicValue]
  def analyze(input: CharSequence): RegexIterator
  def replace(input: CharSequence, replacement: CharSequence): CharSequence
  def replaceWith(input: CharSequence, replacement: Function[CharSequence, CharSequence]): CharSequence
  def getFlags: String
}
