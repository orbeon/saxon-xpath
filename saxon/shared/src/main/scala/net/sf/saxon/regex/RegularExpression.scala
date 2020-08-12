package net.sf.saxon.regex

import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import java.util.function.Function

import net.sf.saxon.value.AtomicValue

trait RegularExpression {

  def matches(input: CharSequence): Boolean

  def containsMatch(input: CharSequence): Boolean

  def tokenize(input: CharSequence): AtomicIterator[_ <: AtomicValue]

  def analyze(input: CharSequence): RegexIterator

  def replace(input: CharSequence, replacement: CharSequence): CharSequence

  def replaceWith(
                   input: CharSequence,
                   replacement: Function[CharSequence, CharSequence]): CharSequence

  def getFlags(): String

}
