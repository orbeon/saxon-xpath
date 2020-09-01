
package net.sf.saxon.regex

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.value.StringValue

import java.util.function.Function

import java.util.regex.Matcher

import java.util.regex.Pattern

import java.util.regex.PatternSyntaxException

import JavaRegularExpression._




object JavaRegularExpression {

  def setFlags(inFlags: CharSequence): Int = {
    var flags: Int = Pattern.UNIX_LINES
    for (i <- 0 until inFlags.length) {
      val c: Char = inFlags.charAt(i)
      c match {
        case 'd' => flags |= Pattern.UNIX_LINES
        case 'm' => flags |= Pattern.MULTILINE
        case 'i' => flags |= Pattern.CASE_INSENSITIVE
        case 's' => flags |= Pattern.DOTALL
        case 'x' =>
          flags |= Pattern.COMMENTS
        case 'u' => flags |= Pattern.UNICODE_CASE
        case 'q' => flags |= Pattern.LITERAL
        case 'c' => flags |= Pattern.CANON_EQ
        case _ =>
          var err: XPathException = new XPathException(
            "Invalid character '" + c + "' in regular expression flags")
          err.setErrorCode("FORX0001")
          throw err

      }
    }
    flags
  }

}


class JavaRegularExpression(javaReg: CharSequence, flags: String)
    extends RegularExpression {

  var javaRegex: String = javaReg.toString

  var flagBits: Int = JavaRegularExpression.setFlags(flags)

  var pattern: Pattern =
    Pattern.compile(this.javaRegex, flagBits & (~(Pattern.COMMENTS)))

  def getJavaRegularExpression: String = javaRegex

  def getFlagBits: Int = flagBits

  def analyze(input: CharSequence): RegexIterator =
    new JRegexIterator(input.toString, pattern)

  def containsMatch(input: CharSequence): Boolean =
    pattern.matcher(input).find()

  def matches(input: CharSequence): Boolean = pattern.matcher(input).matches()

  def replace(input: CharSequence, replacement: CharSequence): CharSequence = {
    val matcher: Matcher = pattern.matcher(input)
    matcher.replaceAll(replacement.toString)
  }


  override def replaceWith(
      input: CharSequence,
      replacement: Function[CharSequence, CharSequence]): CharSequence =
    throw new XPathException(
      "saxon:replace-with() is not supported with the Java regex engine")

  def tokenize(input: CharSequence): AtomicIterator[StringValue] = {
    if (input.length == 0) {
      EmptyIterator.ofAtomic()
    }
    new JTokenIterator(input, pattern)
  }


  override def getFlags(): String = {
    var flags: String = ""
    if ((flagBits & Pattern.UNIX_LINES) != 0) {
      flags += 'd'
    }
    if ((flagBits & Pattern.MULTILINE) != 0) {
      flags += 'm'
    }
    if ((flagBits & Pattern.CASE_INSENSITIVE) != 0) {
      flags += 'i'
    }
    if ((flagBits & Pattern.DOTALL) != 0) {
      flags += 's'
    }
    if ((flagBits & Pattern.COMMENTS) != 0) {
      flags += 'x'
    }
    if ((flagBits & Pattern.UNICODE_CASE) != 0) {
      flags += 'u'
    }
    if ((flagBits & Pattern.LITERAL) != 0) {
      flags += 'q'
    }
    if ((flagBits & Pattern.CANON_EQ) != 0) {
      flags += 'c'
    }
    flags
  }

}








