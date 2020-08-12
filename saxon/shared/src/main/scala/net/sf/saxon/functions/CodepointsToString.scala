package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.NumericValue

import net.sf.saxon.value.StringValue

import java.util.function.IntPredicate

import CodepointsToString._

object CodepointsToString {

  def unicodeToString(chars: SequenceIterator,
                      checker: IntPredicate): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    while (true) {
      val nextInt: NumericValue = chars.next().asInstanceOf[NumericValue]
      if (nextInt == null) {
        return sb.condense()
      }
      val next: Long = nextInt.longValue()
      if (next < 0 || next > java.lang.Integer.MAX_VALUE || !checker.test(
        next.toInt)) {
        throw new XPathException(
          "codepoints-to-string(): invalid XML character [x" + java.lang.Integer
            .toHexString(next.toInt) +
            ']',
          "FOCH0001")
      }
      sb.appendWideChar(next.toInt)
    }
    null
  }

}

class CodepointsToString extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val chars: SequenceIterator = arguments(0).iterate()
    new StringValue(
      unicodeToString(chars,
        context.getConfiguration.getValidCharacterChecker.asInstanceOf[IntPredicate]))
  }

  override def getStreamerName(): String = "CodepointsToString"

}
