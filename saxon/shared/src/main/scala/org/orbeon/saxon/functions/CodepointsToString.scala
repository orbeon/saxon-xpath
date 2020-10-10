package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.NumericValue

import org.orbeon.saxon.value.StringValue

import java.util.function.IntPredicate

import CodepointsToString._

object CodepointsToString {

  def unicodeToString(chars: SequenceIterator,
                      checker: IntPredicate): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.C64)
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
        context.getConfiguration.getValidCharacterChecker))
  }

  override def getStreamerName: String = "CodepointsToString"

}
