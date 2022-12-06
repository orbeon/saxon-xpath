package org.orbeon.saxon.expr.number

import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.z.IntSet

import java.util


class IrregularGroupFormatter(
  private var groupingPositions: IntSet,
  private var separators       : util.List[Integer],
  val adjustedPicture          : UnicodeString
) extends NumericGroupFormatter {

  def format(value: FastStringBuffer): String = {
    val in = UnicodeString.makeUnicodeString(value)
    var l      = 0
    var m      = 0
    while (l < in.uLength) {
      if (groupingPositions.contains(l))
        m += 1
      l += 1
    }
    val out = Array.ofDim[Int](in.uLength + m)
    var j = 0
    var k = out.length - 1
    var i = in.uLength - 1
    while (i >= 0) {
      out({
        k -= 1
        k + 1
      }) = in.uCharAt(i)
      if ((i > 0) && groupingPositions.contains(in.uLength - i)) {
        out({
          k -= 1
          k + 1
        }) = separators.get({
          j += 1
          j - 1
        })
      }
      i -= 1
    }
    UnicodeString.makeUnicodeString(out).toString
  }

  def getSeparator: String =
    if (separators.size == 0) {
      null
    } else {
      val sep = separators.get(separators.size - 1)
      val fsb = new FastStringBuffer(FastStringBuffer.C16)
      fsb.appendWideChar(sep)
      fsb.toString
    }
}
