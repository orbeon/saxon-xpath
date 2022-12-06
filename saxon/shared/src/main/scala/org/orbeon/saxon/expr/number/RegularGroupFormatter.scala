package org.orbeon.saxon.expr.number

import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.tree.util.FastStringBuffer


class RegularGroupFormatter(
  private var groupSize     : Int,
  private var groupSeparator: String,
  val adjustedPicture       : UnicodeString
) extends NumericGroupFormatter {

  def format(value: FastStringBuffer): String =
    if (groupSize > 0 && groupSeparator.nonEmpty) {

      val valueEx = UnicodeString.makeUnicodeString(value)
      val temp    = new FastStringBuffer(FastStringBuffer.C16)

      var i = valueEx.uLength - 1
      var j = 0
      while (i >= 0) {
        if (j != 0 && (j % groupSize) == 0)
          temp.prepend(groupSeparator)
        temp.prependWideChar(valueEx.uCharAt(i))
        i -= 1
        j += 1
      }
      temp.toString
    } else {
      value.toString
    }

  def getSeparator: String = groupSeparator
}
