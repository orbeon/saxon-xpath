package org.orbeon.saxon.expr.number

import org.orbeon.saxon.regex.UnicodeString

import org.orbeon.saxon.tree.util.FastStringBuffer

class RegularGroupFormatter(private var groupSize: Int,
                            private var groupSeparator: String,
                            adjustedPicture: UnicodeString)
  extends NumericGroupFormatter {

  var adjustPic = adjustedPicture
  this.adjustPic = adjustedPicture

  override def format(value: FastStringBuffer): String =
    if (groupSize > 0 && groupSeparator.length > 0) {
      val valueEx: UnicodeString = UnicodeString.makeUnicodeString(value)
      val temp: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
      var i: Int = valueEx.uLength() - 1
      var j: Int = 0
      while (i >= 0) {
        if (j != 0 && (j % groupSize) == 0) {
          temp.prepend(groupSeparator)
        }
        temp.prependWideChar(valueEx.uCharAt(i))
        i -= 1
        j += 1
      }
      temp.toString
    } else {
      value.toString
    }

  override def getSeparator(): String = groupSeparator

}
