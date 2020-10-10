package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

import java.util.Arrays

import IriToUri._

import ScalarSystemFunction._

object IriToUri {

  var allowedASCII: Array[Boolean] = new Array[Boolean](128)

  Arrays.fill(allowedASCII, 0, 32, false)

  Arrays.fill(allowedASCII, 33, 127, true)

  allowedASCII('"'.toInt) = false

  allowedASCII('<'.toInt) = false

  allowedASCII('>'.toInt) = false

  allowedASCII('\\'.toInt) = false

  allowedASCII('^'.toInt) = false

  allowedASCII('`'.toInt) = false

  allowedASCII('{'.toInt) = false

  allowedASCII('|'.toInt) = false

  allowedASCII('}'.toInt) = false

  def iriToUri(s: CharSequence): CharSequence = {
    if (allAllowedAscii(s)) {
      return s
    }
    val sb = new FastStringBuffer(s.length + 20)
    for (i <- 0 until s.length) {
      val c: Char = s.charAt(i)
      if (c >= 0x7f || !allowedASCII(c.toInt)) {
        EncodeForUri.escapeChar(
          c,
          if ((i + 1) < s.length) s.charAt(i + 1) else ' ',
          sb)
      } else {
        sb.cat(c)
      }
    }
    sb
  }

  private def allAllowedAscii(s: CharSequence): Boolean = {
    for (i <- 0 until s.length) {
      val c: Char = s.charAt(i)
      if (c >= 0x7f || !allowedASCII(c.toInt)) {
        false
      }
    }
    true
  }

  private val hex: String = "0123456789ABCDEF"

}

class IriToUri extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue = {
    val s: CharSequence = arg.getStringValueCS
    StringValue.makeStringValue(iriToUri(s))
  }

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

}
