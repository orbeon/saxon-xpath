package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.serialize.codenorm.Normalizer

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.tiny.CompressedWhitespace

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.Whitespace

import NormalizeUnicode._

import scala.util.control.Breaks._

object NormalizeUnicode {

  def normalize(sv: StringValue, form: String, c: XPathContext): StringValue = {
    var fb: Byte = 0
    if (form.equalsIgnoreCase("NFC")) {
      fb = Normalizer.C.toByte
    } else if (form.equalsIgnoreCase("NFD")) {
      fb = Normalizer.D.toByte
    } else if (form.equalsIgnoreCase("NFKC")) {
      fb = Normalizer.KC.toByte
    } else if (form.equalsIgnoreCase("NFKD")) {
      fb = Normalizer.KD.toByte
    } else if (form.isEmpty) {
      return sv
    } else {
      val msg: String = "Normalization form " + form + " is not supported"
      val err: XPathException = new XPathException(msg)
      err.setErrorCode("FOCH0003")
      err.setXPathContext(c)
      throw err
    }
    var allASCII: Boolean = true
    val chars: CharSequence = sv.getStringValueCS
    if (chars.isInstanceOf[CompressedWhitespace]) return sv
    var i: Int = chars.length - 1
    breakable {
      while (i >= 0) {
        if (chars.charAt(i) > 127) {
          allASCII = false
          break
        }
        i -= 1
      }
    }
    if (allASCII) return sv
    val norm: Normalizer = Normalizer.make(fb, c.getConfiguration)
    val result: CharSequence = norm.normalize(sv.getStringValueCS)
    StringValue.makeStringValue(result)
  }

}

class NormalizeUnicode extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val sv: StringValue = arguments(0).head().asInstanceOf[StringValue]
    if (sv == null) {
      StringValue.EMPTY_STRING
    }
    val nf: String =
      if (arguments.length == 1) "NFC"
      else Whitespace.trim(arguments(1).head().getStringValue)
    normalize(sv, nf, context)
  }

}
