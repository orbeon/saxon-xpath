////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.tree.util.FastStringBuffer

import java.nio.ByteBuffer

import java.nio.charset.Charset

import util.control.Breaks._
import AnyURIValue._


object AnyURIValue {

  /*@NotNull*/

  val EMPTY_URI: AnyURIValue = new AnyURIValue("")

  /*@Nullable*/

  def decode(s: String): String = {
    if (s == null) {
      return s
    }
    val n: Int = s.length
    if (n == 0) {
      return s
    }
    if (s.indexOf('%') < 0) {
      return s
    }
    val sb: FastStringBuffer = new FastStringBuffer(n)
    val bb: ByteBuffer = ByteBuffer.allocate(n)
    val utf8: Charset = Charset.forName("UTF-8")
    // This is not horribly efficient, but it will do for now
    var c: Char = s.charAt(0)
    var betweenBrackets: Boolean = false
    var i: Int = 0
    breakable {
      while (i < n) {
        // Loop invariant
        assert(c == s.charAt(i))
        if (c == '[') {
          betweenBrackets = true
        } else if (betweenBrackets && c == ']') {
          betweenBrackets = false
        }
        if (c != '%' || betweenBrackets) {
          sb.cat(c)
          i += 1
          if (i >= n) {
            break
          }
          c = s.charAt(i)
          //          continue
        }
        bb.clear()

        while (true) {
          assert(n - i >= 2)
          i += 1
          bb.put(hex(s.charAt(i), s.charAt(i)))
          if (i >= n) {
            break
          }
          c = s.charAt(i)
          if (c != '%') {
            break
          }
        }
        bb.flip()
        sb.cat(utf8.decode(bb))
      }
    }
    sb.toString
  }

  // Evaluates all escapes in s, applying UTF-8 decoding if needed.  Assumes
  // that escapes are well-formed syntactically, i.e., of the form %XX.  If a
  // sequence of escaped octets is not valid UTF-8 then the erroneous octets
  // are replaced with '\uFFFD'.
  // Exception: any "%" found between "[]" is left alone. It is an IPv6 literal
  //            with a scope_id
  //
  // Evaluates all escapes in s, applying UTF-8 decoding if needed.  Assumes
  // that escapes are well-formed syntactically, i.e., of the form %XX.  If a
  // sequence of escaped octets is not valid UTF-8 then the erroneous octets
  // are replaced with '\uFFFD'.
  // Exception: any "%" found between "[]" is left alone. It is an IPv6 literal
  //            with a scope_id
  //

  private def hex(high: Char, low: Char): Byte =
    ((hexToDec(high) << 4) | hexToDec(low)).toByte

  private def hexToDec(c: Char): Int =
    if (c >= '0' && c <= '9') {
      c - '0'
    } else if (c >= 'a' && c <= 'f') {
      c - 'a' + 10
    } else if (c >= 'A' && c <= 'F') {
      c - 'A' + 10
    } else {
      0
    }

}

class AnyURIValue() extends StringValue {
  var charSeq: CharSequence = _

  typeLabel = BuiltInAtomicType.ANY_URI

  def this(value: CharSequence) = {
    this()
    this.charSeq =
      if (value == null) "" else Whitespace.collapseWhitespace(value).toString
  }

  def this(value: CharSequence, `type`: AtomicType) = {
    this()
    this.value =
      if (value == null) "" else Whitespace.collapseWhitespace(value).toString
    typeLabel = `type`
  }

  /*@NotNull*/

  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: AnyURIValue = new AnyURIValue(charSeq)
    v.typeLabel = typeLabel
    v
  }

  override def getPrimitiveType(): BuiltInAtomicType = BuiltInAtomicType.ANY_URI

}

