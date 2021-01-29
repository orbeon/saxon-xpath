package org.orbeon.saxon.value

import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType}
import org.orbeon.saxon.tree.util.FastStringBuffer

import java.nio.ByteBuffer
import java.nio.charset.Charset
import scala.util.control.Breaks._


object AnyURIValue {

  /*@NotNull*/
  val EMPTY_URI: AnyURIValue = new AnyURIValue("")

  /*@Nullable*/
  def decode(s: String): String = {
    if (s == null)
      return s
    val n = s.length
    if (n == 0)
      return s
    if (s.indexOf('%') < 0)
      return s
    val sb   = new FastStringBuffer(n)
    val bb   = ByteBuffer.allocate(n)
    val utf8 = Charset.forName("UTF-8")
    // This is not horribly efficient, but it will do for now
    var c = s.charAt(0)
    var betweenBrackets          = false
    var i                        = 0
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
            break()
          }
          c = s.charAt(i)
          //          continue
        }
        bb.clear()

        while (true) {
          assert(n - i >= 2)
          i += 1
          bb.put(hex(s.charAt(i), s.charAt(i)))
          if (i >= n)
            break()
          c = s.charAt(i)
          if (c != '%')
            break()
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

  private def hex(high: Char, low: Char): Byte =
    ((hexToDec(high) << 4) | hexToDec(low)).toByte

  private def hexToDec(c: Char): Int =
    if (c >= '0' && c <= '9')
      c - '0'
    else if (c >= 'a' && c <= 'f')
      c - 'a' + 10
    else if (c >= 'A' && c <= 'F')
      c - 'A' + 10
    else
      0
}

class AnyURIValue(value: CharSequence, `type`: AtomicType = BuiltInAtomicType.ANY_URI)
  extends StringValue(if (value == null) "" else Whitespace.collapseWhitespace(value).toString, `type`) {

  /*@NotNull*/
  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v = new AnyURIValue(value)
    v.typeLabel = typeLabel
    v
  }

  override def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.ANY_URI
}
