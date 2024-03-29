package org.orbeon.saxon.value

import java.util.Arrays

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType}
import org.orbeon.saxon.om.SequenceTool
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer

import scala.beans.BeanProperty


class HexBinaryValue extends AtomicValue with AtomicMatchKey with Comparable[AnyRef] {

  @BeanProperty
  var binaryValue: Array[Byte] = _

  var in: CharSequence = _

  def this(in: CharSequence) {
    this()
    this.in = in
    val s = Whitespace.trimWhitespace(in)

    if ((s.length & 1) != 0) {
      val err = new XPathException("A hexBinary value must contain an even number of characters")
      err.setErrorCode("FORG0001")
      throw err
    }
    new Array[Byte](s.length / 2)
    for (i <- binaryValue.indices)
      binaryValue(i) = ((fromHex(s.charAt(2 * i)) << 4) + fromHex(s.charAt(2 * i + 1))).toByte
    typeLabel = BuiltInAtomicType.HEX_BINARY
  }

  def this(s: CharSequence, `type`: AtomicType) = {
    this(s)
    if ((s.length & 1) != 0)
      throw new IllegalArgumentException("A hexBinary value must contain an even number of characters")
    binaryValue = Array.ofDim[Byte](s.length / 2)
    for (i <- binaryValue.indices)
      binaryValue(i) = ((fromHex(s.charAt(2 * i)) << 4) + fromHex(s.charAt(2 * i + 1))).toByte
    typeLabel = `type`
  }

  def this(value: Array[Byte]) = {
    this()
    binaryValue = value
    typeLabel = BuiltInAtomicType.HEX_BINARY
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v = new HexBinaryValue(binaryValue)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.HEX_BINARY

  private def fromHex(c: Char): Int = {
    var d = "0123456789ABCDEFabcdef".indexOf(c)
    if (d > 15)
      d = d - 6
    if (d < 0) {
      val err = new XPathException("Invalid hexadecimal digit '" + c + "'")
      err.setErrorCode("FORG0001")
      throw err
    }
    d
  }

  def getPrimitiveStringValue: CharSequence = {
    val digits = "0123456789ABCDEF"
    val sb = new FastStringBuffer(binaryValue.length * 2)
    for (aBinaryValue <- binaryValue) {
      sb.cat(digits.charAt((aBinaryValue >> 4) & 0xf))
      sb.cat(digits.charAt(aBinaryValue & 0xf))
    }
    sb
  }

  def getLengthInOctets: Int = binaryValue.length

  def getSchemaComparable: Comparable[AnyRef] =
    new HexBinaryComparable().asInstanceOf[Comparable[AnyRef]]

  class HexBinaryComparable extends Comparable[HexBinaryComparable] {

    def getHexBinaryValue: HexBinaryValue = HexBinaryValue.this

    def compareTo(o: HexBinaryComparable): Int =
      if (Arrays.equals(getHexBinaryValue.binaryValue,
        o.getHexBinaryValue.binaryValue)) {
        0
      } else {
        SequenceTool.INDETERMINATE_ORDERING
      }

    override def equals(o: Any): Boolean = o match {
      case o: HexBinaryComparable => compareTo(o) == 0
      case _ => false
    }

    override def hashCode: Int = HexBinaryValue.this.hashCode
  }

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey = this

  override def equals(other: Any): Boolean = other match {
    case other: HexBinaryValue => Arrays.equals(binaryValue, other.binaryValue)
    case _ => false

  }

  override def hashCode: Int =
    Base64BinaryValue.byteArrayHashCode(binaryValue)

  def compareTo(o: AnyRef): Int = {
    val other = o.asInstanceOf[HexBinaryValue].binaryValue
    val len0 = binaryValue.length
    val len1 = other.length
    val shorter = java.lang.Math.min(len0, len1)
    for (i <- 0 until shorter) {
      val a = binaryValue(i).toInt & 0xff
      val b = other(i).toInt & 0xff
      if (a != b)
        return if (a < b) -1 else +1
    }
    java.lang.Integer.signum(len0 - len1)
  }
}