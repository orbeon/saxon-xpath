package org.orbeon.saxon.value

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import java.util.Arrays
import java.util.StringTokenizer

import NestedIntegerValue._
import org.orbeon.saxon.om.Item

object NestedIntegerValue {

  var ONE: NestedIntegerValue = new NestedIntegerValue(Array(1))

  var TWO: NestedIntegerValue = new NestedIntegerValue(Array(2))

  def parse(v: String): NestedIntegerValue = {
    val st: StringTokenizer = new StringTokenizer(v, ".")
    val valuei: Array[Int] = Array.ofDim[Int](st.countTokens())
    var i: Int = 0
    while (st.hasMoreTokens()) {
      valuei(i) = java.lang.Integer.parseInt(st.nextToken())
      i += 1
    }
    new NestedIntegerValue(valuei)
  }

}

class NestedIntegerValue(v: String)
  extends AtomicValue
    with Comparable[AnyRef]
    with AtomicMatchKey {

  var value: Array[Int] = _

  typeLabel = BuiltInAtomicType.STRING

  parse(v)

  def this(`val`: Array[Int]) = {
    this("")
    typeLabel = BuiltInAtomicType.STRING
    value = `val`
  }

  def append(leaf: Int): NestedIntegerValue = {
    val v: Array[Int] = Array.ofDim[Int](value.length + 1)
    System.arraycopy(value, 0, v, 0, value.length)
    v(value.length) = leaf
    new NestedIntegerValue(v)
  }

  def getStem: NestedIntegerValue =
    if (value.length == 0) {
      null
    } else {
      val v: Array[Int] = Array.ofDim[Int](value.length - 1)
      System.arraycopy(value, 0, v, 0, v.length)
      new NestedIntegerValue(v)
    }

  def getDepth: Int = value.length

  def getLeaf: Int =
    if (value.length == 0) {
      -1
    } else {
      value(value.length - 1)
    }

  override def getSchemaComparable(): Comparable[AnyRef] = this.asInstanceOf

  override def getXPathComparable(ordered: Boolean,
                                  collator: StringCollator,
                                  implicitTimezone: Int): AtomicMatchKey = this

  override def equals(o: Any): Boolean =
    (o.isInstanceOf[NestedIntegerValue]) &&
      Arrays.equals(value, o.asInstanceOf[NestedIntegerValue].value)

  override def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.STRING

  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: NestedIntegerValue = new NestedIntegerValue(value)
    v.typeLabel = typeLabel
    v
  }

  override def getPrimitiveStringValue(): CharSequence = {
    val buffer: FastStringBuffer = new FastStringBuffer(value.length * 2)
    for (i <- 0 until value.length - 1) {
      buffer.append(value(i).toString + ".")
    }
    buffer.append(value(value.length - 1).toString)
    buffer
  }

  def compareTo(other: AnyRef): Int =
    if (!(other.isInstanceOf[NestedIntegerValue])) {
      throw new ClassCastException(
        "NestedIntegerValue is not comparable to " + other.getClass)
    } else {
      val v2: NestedIntegerValue = other.asInstanceOf[NestedIntegerValue]
      var i: Int = 0
      while (i < value.length && i < v2.value.length) {
        if (value(i) != v2.value(i)) {
          if (value(i) < v2.value(i)) {
            return -1
          } else {
            return 1
          }
        }
        i += 1
      }
      java.lang.Integer.signum(value.length - v2.value.length)
    }
}