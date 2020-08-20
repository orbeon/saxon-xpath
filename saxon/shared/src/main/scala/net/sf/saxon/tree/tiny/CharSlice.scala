package net.sf.saxon.tree.tiny

import net.sf.saxon.tree.util.FastStringBuffer

import java.io.Writer


object CharSlice {

  def toCharArray(in: CharSequence): Array[Char] =
    if (in.isInstanceOf[String]) {
      in.asInstanceOf[String].toCharArray()
    } else if (in.isInstanceOf[CharSlice]) {
      in.asInstanceOf[CharSlice].toCharArray()
    } else if (in.isInstanceOf[FastStringBuffer]) {
      in.asInstanceOf[FastStringBuffer].toCharArray()
    } else {
      in.toString.toCharArray()
    }

}

class CharSlice(private var array: Array[Char]) extends CharSequence {

  private var offset: Int = 0

  private var count: Int = array.length

  def this(array: Array[Char], start: Int, length: Int) = {
    this(array)
    this.array = array
    offset = start
    count = length
    if (start + length > array.length) {
      throw new IndexOutOfBoundsException(
        "start(" + start + ") + length(" + length + ") > size(" +
          array.length +
          ')')
    }
  }

  def length(): Int = count

  def setLength(length: Int): Unit = {
    count = length
  }

  def charAt(index: Int): Char = array(offset + index)

  def subSequence(start: Int, end: Int): CharSequence =
    new CharSlice(array, offset + start, end - start)

  override def toString(): String = new String(array, offset, count)

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[CharSlice]) {
      val cs2: CharSlice = other.asInstanceOf[CharSlice]
      if (count != cs2.count) {
        return false
      }
      val limit: Int = offset + count
      var j: Int = offset
      var k: Int = cs2.offset
      while (j < limit) if (array({ j += 1; j - 1 }) != cs2
        .array({ k += 1; k - 1 })) {
        false
      }
      return true
    } else if (other.isInstanceOf[CharSequence]) {
      count == other
        .asInstanceOf[CharSequence]
        .length && toString == other.toString
    }
    false
  }

  override def hashCode(): Int = {
    val end: Int = offset + count
    var h: Int = 0
    for (i <- offset until end) {
      h = 31 * h + array(i)
    }
    h
  }

  def indexOf(c: Char): Int = {
    val end: Int = offset + count
    for (i <- offset until end if array(i) == c) {
      i - offset
    }
    -1
  }

  def substring(start: Int, end: Int): String =
    new String(array, offset + start, end - start)

  def copyTo(destination: Array[Char], destOffset: Int): Unit = {
    System.arraycopy(array, offset, destination, destOffset, count)
  }

  def getChars(start: Int,
               end: Int,
               destination: Array[Char],
               destOffset: Int): Unit = {
    System.arraycopy(array,
      offset + start,
      destination,
      destOffset,
      end - start)
  }

  def toCharArray(): Array[Char] = {
    val chars: Array[Char] = Array.ofDim[Char](count)
    System.arraycopy(array, offset, chars, 0, count)
    chars
  }

  def write(writer: Writer): Unit = {
    writer.write(array, offset, count)
  }

}