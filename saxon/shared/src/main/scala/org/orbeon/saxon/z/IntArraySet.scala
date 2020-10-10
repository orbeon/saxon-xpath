package org.orbeon.saxon.z

import java.util.Arrays

import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.z.IntArraySet._

import scala.util.control.Breaks._

object IntArraySet {

  val EMPTY_INT_ARRAY = new Array[Int](0)

  def make(in: Array[Int], size: Int): IntArraySet = {
    var out: Array[Int] = null
    if (in.length == size) {
      out = in
    } else {
      out = Array.ofDim[Int](size)
      System.arraycopy(in, 0, out, 0, size)
    }
    new IntArraySet(out)
  }

  class IntArrayIterator(private var contents: Array[Int],
                         private var limit: Int)
    extends IntIterator {

    private var i: Int = 0

    def hasNext: Boolean = i < limit

    def next(): Integer = contents({
      i += 1
      i - 1
    })
  }
}

class IntArraySet(private var contents: Array[Int]) extends IntSet {

  private var hashCodeVar: Int = -1

  def this() =
    this(EMPTY_INT_ARRAY)

  def this(input: IntHashSet) = {
    this(input.getValues)
    scala.util.Sorting.quickSort(contents)
  }

  def this(input: IntArraySet) = {
    this(Array.ofDim[Int](input.contents.length))
    System.arraycopy(input.contents, 0, contents, 0, contents.length)
  }

  def copy(): IntSet = {
    val i2 = new IntArraySet(Array.ofDim[Int](contents.length))
    System.arraycopy(contents, 0, i2.contents, 0, contents.length)
    i2
  }

  def mutableCopy(): IntSet = copy()

  def clear(): Unit = {
    contents = EMPTY_INT_ARRAY
    hashCodeVar = -1
  }

  def size: Int = contents.length
  def isEmpty: Boolean = contents.isEmpty
  def getValues: Array[Int] = contents
  def contains(value: Int): Boolean = Arrays.binarySearch(contents, value) >= 0

  def remove(value: Int): Boolean = {
    hashCodeVar = -1
    val pos: Int = Arrays.binarySearch(contents, value)
    if (pos < 0) {
      return false
    }
    val newArray: Array[Int] = Array.ofDim[Int](contents.length - 1)
    if (pos > 0) {
      System.arraycopy(contents, 0, newArray, 0, pos)
    }
    if (pos < newArray.length) {
      System.arraycopy(contents, pos + 1, newArray, pos, contents.length - pos)
    }
    contents = newArray
    true
  }

  def add(value: Int): Boolean = {
    hashCodeVar = -1
    if (contents.isEmpty) {
      contents = Array(value)
      return true
    }
    var pos: Int = Arrays.binarySearch(contents, value)
    if (pos >= 0) {
      return false
    }
    pos = -pos - 1
    val newArray: Array[Int] = Array.ofDim[Int](contents.length + 1)
    if (pos > 0) {
      System.arraycopy(contents, 0, newArray, 0, pos)
    }
    newArray(pos) = value
    if (pos < contents.length) {
      System.arraycopy(contents, pos, newArray, pos + 1, newArray.length - pos)
    }
    contents = newArray
    true
  }

  def getFirst: Int = contents(0)

  def iterator: IntIterator = new IntArrayIterator(contents, contents.length)

  override def union(other: IntSet): IntSet = {
    if (size == 0) {
      return other.copy()
    } else if (other.isEmpty) {
      return copy()
    } else if (other == IntUniversalSet.getInstance) {
      return other
    } else if (other.isInstanceOf[IntComplementSet]) {
      return other.union(this)
    }
    if (equals(other)) {
      return copy()
    }
    other match {
      case intArraySet: IntArraySet =>
        val merged: Array[Int] = Array.ofDim[Int](size + other.size)
        val a: Array[Int] = contents
        val b: Array[Int] = intArraySet.contents
        val m: Int = a.length
        val n: Int = b.length
        var o: Int = 0
        var i: Int = 0
        var j: Int = 0
        while (true) {
          if (a(i) < b(j)) {

            merged(o) = a(i)
            o += 1
            i += 1

          } else if (b(j) < a(i)) {
            merged(o) = b(j)
            o += 1
            j += 1
          } else {
            merged(o) = a(i)
            o += 1
            i += 1
            j += 1
          }
          if (i == m) {
            System.arraycopy(b, j, merged, o, n - j)
            o += (n - j)
            return make(merged, o)
          } else if (j == n) {
            System.arraycopy(a, i, merged, o, m - i)
            o += (m - i)
            return make(merged, o)
          }
        }
      case _ =>
        val n = new IntHashSet(size)
        val it = iterator
        while ( {
          it.hasNext
        }) {
          val v = it.next()
          if (!other.contains(v)) n.add(v)
        }
        return n
    }
    null
  }

  override def toString: String = {
    val sb = new FastStringBuffer(contents.length * 4)
    var i = 0
    while (i < contents.length) {
      if (i == contents.length - 1) {
        sb.append(contents(i).toString)
      } else if (contents(i) + 1 != contents(i + 1)) {
        sb.append(contents(i) .toString+ ",")
      } else {
        var j: Int = i + 1
        breakable {
          while (contents(j) == contents(j - 1) + 1) {
            j += 1
            if (j == contents.length) {
              break()
            }
          }
        }
        sb.append(contents(i).toString + "-" + contents(j - 1).toString + ",")
        i = j - 1
      }
    }
    sb.toString
  }

  override def equals(other: Any): Boolean =
    other match {
      case s: IntArraySet =>
        hashCode == other.hashCode && Arrays.equals(contents, s.contents)
      case _ =>
        other.isInstanceOf[IntSet]                           &&
          contents.length == other.asInstanceOf[IntSet].size &&
          containsAll(other.asInstanceOf[IntSet])
    }

  override def hashCode: Int = {
    if (hashCodeVar == -1) {
      var h = 936247625
      val it = iterator
      while (it.hasNext)
        h += it.next
      return h
    }
    hashCodeVar
  }
}
