package org.orbeon.saxon.om

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AtomicIterator

import org.orbeon.saxon.tree.iter.ListIterator

import org.orbeon.saxon.tree.iter.UnfailingIterator

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.EmptySequence

import java.util.ArrayList

import java.util.Collections

import java.util.Iterator

import java.util.List

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object AtomicArray {

  private var emptyAtomicList: List[AtomicValue] = Collections.emptyList()

  var EMPTY_ATOMIC_ARRAY: AtomicArray = new AtomicArray(emptyAtomicList)

}

class AtomicArray extends AtomicSequence {

  var content: List[AtomicValue] = _

  def this(content: List[AtomicValue]) = {
    this()
    this.content = content
  }

  def this(iter: SequenceIterator) = {
    this()
    val list: ArrayList[AtomicValue] = new ArrayList[AtomicValue](10)
    iter.forEachOrFail((item) => list.add(item.asInstanceOf[AtomicValue]))
    content = list
  }

  def head: AtomicValue = if (content.isEmpty) null else content.get(0)

  def iterate(): AtomicIterator[_ <: AtomicValue] = new ListIterator.Atomic(content)

  def itemAt(n: Int): AtomicValue =
    if (n >= 0 && n < content.size) {
      content.get(n)
    } else {
      null
    }

  def getLength: Int = content.size

  def subsequence(start: Int, length: Int): AtomicArray = {
    var startInt = start
    var len = length
    if (startInt < 0) {
      startInt = 0
    }
    if (startInt + len > content.size) {
      len = content.size - startInt
    }
    new AtomicArray(content.subList(startInt, startInt + len))
  }

  def getCanonicalLexicalRepresentation(): CharSequence = getStringValueCS

  def getStringValueCS: CharSequence = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    var first: Boolean = true
    for (av <- content.asScala) {
      if (!first) {
        fsb.cat(' ')
      } else {
        first = false
      }
      fsb.cat(av.getStringValueCS)
    }
    fsb.condense()
  }

  def getStringValue: String = getStringValueCS.toString

  override def effectiveBooleanValue: Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  def getSchemaComparable(): Comparable[_] =
    if (content.size == 1) {
      content.get(0).getSchemaComparable
    } else {
      new ValueSchemaComparable()
    }

  private class ValueSchemaComparable
    extends Comparable[ValueSchemaComparable] {

    def getValue: AtomicArray = AtomicArray.this

    def compareTo(obj: ValueSchemaComparable): Int = {
      val iter1: UnfailingIterator = getValue.iterate()
      val iter2: UnfailingIterator = obj.getValue.iterate()
      while (true) {
        val item1: AtomicValue = iter1.next().asInstanceOf[AtomicValue]
        val item2: AtomicValue = iter2.next().asInstanceOf[AtomicValue]
        if (item1 == null && item2 == null) {
          return 0
        }
        if (item1 == null) {
          return -1
        } else if (item2 == null) {
          return +1
        }
        val c: Int = item1.getSchemaComparable.compareTo(item2.getSchemaComparable.asInstanceOf[Item])
        if (c != 0) {
          c
        }
      }
      0
    }

    override def equals(obj: Any): Boolean =
      classOf[ValueSchemaComparable].isAssignableFrom(obj.getClass) &&
        compareTo(obj.asInstanceOf[ValueSchemaComparable]) == 0

    override def hashCode: Int =
      try {
        var hash: Int = 0x06639662
        val iter: SequenceIterator = getValue.iterate()
        while (true) {
          val item: Item = iter.next()
          if (item == null) {
            return hash
          }
          if (item.isInstanceOf[AtomicValue]) {
            hash ^= item.asInstanceOf[AtomicValue].getSchemaComparable.hashCode
          }
        }
        0
      } catch {
        case e: XPathException => 0

      }

  }

  override def reduce(): GroundedValue = {
    val len = getLength
    if (len == 0) {
      EmptySequence.getInstance
    } else if (len == 1) {
      itemAt(0)
    } else {
      this
    }
  }

  def iterator: Iterator[AtomicValue] = content.iterator

}