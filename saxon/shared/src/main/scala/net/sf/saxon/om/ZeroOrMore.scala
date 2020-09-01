package net.sf.saxon.om

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.EmptySequence

import java.util.ArrayList

import java.util.Arrays

import java.util.Iterator

import java.util.List

class ZeroOrMore[T <: Item] extends GroundedValue with java.lang.Iterable[T] {

  private var content: List[T] = _

  def this(contentArr: Array[T]) {
    this()
    this.content = Arrays.asList(contentArr: _*)
  }

  def this(content: List[T]) = {
    this()
    this.content = content
  }

  def this(iter: SequenceIterator) = {
    this()
    content = new ArrayList()
    iter.forEachOrFail((item) => content.add(item.asInstanceOf[T]))
  }

  def head: T = if (content.isEmpty) null.asInstanceOf[T] else content.get(0)

  def iterate(): ListIterator[T] = new ListIterator(content)

  def iterator(): Iterator[T] = content.iterator()

  def itemAt(n: Int): T =
    if (n >= 0 && n < content.size) {
      content.get(n)
    } else {
      null.asInstanceOf[T]
    }

  def subsequence(start: Int, length: Int): ZeroOrMore[T] = {
    var startInt = start
    var len = length
    if (startInt < 0) {
      startInt = 0
    }
    if (startInt + len > content.size) {
      len = content.size - startInt
    }
    new ZeroOrMore(content.subList(startInt, startInt + len))
  }

  def getLength: Int = content.size

  override def effectiveBooleanValue(): Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  def getStringValue: String = SequenceTool.getStringValue(this)

  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  override def reduce(): GroundedValue = {
    if (content.isEmpty) {
      EmptySequence.getInstance
    } else if (content.size == 1) {
      val first: T = content.get(0)
      if (first.isInstanceOf[AtomicValue]) {
        return first
      } else {
        new One(head)
      }
    }
    this
  }

}
