package org.orbeon.saxon.om

import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.om.Chain.ChainIterator.ChainPosition
import org.orbeon.saxon.om.Chain._
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.tree.iter.{GroundedIterator, UnfailingIterator}
import org.orbeon.saxon.value.{EmptySequence, SequenceExtent}

import java.{util => ju}
import scala.collection.immutable
import scala.jdk.CollectionConverters._


/**
 * A chain is an implementation of Sequence that represents the concatenation of
 * a number of subsequences.
 *
 * The most common use case is a recursive function that appends one item to a
 * sequence each time it is called. Each call of this function will create a Chain
 * with two subsequences, the first being a Chain and the second an individual item.
 * The design of the class is constrained by the need to handle this extreme case.
 *
 * Firstly, the iterator for the class cannot use simple recursion to navigate the
 * tree because it will often be too deep, causing StackOverflow. So it maintains its
 * own Stack (on the Java heap).
 *
 * Secondly, even using the heap will run out of space at about a million entries.
 * To prevent this, any Chains of size less than thirty items are amalgamated into
 * chunks of 30. Building larger chunks than this would cause insertion operations
 * to have linear performance (and thus the total cost of sequence construction
 * would be quadratic). The figure of 30 was chosen because elapsed time is almost
 * as good as with smaller chunks, and memory use during navigation is substantially
 * reduced.
 *
 * A Chain has two phases in its life-cycle. In the first phase, the Chain is mutable;
 * it can be extended using append() calls. In the second phase, the Chain is immutable;
 * further append() operations are not allowed. The transition from the first to the
 * second phase occurs when any of the methods itemAt(), reduce(), or subsequence()
 * is called.
 */
object Chain {

  object ChainIterator {
    private class ChainPosition(var chain: Chain, var offset: Int)
  }

  private class ChainIterator(private var thisChain: Chain)
    extends UnfailingIterator
      with GroundedIterator {

    private val queue: ju.Queue[UnfailingIterator] = new ju.LinkedList()
    private var stack: List[ChainPosition] = Nil

    stack ::= new ChainPosition(thisChain, 0)

    def next(): Item = {
      while (! queue.isEmpty) {
        var ui = queue.peek()
        while (ui != null) {
          val current = ui.next()
          if (current != null) current else {
            queue.remove()
            ui = queue.peek()
          }
        }
      }
      while (stack.nonEmpty) {
        val cp = stack.head
        if (cp.offset >= cp.chain.children.size) {
          stack = stack.tail
          //continue
        }
        val gv = cp.chain.children.get({
          cp.offset += 1
          cp.offset - 1
        })
        gv match {
          case chain: Chain =>
            stack ::= new ChainPosition(chain, 0)
          case item: Item =>
            item
          case _ =>
            queue.offer(gv.iterate())
            next()
        }
      }
      null
    }

    override def getProperties: immutable.Set[Property] = immutable.Set(GROUNDED)
    override def materialize: GroundedValue = thisChain

    def getResidue = new SequenceExtent(this)
  }
}

class Chain(private var children: ju.List[GroundedValue]) extends GroundedValue {

  private var extent: ju.List[Item] = null

  var size: Int = 0
  var copy: Boolean = false

  for (gv <- children.asScala) {
    gv match {
      case chain: Chain =>
        if (chain.children.size < 30) {
          size += chain.children.size
          copy = true
        } else {
          size += 1
        }
      case _ =>
        size += 1
    }
  }

  if (copy) {
    this.children = new ju.ArrayList(size)
    for (gv <- children.asScala) {
      gv match {
        case chain: Chain =>
          if (chain.children.size < 30)
            this.children.addAll(chain.children)
          else
            this.children.add(gv)
        case _ =>
          this.children.add(gv)
      }
    }
  }

  def head: Item = {
    if (extent != null)
      return if (extent.isEmpty) null else extent.get(0)
    for (seq <- children.asScala) {
      val head = seq.head
      if (head != null)
        return head
    }
    null
  }

  def iterate(): UnfailingIterator =
    if (extent != null)
      new org.orbeon.saxon.tree.iter.ListIterator(extent)
    else
      new ChainIterator(this)

  def append(item: Item): Unit = {
    if (extent != null)
      throw new IllegalStateException
    if (item != null)
      children.add(item.asInstanceOf[GroundedValue])
  }

  private def consolidate(): Unit =
    if (extent == null)
      extent = iterate().toList

  def itemAt(n: Int): Item =
    if (n == 0) {
      head
    } else {
      consolidate()
      if (n >= 0 && n < extent.size)
        extent.get(n)
      else
        null
    }

  def subsequence(start: Int, length: Int): GroundedValue = {
    var startInt = start
    consolidate()
    var newStart: Int = 0
    if (startInt < 0)
      startInt = 0
    else if (startInt >= extent.size)
      return EmptySequence.getInstance
    newStart = startInt
    var newEnd: Int = 0
    if (length == java.lang.Integer.MAX_VALUE) {
      newEnd = extent.size
    } else if (length < 0) {
      return EmptySequence.getInstance
    } else {
      newEnd = newStart + length
      if (newEnd > extent.size)
        newEnd = extent.size
    }
    new SequenceExtent(extent.subList(newStart, newEnd))
  }

  def getLength: Int =
    if (extent != null) {
      extent.size
    } else {
      var n = 0
      for (v <- children.asScala)
        n += v.getLength
      n
    }

  override def effectiveBooleanValue: Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  def getStringValue: String = SequenceTool.getStringValue(this)
  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  override def reduce(): GroundedValue = {
    consolidate()
    SequenceExtent.makeSequenceExtent(extent)
  }
}
