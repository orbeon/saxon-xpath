package org.orbeon.saxon.om

import java.util._

import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.om.Chain.ChainIterator.ChainPosition
import org.orbeon.saxon.om.Chain._
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.tree.iter.{GroundedIterator, UnfailingIterator}
import org.orbeon.saxon.value.{EmptySequence, SequenceExtent}

import scala.collection.immutable
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object Chain {

  object ChainIterator {
    private class ChainPosition(var chain: Chain, var offset: Int)
  }

  private class ChainIterator(private var thisChain: Chain)
    extends UnfailingIterator
      with GroundedIterator {

    private val queue: Queue[UnfailingIterator] = new LinkedList()
    private val stack: Stack[ChainPosition] = new Stack()

    stack.push(new ChainPosition(thisChain, 0))

    def next(): Item = {
      while (!queue.isEmpty) {
        var ui: UnfailingIterator = queue.peek()
        while (ui != null) {
          val current: Item = ui.next()
          if (current != null) {
            current
          } else {
            queue.remove()
            ui = queue.peek()
          }
        }
      }
      while (!stack.isEmpty) {
        val cp: ChainPosition = stack.peek()
        if (cp.offset >= cp.chain.children.size) {
          stack.pop()
          //continue
        }
        val gv: GroundedValue = cp.chain.children.get({
          cp.offset += 1
          cp.offset - 1
        })
        gv match {
          case chain: Chain =>
            stack.push(new ChainPosition(chain, 0))
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
    override def materialize(): GroundedValue = thisChain
    def getResidue: GroundedValue = new SequenceExtent(this)

  }

}

class Chain(private var children: List[GroundedValue]) extends GroundedValue {

  private var extent: List[Item] = null

  var size: Int = 0

  var copy: Boolean = false

  for (gv <- children.asScala) {
    if (gv.isInstanceOf[Chain]) {
      if (gv.asInstanceOf[Chain].children.size < 30) {
        size += gv.asInstanceOf[Chain].children.size
        copy = true
      } else {
        {
          size += 1;
          size - 1
        }
      }
    } else {
      {
        size += 1;
        size - 1
      }
    }
  }

  if (copy) {
    this.children = new ArrayList(size)
    for (gv <- children.asScala) {
      if (gv.isInstanceOf[Chain]) {
        if (gv.asInstanceOf[Chain].children.size < 30) {
          this.children.addAll(gv.asInstanceOf[Chain].children)
        } else {
          this.children.add(gv)
        }
      } else {
        this.children.add(gv)
      }
    }
  }

  def head: Item = {
    if (extent != null)
      return if (extent.isEmpty) null else extent.get(0)
    for (seq <- children.asScala) {
      val head: Item = seq.head
      if (head != null) {
        head
      }
    }
    null
  }

  def iterate(): UnfailingIterator =
    if (extent != null) {
      new org.orbeon.saxon.tree.iter.ListIterator(extent)
    } else {
      new ChainIterator(this)
    }

  def append(item: Item): Unit = {
    if (extent != null) {
      throw new IllegalStateException()
    }
    if (item != null) {
      children.add(item.asInstanceOf[GroundedValue])
    }
  }

  private def consolidate(): Unit =
    if (extent == null)
      extent = iterate().toList

  def itemAt(n: Int): Item =
    if (n == 0) {
      head
    } else {
      consolidate()
      if (n >= 0 && n < extent.size) {
        extent.get(n)
      } else {
        null
      }
    }

  def subsequence(start: Int, length: Int): GroundedValue = {
    var startInt = start
    consolidate()
    var newStart: Int = 0
    if (startInt < 0) {
      startInt = 0
    } else if (startInt >= extent.size) {
      EmptySequence.getInstance
    }
    newStart = startInt
    var newEnd: Int = 0
    if (length == java.lang.Integer.MAX_VALUE) {
      newEnd = extent.size
    } else if (length < 0) {
      EmptySequence.getInstance
    } else {
      newEnd = newStart + length
      if (newEnd > extent.size) {
        newEnd = extent.size
      }
    }
    new SequenceExtent(extent.subList(newStart, newEnd))
  }

  def getLength: Int =
    if (extent != null) {
      extent.size
    } else {
      var n: Int = 0
      for (v <- children.asScala) {
        n += v.getLength
      }
      n
    }

  override def effectiveBooleanValue(): Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  def getStringValue: String = SequenceTool.getStringValue(this)
  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  override def reduce(): GroundedValue = {
    consolidate()
    SequenceExtent.makeSequenceExtent(extent)
  }
}
