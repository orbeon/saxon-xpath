package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.IntegerValue

import org.orbeon.saxon.value.NumericValue

import java.util.EnumSet

import Remove._

import SequenceIterator.Property._

object Remove {

  class RemoveIterator(var base: SequenceIterator, var removePosition: Int)
    extends SequenceIterator
      with LastPositionFinder {

    var basePosition: Int = 0

    var current: Item = null

    def next(): Item = {
      current = base.next()
      basePosition += 1
      if (current != null && basePosition == removePosition) {
        current = base.next()
        basePosition += 1
      }
      current
    }

    override def close(): Unit = {
      base.close()
    }

    def getLength: Int =
      if (base.isInstanceOf[LastPositionFinder]) {
        val x: Int = base.asInstanceOf[LastPositionFinder].getLength
        if (removePosition >= 1 && removePosition <= x) {
          x - 1
        } else {
          x
        }
      } else {
        throw new AssertionError(
          "base of removeIterator is not a LastPositionFinder")
      }

    override def getProperties: Set[Property] = base.getProperties.intersect(Set(LAST_POSITION_FINDER))

  }

}

class Remove extends SystemFunction {

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    if (Literal.isAtomic(arguments(1))) {
      val index: Sequence = arguments(1).asInstanceOf[Literal].value
      if (index.isInstanceOf[IntegerValue]) {
        try {
          val value: Long = index.asInstanceOf[IntegerValue].longValue
          if (value <= 0) {
            arguments(0)
          } else if (value == 1) {
            new TailExpression(arguments(0), 2)
          }
        } catch {
          case _: XPathException =>

        }
      }
    }
    super.makeFunctionCall(arguments.toIndexedSeq: _*)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val n: NumericValue = arguments(1).head.asInstanceOf[NumericValue]
    val pos: Int = n.longValue.toInt
    if (pos < 1) {
      arguments(0)
    }
    SequenceTool.toLazySequence2(
      new RemoveIterator(arguments(0).iterate(), pos))
  }

  override def getStreamerName: String = "Remove"

}
