package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.NumericValue

import InsertBefore._

object InsertBefore {

  class InsertIterator(private var base: SequenceIterator,
                       private var insert: SequenceIterator,
                       insertPosition: Int)
    extends SequenceIterator {

    private var insertPos: Int = Math.max(insertPosition, 1)

    private var position: Int = 0

    private var inserting: Boolean = insertPos == 1

    def next(): Item = {
      var nextItem: Item = null
      if (inserting) {
        nextItem = insert.next()
        if (nextItem == null) {
          inserting = false
          nextItem = base.next()
        }
      } else {
        if (position == insertPos - 1) {
          nextItem = insert.next()
          if (nextItem == null) {
            nextItem = base.next()
          } else {
            inserting = true
          }
        } else {
          nextItem = base.next()
          if (nextItem == null && position < insertPos - 1) {
            inserting = true
            nextItem = insert.next()
          }
        }
      }
      if (nextItem == null) {
        position = -1
        null
      } else {
        position += 1
        nextItem
      }
    }

    override def close(): Unit = {
      base.close()
      insert.close()
    }

  }

}

class InsertBefore extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val n: NumericValue = arguments(1).head.asInstanceOf[NumericValue]
    val pos: Int = n.longValue().toInt
    SequenceTool.toLazySequence(
      new InsertIterator(arguments(0).iterate(), arguments(2).iterate(), pos))
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def getItemType: ItemType =
        Type.getCommonSuperType(getArg(0).getItemType, getArg(2).getItemType)
    }

  override def getStreamerName: String = "InsertBefore"

}
