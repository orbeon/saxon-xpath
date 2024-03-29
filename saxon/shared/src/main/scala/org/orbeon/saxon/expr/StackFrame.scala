package org.orbeon.saxon.expr

import java.util.Arrays

import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.om.Sequence

object StackFrame {
  val EMPTY: StackFrame = new StackFrame(SlotManager.EMPTY, new Array[Sequence](0))
}

class StackFrame(var map: SlotManager, var slots: Array[Sequence]) {

  private var dynamicStack: List[Sequence] = Nil

  def getStackFrameMap: SlotManager = map

  def getStackFrameValues: Array[Sequence] = slots

  def setStackFrameValues(values: Array[Sequence]): Unit =
    slots = values

  def copy(): StackFrame = {
    val v2 = Arrays.copyOf(slots, slots.length)
    val s = new StackFrame(map, v2)
    s.dynamicStack = dynamicStack
    s
  }

  def pushDynamicValue(value: Sequence): Unit = {
    if (this eq StackFrame.EMPTY)
      throw new IllegalStateException("Immutable stack frame")
    dynamicStack ::= value
  }

  def popDynamicValue(): Sequence = {
    val s = dynamicStack.head
    dynamicStack = dynamicStack.tail
    s
  }

  def holdsDynamicValue(): Boolean =
    dynamicStack.nonEmpty
}
