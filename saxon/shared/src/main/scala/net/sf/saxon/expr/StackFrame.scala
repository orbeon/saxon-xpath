package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.om.Sequence

import java.util.Arrays

import java.util.Stack

object StackFrame {
  val EMPTY: StackFrame = new StackFrame(SlotManager.EMPTY, new Array[Sequence](0))
}

class StackFrame(var map: SlotManager, var slots: Array[Sequence]) {

   var seqArray: Array[Sequence] = slots
   var dynamicStack: Stack[Sequence] = _

  def getStackFrameMap: SlotManager = map

  def getStackFrameValues: Array[Sequence] = seqArray

  def setStackFrameValues(values: Array[Sequence]): Unit = {
    seqArray = values
  }

  def copy(): StackFrame = {
    val v2 = Arrays.copyOf(seqArray, seqArray.length)
    val s = new StackFrame(map, v2)
    if (dynamicStack != null) {
      s.dynamicStack = new Stack()
      s.dynamicStack.addAll(dynamicStack)
    }
    s
  }

  def pushDynamicValue(value: Sequence): Unit = {
    if (this == StackFrame.EMPTY) {
      throw new IllegalStateException("Immutable stack frame")
    }
    if (dynamicStack == null)
      dynamicStack = new Stack()
    dynamicStack.push(value)
  }

  def popDynamicValue(): Sequence = dynamicStack.pop()

  def holdsDynamicValue(): Boolean =
    dynamicStack != null && ! dynamicStack.empty()
}
