package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List


class WindowClausePull(private var source: TuplePull,
                       private var windowClause: WindowClause,
                       private var context: XPathContext)
  extends TuplePull {

  private var baseIterator: SequenceIterator = _

  private var finished: Boolean = false

  private var previous: Item = null

  private var current: Item = null

  private var next: Item = null

  private var position: Int = -1

  private var currentWindows: List[WindowClause.Window] = new ArrayList()

  override def nextTuple(context: XPathContext): Boolean = {
    var deliver: Boolean = false
    val pending: Boolean = lookForEarliest()
    if (pending) {
      return true
    }
    if (finished || baseIterator == null) {
      if (source.nextTuple(context)) {
        baseIterator = windowClause.getSequence.iterate(context)
        finished = false
        previous = null
        position = -1
        current = null
        next = null
      }
    }
    while (!finished) {
      val autoclose: Boolean = windowClause.isTumblingWindow && windowClause.getEndCondition == null
      deliver = false
      val oldPrevious: Item = previous
      previous = current
      current = next
      next = baseIterator.next()
      if (next == null) {
        finished = true
      }
      { position += 1;}
      if (position > 0) {
        if ((windowClause.isSlidingWindow || currentWindows.isEmpty ||
          autoclose) &&
          windowClause.matchesStart(previous,
            current,
            next,
            position,
            context)) {
          if (autoclose && !currentWindows.isEmpty) {
            val w: WindowClause.Window = currentWindows.get(0)
            w.endItem = previous
            w.endPreviousItem = oldPrevious
            w.endNextItem = current
            w.endPosition = position - 1
            deliver = despatch(w, context)
            currentWindows.clear()
          }
          val window: WindowClause.Window = new WindowClause.Window()
          window.startPosition = position
          window.startItem = current
          window.startPreviousItem = previous
          window.startNextItem = next
          window.contents = new ArrayList()
          currentWindows.add(window)
        }
        for (active <- currentWindows.asScala if !active.isFinished) {
          active.contents.add(current)
        }
        val explicitEndCondition: Boolean = windowClause.getEndCondition != null
        val implicitEndCondition: Boolean = finished && windowClause.isIncludeUnclosedWindows
        if (explicitEndCondition || implicitEndCondition) {
          val removals: List[WindowClause.Window] =
            new ArrayList[WindowClause.Window]()
          for (w <- currentWindows.asScala if !w.isFinished &&
            (implicitEndCondition ||
              windowClause.matchesEnd(w,
                previous,
                current,
                next,
                position,
                context))) {
            w.endItem = current
            w.endPreviousItem = previous
            w.endNextItem = next
            w.endPosition = position
            if (!deliver) {
              deliver = despatch(w, context)
              if (w.isDespatched) {
                removals.add(w)
              }
            }
          }
          for (w <- removals.asScala) {
            currentWindows.remove(w)
          }
        }
        if (deliver) {
          true
        }
      }
    }
    false
  }

  private def despatch(w: WindowClause.Window,
                       context: XPathContext): Boolean = {
    windowClause.checkWindowContents(w)
    lookForEarliest()
  }

  private def lookForEarliest(): Boolean = {
    var earliestStart: Int = java.lang.Integer.MAX_VALUE
    var earliestWindow: WindowClause.Window = null
    for (u <- currentWindows.asScala
         if u.startPosition < earliestStart && !u.isDespatched) {
      earliestStart = u.startPosition
      earliestWindow = u
    }
    if (earliestWindow == null || !earliestWindow.isFinished) {
      false
    } else {
      processWindow(earliestWindow, context)
      true
    }
  }

  private def processWindow(w: WindowClause.Window,
                            context: XPathContext): Unit = {
    val clause: WindowClause = windowClause
    var binding: LocalVariableBinding = null
    binding = clause.getVariableBinding(WindowClause.WINDOW_VAR)
    context.setLocalVariable(binding.getLocalSlotNumber,
      SequenceExtent.makeSequenceExtent(w.contents))
    binding = clause.getVariableBinding(WindowClause.START_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.startItem))
    }
    binding = clause.getVariableBinding(WindowClause.START_ITEM_POSITION)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        new Int64Value(w.startPosition))
    }
    binding = clause.getVariableBinding(WindowClause.START_NEXT_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.startNextItem))
    }
    binding = clause.getVariableBinding(WindowClause.START_PREVIOUS_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.startPreviousItem))
    }
    binding = clause.getVariableBinding(WindowClause.END_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.endItem))
    }
    binding = clause.getVariableBinding(WindowClause.END_ITEM_POSITION)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        new Int64Value(w.endPosition))
    }
    binding = clause.getVariableBinding(WindowClause.END_NEXT_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.endNextItem))
    }
    binding = clause.getVariableBinding(WindowClause.END_PREVIOUS_ITEM)
    if (binding != null) {
      context.setLocalVariable(binding.getLocalSlotNumber,
        WindowClause.makeValue(w.endPreviousItem))
    }
    w.isDespatched = true
  }

  override def close(): Unit = {
    baseIterator.close()
    source.close()
  }

}
