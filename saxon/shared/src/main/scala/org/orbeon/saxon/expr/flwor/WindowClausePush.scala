package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class WindowClausePush(outputter: Outputter,
                       private var destination: TuplePush,
                       private var windowClause: WindowClause)
  extends TuplePush(outputter) {

  var currentWindows: List[WindowClause.Window] = new ArrayList()

  override def processTuple(context: XPathContext): Unit = {
    currentWindows = new ArrayList()
    val autoclose: Boolean = windowClause.isTumblingWindow && windowClause.getEndCondition == null
    var previousPrevious: Item = null
    var previous: Item = null
    var current: Item = null
    var next: Item = null
    var position: Int = -1
    val iter: SequenceIterator = windowClause.getSequence.iterate(context)
    var finished: Boolean = false
    while (!finished) {
      previousPrevious = previous
      previous = current
      current = next
      next = iter.next()
      if (next == null) {
        finished = true
      }
       position += 1
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
            w.endPreviousItem = previousPrevious
            w.endNextItem = current
            w.endPosition = position - 1
            despatch(w, getOutputter, context)
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
        if (windowClause.getEndCondition != null) {
          val removals: List[WindowClause.Window] =
            new ArrayList[WindowClause.Window]()
          for (w <- currentWindows.asScala if !w.isFinished &&
            windowClause.matchesEnd(w,
              previous,
              current,
              next,
              position,
              context)) {
            w.endItem = current
            w.endPreviousItem = previous
            w.endNextItem = next
            w.endPosition = position
            despatch(w, getOutputter, context)
            if (w.isDespatched) {
              removals.add(w)
            }
          }
          for (w <- removals.asScala) {
            currentWindows.remove(w)
          }
        }
      }
    }
    if (windowClause.isIncludeUnclosedWindows) {
      for (w <- currentWindows.asScala) {
        w.endItem = current
        w.endPreviousItem = previous
        w.endNextItem = null
        w.endPosition = position
        despatch(w, getOutputter, context)
      }
    }
  }

  private def despatch(w: WindowClause.Window,
                       output: Outputter,
                       context: XPathContext): Unit = {
    windowClause.checkWindowContents(w)
    while (true) {
      var earliestStart: Int = java.lang.Integer.MAX_VALUE
      var earliestWindow: WindowClause.Window = null
      for (u <- currentWindows.asScala
           if u.startPosition < earliestStart && !u.isDespatched) {
        earliestStart = u.startPosition
        earliestWindow = u
      }
      if (earliestWindow == null || !earliestWindow.isFinished) {
        return
      } else {
        val clause: WindowClause = windowClause
        var binding: LocalVariableBinding = null
        binding = clause.getVariableBinding(WindowClause.WINDOW_VAR)
        context.setLocalVariable(
          binding.getLocalSlotNumber,
          SequenceExtent.makeSequenceExtent(earliestWindow.contents))
        binding = clause.getVariableBinding(WindowClause.START_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.startItem))
        }
        binding = clause.getVariableBinding(WindowClause.START_ITEM_POSITION)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            new Int64Value(earliestWindow.startPosition))
        }
        binding = clause.getVariableBinding(WindowClause.START_NEXT_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.startNextItem))
        }
        binding = clause.getVariableBinding(WindowClause.START_PREVIOUS_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.startPreviousItem))
        }
        binding = clause.getVariableBinding(WindowClause.END_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.endItem))
        }
        binding = clause.getVariableBinding(WindowClause.END_ITEM_POSITION)
        if (binding != null) {
          context.setLocalVariable(binding.getLocalSlotNumber,
            new Int64Value(earliestWindow.endPosition))
        }
        binding = clause.getVariableBinding(WindowClause.END_NEXT_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.endNextItem))
        }
        binding = clause.getVariableBinding(WindowClause.END_PREVIOUS_ITEM)
        if (binding != null) {
          context.setLocalVariable(
            binding.getLocalSlotNumber,
            WindowClause.makeValue(earliestWindow.endPreviousItem))
        }
        destination.processTuple(context)
        earliestWindow.isDespatched = true
      }
    }
  }

  override def close(): Unit = {
    currentWindows = null
    destination.close()
  }

}
