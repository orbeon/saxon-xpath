////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trace

import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.Logger
import org.orbeon.saxon.lib.TraceListener
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.trans.SimpleMode
import java.util

/**
 * A class which implements efficient and thread-safe multi-cast event
 * dispatching for the TraceListener evants.
 */
object TraceEventMulticaster {
  /**
   * Adds trace-listener-a with trace-listener-b and
   * returns the resulting multicast listener.
   *
   * @param a trace-listener-a
   * @param b trace-listener-b
   */
    def add(a: TraceListener, b: TraceListener) = addInternal(a, b)

  /**
   * Removes the old trace-listener from trace-listener-l and
   * returns the resulting multicast listener.
   *
   * @param l    trace-listener-l
   * @param oldl the trace-listener being removed
   */
  def remove(l: TraceListener, oldl: TraceListener) = removeInternal(l, oldl)

  /**
   * Returns the resulting multicast listener from adding listener-a
   * and listener-b together.
   * If listener-a is null, it returns listener-b;
   * If listener-b is null, it returns listener-a
   * If neither are null, then it creates and returns
   * a new EventMulticaster instance which chains a with b.
   *
   * @param a event listener-a
   * @param b event listener-b
   */
   def addInternal(a: TraceListener, b: TraceListener): TraceListener = {
    if (a == null) return b
    if (b == null) return a
    new TraceEventMulticaster(a, b)
  }

  /**
   * Returns the resulting multicast listener after removing the
   * old listener from listener-l.
   * If listener-l equals the old listener OR listener-l is null,
   * returns null.
   * Else if listener-l is an instance of SaxonEventMulticaster,
   * then it removes the old listener from it.
   * Else, returns listener l.
   *
   * @param l    the listener being removed from
   * @param oldl the listener being removed
   */
   def removeInternal(l: TraceListener, oldl: TraceListener) = if ((l eq oldl) || l == null) null
  else if (l.isInstanceOf[TraceEventMulticaster]) l.asInstanceOf[TraceEventMulticaster].remove(oldl)
  else l // it's not here
}

class TraceEventMulticaster (val a: TraceListener, val b: TraceListener)

/**
 * Creates an event multicaster instance which chains listener-a
 * with listener-b.
 *
 * @param a listener-a
 * @param b listener-b
 */
  extends TraceListener {
  override def setOutputDestination(stream: Logger) = {
    a.setOutputDestination(stream)
    b.setOutputDestination(stream)
  }

  /**
   * Removes a listener from this multicaster and returns the
   * resulting multicast listener.
   *
   * @param oldl the listener to be removed
   */
  /*@Nullable*/  def remove(oldl: TraceListener): TraceListener = {
    if (oldl eq a) return b
    if (oldl eq b) return a
    val a2 = TraceEventMulticaster.removeInternal(a, oldl)
    val b2 = TraceEventMulticaster.removeInternal(b, oldl)
    if ((a2 eq a) && (b2 eq b)) return this
    TraceEventMulticaster.addInternal(a2, b2)
  }

  /**
   * Called at start
   */
  override def open(controller: Controller) = {
    a.open(controller)
    b.open(controller)
  }

  /**
   * Called at end
   */
  override def close() = {
    a.close()
    b.close()
  }

  /**
   * Called when an element of the stylesheet gets processed
   */
  override def enter(element: Traceable, properties: java.util.Map[String, Any], context: XPathContext) = {
    a.enter(element, properties, context)
    b.enter(element, properties, context)
  }

  /**
   * Called after an element of the stylesheet got processed
   *
   * @param element
   */
  override def leave(element: Traceable) = {
    a.leave(element)
    b.leave(element)
  }

  /**
   * Called when an item becomes current
   */
  override def startCurrentItem(item: Item) = {
    a.startCurrentItem(item)
    b.startCurrentItem(item)
  }

  /**
   * Called when an item ceases to be the current item
   */
  override def endCurrentItem(item: Item) = {
    a.endCurrentItem(item)
    b.endCurrentItem(item)
  }

  /**
   * Called at the start of a rule search
   */
  override def startRuleSearch() = {
    a.startRuleSearch()
    b.startRuleSearch()
  }

  /**
   * Called at the end of a rule search
   *
   * @param rule the rule (or possible built-in ruleset) that has been selected
   * @param mode
   * @param item
   */
  def endRuleSearch(rule: AnyRef, mode: SimpleMode, item: Item) = {
    a.endRuleSearch(rule, mode, item)
    b.endRuleSearch(rule, mode, item)
  }
}