////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.util.{EventListener, Map}

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.trace.Traceable
import org.orbeon.saxon.trans.Mode
import org.orbeon.saxon.utils.Controller


/**
  * This interface defines methods that are called by Saxon during the execution of
  * a stylesheet, if tracing is switched on. Tracing can be switched on by nominating
  * an implementation of this class using the TRACE_LISTENER feature of the TransformerFactory,
  * or using the addTraceListener() method of the Controller, which is Saxon's implementation
  * of tyhe JAXP javax.xml.transform.Transformer interface.
  */
trait TraceListener extends EventListener {
  def setOutputDestination(stream: Logger): Unit
  def open(controller: Controller): Unit
  def close(): Unit
  def enter(instruction: Traceable, properties: Map[String, Any], context: XPathContext): Unit = ()
  def leave(instruction: Traceable): Unit = ()
  def startCurrentItem(currentItem: Item): Unit
  def endCurrentItem(currentItem: Item): Unit
  def startRuleSearch(): Unit = ()
  def endRuleSearch(rule: AnyRef, mode: Mode, item: Item): Unit = ()
}
