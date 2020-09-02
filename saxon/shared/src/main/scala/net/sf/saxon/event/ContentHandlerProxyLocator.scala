////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import org.xml.sax.Locator
import java.util.Stack

import net.sf.saxon.om.Item




class ContentHandlerProxyLocator(private var parent: ContentHandlerProxy)
    extends Locator {

  def getPublicId: String = null

  def getSystemId: String = parent.getCurrentLocation.getSystemId

  def getLineNumber: Int = parent.getCurrentLocation.getLineNumber

  def getColumnNumber(): Int = parent.getCurrentLocation.getColumnNumber

  /*@Nullable*/

  def getContextItemStack: Stack[Item] = {
    val traceListener: ContentHandlerProxy.ContentHandlerProxyTraceListener =
      parent.getTraceListener
    if (traceListener == null) {
      null
    } else {
      traceListener.getContextItemStack
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implementation of the SAX Locator interface, used to supply location information to the ContentHandler.
  * <p>When the ContentHandler is used to receive the results of a query or stylesheet,
  * the information supplied by the standard methods such as {@link #getSystemId} and
  * {@link #getLineNumber} relates to the position of the expression/instruction in the stylesheet
  * or query that caused the relevant nodes to be output.</p>
  * <p>If the ContentHandler is used in other contexts, for example as the destination of an
  * IdentityTransformer, the information reflects the position in the source document.</p>
  * <p>If the output property <code>saxon:supply-source-locator</code> was set to the
  * value "yes" (which in turn requires that tracing was enabled at compile time), the Locator will
  * also contain information about the current location in the source document (specifically, the position
  * of the context node). This will not always be 100% accurate, since there is some buffering of events
  * in the output pipeline: it reflects the context node at the time the event reaches the ContentHandler,
  * which may not be the same as the context node at the time the relevant instruction was executed;
  * however, it still provides some information that may be useful for diagnostics.</p>
  */
