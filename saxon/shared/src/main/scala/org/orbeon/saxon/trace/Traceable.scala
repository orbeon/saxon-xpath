////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.Locatable

import org.orbeon.saxon.om.StructuredQName

import java.util.function.BiConsumer




trait Traceable extends Locatable {

  /*@Nullable*/

  def getObjectName: StructuredQName

  def gatherProperties(consumer: BiConsumer[String, Any]): Unit = ()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A construct whose execution can be notified to a TraceListener. In practice this is either
  * an expression, or a component such as a function or template or global variable.
  */
