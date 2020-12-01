////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trace

import org.orbeon.saxon.expr.instruct.ComponentTracer


/**
  * A code injector that wraps the body of a template or function in a TraceExpression, which causes
  * the TimingTraceListener to be notified at the start and end of the function/template evaluation
  */
class TimingCodeInjector extends TraceCodeInjector {

  override def process(component: TraceableComponent): Unit = {
    val trace = new ComponentTracer(component)
    component.setBody(trace)
  }
}

