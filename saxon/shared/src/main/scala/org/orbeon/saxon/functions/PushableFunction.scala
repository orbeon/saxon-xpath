////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.Sequence


/**
  * Interface implemented by functions that have a "push" implementation, whereby the result
  * of the function is written incrementally to an `Outputter` rather than being
  * returned as the result of a `call()` method.
  */
trait PushableFunction {

  def process(destination: Outputter,
              context: XPathContext,
              arguments: Array[Sequence]): Unit
}

