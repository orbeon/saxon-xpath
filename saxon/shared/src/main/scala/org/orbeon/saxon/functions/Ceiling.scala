////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.value.NumericValue




class Ceiling extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): NumericValue =
    arg.asInstanceOf[NumericValue].ceiling()

  override def getCompilerName(): String = "RoundingCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the ceiling() function
  */
