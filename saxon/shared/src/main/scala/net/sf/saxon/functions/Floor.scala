////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.NumericValue




class Floor extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    arg.asInstanceOf[NumericValue].floor()

  override def getCompilerName(): String = "RoundingCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the fn:floor() function
  */
