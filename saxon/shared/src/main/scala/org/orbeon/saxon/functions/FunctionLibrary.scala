////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A FunctionLibrary handles the binding of function calls in XPath (or XQuery) expressions.
  * There are a number of implementations of this
  * class to handle different kinds of function: system functions, constructor functions, vendor-defined
  * functions, Java extension functions, stylesheet functions, and so on. There is also an implementation
  * {@link org.orbeon.saxon.functions.FunctionLibraryList} that allows a FunctionLibrary
  * to be constructed by combining other FunctionLibrary objects.
  */
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, StaticContext}
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.trans.SymbolicName
import org.orbeon.saxon.utils.Configuration

trait FunctionLibrary {

  def setConfiguration(config: Configuration): Unit = ()

  /*@Nullable*/

  def isAvailable(functionName: SymbolicName.F): Boolean

  /*@Nullable*/

  def bind(functionName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: java.util.List[String]): Expression

  def copy(): FunctionLibrary

  /*@Nullable*/

  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function

}
