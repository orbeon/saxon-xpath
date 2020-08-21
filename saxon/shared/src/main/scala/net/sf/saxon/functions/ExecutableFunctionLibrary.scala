////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.expr.UserFunctionCall

import net.sf.saxon.expr.instruct.UserFunction

import net.sf.saxon.om.Function

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import java.util.HashMap

import java.util.Iterator

import java.util.List


class ExecutableFunctionLibrary(@transient private var config: Configuration)
  extends FunctionLibrary {

  private var functions: HashMap[SymbolicName, UserFunction] = new HashMap(20)

  def addFunction(fn: UserFunction): Unit = {
    functions.put(fn.getSymbolicName, fn)
  }

  def bind(functionName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val fn: UserFunction = functions.get(functionName)
    if (fn == null) {
      return null
    }
    val fc: UserFunctionCall = new UserFunctionCall()
    fc.setFunctionName(functionName.getComponentName)
    fc.setArguments(staticArgs)
    fc.setFunction(fn)
    fc.setStaticType(fn.getResultType)
    fc
  }

  /**
   * Test whether a function with a given name and arity is available; if so, return a function
   * item that can be dynamically called.
   * <p>This supports the function-lookup() function in XPath 3.0.</p>
   *
   * @param functionName  the qualified name of the function being called
   * @param staticContext the static context to be used by the function, in the event that
   *                      it is a system function with dependencies on the static context
   * @return if a function of this name and arity is available for calling, then a corresponding
   *         function item; or null if the function does not exist
   * @throws net.sf.saxon.trans.XPathException
   * in the event of certain errors, for example attempting to get a function
   * that is private
   */
  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    val fn: UserFunction = functions.get(functionName)
    if (fn != null && fn.isUpdating) {
      throw new XPathException(
        "Cannot bind a function item to an updating function")
    }
    fn
  }

  def isAvailable(functionName: SymbolicName.F): Boolean =
    functions.get(functionName) != null

  def copy(): FunctionLibrary = {
    val efl: ExecutableFunctionLibrary = new ExecutableFunctionLibrary(config)
    efl.functions = new HashMap(functions)
    efl
  }

  def iterateFunctions(): Iterator[UserFunction] = functions.values.iterator()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An ExecutableFunctionLibrary is a function library that contains definitions of functions for use at
 * run-time. Normally functions are bound at compile-time; however there are various situations in which
 * the information is needed dynamically, for example (a) to support the XSLT function-available() call
 * (in the pathological case where the argument is not known statically), (b) to allow functions to be
 * called from saxon:evaluate(), (c) to allow functions to be called from a debugging breakpoint.
 */
