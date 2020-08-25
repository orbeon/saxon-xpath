////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.lib.ExtensionFunctionCall

import net.sf.saxon.lib.ExtensionFunctionDefinition

import net.sf.saxon.om.Function

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import java.util.HashMap

import java.util.List

import IntegratedFunctionLibrary._


object IntegratedFunctionLibrary {

  def makeFunctionCall(defn: ExtensionFunctionDefinition,
                       staticArgs: Array[Expression]): Expression = {
    val f: ExtensionFunctionCall = defn.makeCallExpression()
    f.setDefinition(defn)
    val fc: IntegratedFunctionCall =
      new IntegratedFunctionCall(defn.getFunctionQName, f)
    fc.setArguments(staticArgs)
    fc
  }

}

/**
 * A library of integrated function calls, that is, user-written extension functions implemented
 * as instances of the class IntegratedFunction.
 */
class IntegratedFunctionLibrary extends FunctionLibrary {

  private var functions: HashMap[StructuredQName, ExtensionFunctionDefinition] =
    new HashMap()

  def registerFunction(function: ExtensionFunctionDefinition): Unit = {
    functions.put(function.getFunctionQName, function)
  }

  def bind(functionName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val defn: ExtensionFunctionDefinition =
      functions.get(functionName.getComponentName)
    if (defn == null) {
      return null
    }
    makeFunctionCall(defn, staticArgs)
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
    val defn: ExtensionFunctionDefinition =
      functions.get(functionName.getComponentName)
    if (defn == null) {
      return null
    }
    defn.asFunction()
  }

  /**
   * Test whether a function with a given name and arity is available
   * <p>This supports the function-available() function in XSLT.</p>
   *
   * @param functionName the qualified name of the function being called
   * @return true if a function of this name and arity is available for calling
   */
  def isAvailable(functionName: SymbolicName.F): Boolean = {
    val defn: ExtensionFunctionDefinition =
      functions.get(functionName.getComponentName)
    val arity: Int = functionName.getArity
    defn != null && defn.getMaximumNumberOfArguments >= arity &&
      defn.getMinimumNumberOfArguments <= arity
  }

  def copy(): FunctionLibrary = {
    val lib: IntegratedFunctionLibrary = new IntegratedFunctionLibrary()
    lib.functions = new HashMap(functions)
    lib
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
