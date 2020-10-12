////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.query

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.instruct.UserFunction

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.functions.FunctionLibrary

import org.orbeon.saxon.functions.hof.UnresolvedXQueryFunctionItem

import org.orbeon.saxon.functions.hof.UserFunctionReference

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.SymbolicName

import org.orbeon.saxon.trans.XPathException

import java.util.HashMap

import java.util.Iterator

import java.util.List

import XQueryFunctionLibrary._




object XQueryFunctionLibrary {

  class UnresolvedCallable(var symbolicName: SymbolicName.F)
      extends UserFunctionResolvable
      with Callable {

    var function: UserFunction = _

    def getFunctionName: StructuredQName = symbolicName.getComponentName

    def getArity: Int = symbolicName.getArity

    /**
      * Evaluate the expression
      *
      * @param context   the dynamic evaluation context
      * @param arguments the values of the arguments, supplied as Sequences
      * @return the result of the evaluation, in the form of a Sequence
      * @throws XPathException
      *          if a dynamic error occurs during the evaluation of the expression
      */
    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      if (function == null) {
        throw new XPathException(
          "Forwards reference to XQuery function has not been resolved")
      }
      val args: Array[Sequence] = Array.ofDim[Sequence](arguments.length)
      for (i <- 0 until arguments.length) {
        args(i) =
          arguments(i).iterate().materialize
      }
      function.call(context.newCleanContext(), args)
    }

    def setFunction(function: UserFunction): Unit = {
      this.function = function
    }

    def getFunction: UserFunction = function

  }

}

class XQueryFunctionLibrary(private var config: Configuration)
    extends FunctionLibrary
    with XQueryFunctionBinder {

  /*@NotNull*/

  private var functions: HashMap[SymbolicName, XQueryFunction] = new HashMap(
    20)

  override def setConfiguration(config: Configuration): Unit = {
    this.config = config
  }

  def getConfiguration: Configuration = config

  def declareFunction(function: XQueryFunction): Unit = {
    val keyObj: SymbolicName = function.getIdentificationKey
    val existing: XQueryFunction = functions.get(keyObj)
    if (existing == function) {
      return
    }
    if (existing != null) {
      val err = new XPathException(
        "Duplicate definition of function " + function.getDisplayName +
          " (see line " +
          existing.getLineNumber +
          " in " +
          existing.getSystemId +
          ')')
      err.setErrorCode("XQST0034")
      err.setIsStaticError(true)
      err.setLocator(function)
      throw err
    }
    functions.put(keyObj, function)
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
   */
  def getFunctionItem(functionName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    val fd: XQueryFunction = functions.get(functionName)
    if (fd != null) {
      if (fd.isPrivate && fd.getSystemId != staticContext.getStaticBaseURI) {
        throw new XPathException(
          "Cannot call the private function " + functionName.getComponentName.getDisplayName +
            " from outside its module",
          "XPST0017")
      }
      val fn: UserFunction = fd.getUserFunction
      val `type`: FunctionItemType = new SpecificFunctionType(
        fd.getArgumentTypes,
        fd.getResultType,
        fd.getAnnotations)
      if (fn == null) {
        // not yet compiled: create a dummy
        val uf: UserFunction = new UserFunction()
        uf.setFunctionName(functionName.getComponentName)
        uf.setResultType(fd.getResultType)
        uf.setParameterDefinitions(fd.getParameterDefinitions)
        val ref: UserFunctionReference = new UserFunctionReference(uf)
        fd.registerReference(ref)
        // Dependent Error due to UnresolvedXQueryFunctionItem. Will be resolved with correct import.
        new UnresolvedXQueryFunctionItem(fd, functionName, ref)
      } else {
        fn
      }
    } else {
      null
    }
  }

  /**
   * Test whether a function with a given name and arity is available
   * <p>This supports the function-available() function in XSLT.</p>
   *
   * @param functionName the qualified name of the function being called
   * @return true if a function of this name and arity is available for calling
   */
  def isAvailable(functionName: SymbolicName.F): Boolean =
    functions.get(functionName) != null

  /*@Nullable*/

  def bind(functionName: SymbolicName.F,
           arguments: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    val fd: XQueryFunction = functions.get(functionName)
    if (fd != null) {
      if (fd.isPrivate && fd.getStaticContext != env) {
        reasons.add(
          "Cannot call the private XQuery function " + functionName.getComponentName.getDisplayName +
            " from outside its module")
        return null
      }
      val ufc: UserFunctionCall = new UserFunctionCall()
      ufc.setFunctionName(fd.getFunctionName)
      ufc.setArguments(arguments)
      ufc.setStaticType(fd.getResultType)
      val fn: UserFunction = fd.getUserFunction
      if (fn == null) {
        // not yet compiled
        fd.registerReference(ufc)
      } else {
        ufc.setFunction(fn)
      }
      ufc
    } else {
      null
    }
  }

  def getDeclaration(functionName: StructuredQName,
                     staticArgs: Int): XQueryFunction = {
    val functionKey: SymbolicName =
      XQueryFunction.getIdentificationKey(functionName, staticArgs)
    functions.get(functionKey)
  }

  def getDeclarationByKey(functionKey: SymbolicName): XQueryFunction =
    functions.get(functionKey)

  def getFunctionDefinitions: Iterator[XQueryFunction] =
    functions.values.iterator

  def fixupGlobalFunctions(env: QueryModule): Unit = {
    val visitor: ExpressionVisitor = ExpressionVisitor.make(env)
    functions.values.forEach { fn =>
      fn.compile()
    }
    functions.values.forEach { fn =>
      fn.checkReferences(visitor)
    }
  }

  def optimizeGlobalFunctions(topModule: QueryModule): Unit = {
    functions.values.forEach { fn =>
      if (fn.getStaticContext.getTopLevelModule == topModule) {
        fn.optimize()
      }
    }

    def explainGlobalFunctions(out: ExpressionPresenter): Unit = {
      functions.values.forEach { fn =>
        fn.explain(out)
      }
    }

    /*@Nullable*/

    def getUserDefinedFunction(uri: String,
                               localName: String,
                               arity: Int): UserFunction = {
      val functionKey: SymbolicName =
        new SymbolicName.F(new StructuredQName("", uri, localName), arity)
      val fd: XQueryFunction = functions.get(functionKey)
      if (fd == null) {
        return null
      }
      fd.getUserFunction
    }

  }

  /*@NotNull*/

  override def copy(): FunctionLibrary = {
    val qfl: XQueryFunctionLibrary = new XQueryFunctionLibrary(config)
    qfl.functions = new HashMap(functions)
    qfl
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An XQueryFunctionLibrary is a function library containing all the user-defined functions available for use within a
  * particular XQuery module: that is, the functions declared in that module, and the functions imported from other
  * modules. It also contains (transiently during compilation) a list of function calls within the module that have not
  * yet been bound to a specific function declaration.
  */
