////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.xpath

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.functions.CallableFunction

import net.sf.saxon.functions.FunctionLibrary

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om.Function

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

import javax.xml.namespace.QName

import javax.xml.xpath.XPathFunction

import javax.xml.xpath.XPathFunctionResolver

import java.util.Arrays

import java.util.List




class XPathFunctionLibrary extends FunctionLibrary {

  private var resolver: XPathFunctionResolver = _

  def setXPathFunctionResolver(resolver: XPathFunctionResolver): Unit = {
    this.resolver = resolver
  }

  def getXPathFunctionResolver: XPathFunctionResolver = resolver

  /*@Nullable*/

  def bind(functionName: SymbolicName.F,
           staticArgs: Array[Expression],
           env: StaticContext,
           reasons: List[String]): Expression = {
    if (resolver == null) {
      return null
    }
    val qn: StructuredQName = functionName.getComponentName
    val name: QName = new QName(qn.getURI, qn.getLocalPart)
    val function: XPathFunction =
      resolver.resolveFunction(name, functionName.getArity)
    if (function == null) {
      return null
    }
    val fc: XPathFunctionCall = new XPathFunctionCall(qn, function)
    fc.setArguments(staticArgs)
    fc
  }

  /**
   * Test whether a function with a given name and arity is available; if so, return a function
   * item that can be dynamically called.
   * <p>This supports the function-lookup() function in XPath 3.0.</p>
   *
   * @param symbolicName  the qualified name of the function being called
   * @param staticContext the static context to be used by the function, in the event that
   *                      it is a system function with dependencies on the static context
   * @return if a function of this name and arity is available for calling, then a corresponding
   *         function item; or null if the function does not exist
   */
  def getFunctionItem(symbolicName: SymbolicName.F,
                      staticContext: StaticContext): Function = {
    if (resolver == null) {
      return null
    }
    val functionName: StructuredQName = symbolicName.getComponentName
    val arity: Int = symbolicName.getArity
    val name: QName = new QName(functionName.getURI, functionName.getLocalPart)
    val function: XPathFunction = resolver.resolveFunction(name, arity)
    if (function == null) {
      return null
    }
    val functionCall: XPathFunctionCall =new XPathFunctionCall(functionName, function)
    val argTypes: Array[SequenceType] = Array.fill[SequenceType](arity)(SequenceType.ANY_SEQUENCE)
    val functionType: FunctionItemType =
      new SpecificFunctionType(argTypes, SequenceType.ANY_SEQUENCE)
    new CallableFunction(symbolicName, functionCall, functionType)
  }

  /**
   * Test whether a function with a given name and arity is available
   * <p>This supports the function-available() function in XSLT.</p>
   *
   * @param functionName the qualified name of the function being called
   * @return true if a function of this name and arity is available for calling
   */
  def isAvailable(functionName: SymbolicName.F): Boolean =
    resolver != null &&
      resolver.resolveFunction(
        new QName(functionName.getComponentName.getURI,
          functionName.getComponentName.getLocalPart),
        functionName.getArity) !=
        null

  /*@NotNull*/

  def copy(): FunctionLibrary = {
    val xfl: XPathFunctionLibrary = new XPathFunctionLibrary()
    xfl.resolver = resolver
    xfl
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * The XPathFunctionLibrary is a FunctionLibrary that supports binding of XPath function
 * calls to instances of the JAXP XPathFunction interface returned by an XPathFunctionResolver.
 */
