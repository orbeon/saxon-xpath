////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trace

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.ContextOriginator

import net.sf.saxon.expr.UserFunctionCall

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.ApplyTemplates

import net.sf.saxon.expr.instruct.CallTemplate

import net.sf.saxon.expr.instruct.GlobalVariable

import net.sf.saxon.expr.instruct.UserFunction

import net.sf.saxon.trans.rules.BuiltInRuleSet

import java.util.Iterator

import ContextStackIterator._




object ContextStackIterator {

  private def getMajorCaller(context: XPathContext): XPathContextMajor = {
    var caller: XPathContext = context.getCaller
    while (!(caller == null || caller
             .isInstanceOf[XPathContextMajor])) caller = caller.getCaller
    caller.asInstanceOf[XPathContextMajor]
  }

}

/**
  * This class provides a representation of the current runtime call stack, as represented by the stack
  * of XPathContext objects.
  */
class ContextStackIterator(var context: XPathContext) extends Iterator[ContextStackFrame] {

  private var nextVar : XPathContextMajor = context.asInstanceOf[XPathContextMajor]

  if (!(context.isInstanceOf[XPathContextMajor])) {
    context = getMajorCaller(context)
  }

  /**
    * Returns <tt>true</tt> if the iteration has more elements. (In other
    * words, returns <tt>true</tt> if <tt>next</tt> would return an element
    * rather than throwing an exception.)
    *
    * @return <tt>true</tt> if the iterator has more elements.
    */
  def hasNext: Boolean = next != null

  /*@Nullable*/

  def next(): ContextStackFrame = {
    val context: XPathContextMajor = nextVar
    if (context == null) {
      return null
    }
    val origin: ContextOriginator = context.getOrigin
    if (origin.isInstanceOf[Controller]) {
      nextVar = getMajorCaller(context)
      new ContextStackFrame.CallingApplication()
    } else if (origin.isInstanceOf[BuiltInRuleSet]) {
      nextVar = getMajorCaller(context)
      new ContextStackFrame.BuiltInTemplateRule(context)
    } else if (origin.isInstanceOf[UserFunction]) {
      val sf: ContextStackFrame.FunctionCall =
        new ContextStackFrame.FunctionCall()
      val ufc: UserFunction = origin.asInstanceOf[UserFunction]
      sf.setLocation(ufc.getLocation)
      sf.setFunctionName(ufc.getFunctionName)
      sf.setContextItem(context.getContextItem)
      sf.setContext(context)
      nextVar = getMajorCaller(context)
      sf
    } else if (origin.isInstanceOf[UserFunctionCall]) {
// No longer used? Bug 3671
      val sf: ContextStackFrame.FunctionCall =
        new ContextStackFrame.FunctionCall()
      val ufc: UserFunctionCall = origin.asInstanceOf[UserFunctionCall]
      sf.setLocation(ufc.getLocation)
      sf.setFunctionName(ufc.getFunctionName)
      sf.setContextItem(context.getContextItem)
      sf.setContext(context)
      nextVar = getMajorCaller(context)
      sf
    } else if (origin.isInstanceOf[ApplyTemplates]) {
      val sf: ContextStackFrame.ApplyTemplates =
        new ContextStackFrame.ApplyTemplates()
      val loc: ApplyTemplates = origin.asInstanceOf[ApplyTemplates]
      sf.setLocation(loc.getLocation)
      sf.setContextItem(context.getContextItem)
      sf.setContext(context)
      nextVar = getMajorCaller(context)
      sf
    } else if (origin.isInstanceOf[CallTemplate]) {
      val sf: ContextStackFrame.CallTemplate =
        new ContextStackFrame.CallTemplate()
      val loc: CallTemplate = origin.asInstanceOf[CallTemplate]
      sf.setLocation(loc.getLocation)
      sf.setTemplateName(loc.getObjectName)
      sf.setContextItem(context.getContextItem)
      sf.setContext(context)
      nextVar = getMajorCaller(context)
      sf
    } else if (origin.isInstanceOf[GlobalVariable]) {
      val sf: ContextStackFrame.VariableEvaluation =
        new ContextStackFrame.VariableEvaluation()
      val `var`: GlobalVariable = origin.asInstanceOf[GlobalVariable]
      sf.setLocation(`var`.getLocation)
      sf.setContextItem(context.getContextItem)
      sf.setVariableName(`var`.getVariableQName)
      sf.setComponent(`var`)
      sf.setContext(context)
      nextVar = getMajorCaller(context)
      sf
    } else {
//out.println("    In unidentified location " + construct);
      nextVar = getMajorCaller(context)
      val csf: ContextStackFrame = next()
      if (csf == null) {
// we can't return null, because hasNext returned true...
        new ContextStackFrame.CallingApplication()
      } else {
        csf
      }
    }
//other context changes are not considered significant enough to report
//other context changes are not considered significant enough to report
  }

  /**
    * Removes from the underlying collection the last element returned by the
    * iterator (optional operation).
    *
    * @throws UnsupportedOperationException as the <tt>remove</tt>
    *                                       operation is not supported by this Iterator.
    */
  override def remove(): Unit = {
    throw new UnsupportedOperationException()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
