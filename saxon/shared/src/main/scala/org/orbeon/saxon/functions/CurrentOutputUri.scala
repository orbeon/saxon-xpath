////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AnyURIValue




class CurrentOutputUri extends SystemFunction {

  /**
    * Determine the special properties of this function. The general rule
    * is that a system function call is non-creative if its return type is
    * atomic, or if all its arguments are non-creative. This is overridden
    * for the generate-id() function, which is considered creative if
    * its operand is creative (because the result depends on the
    * identity of the operand)
    * @param arguments the actual arguments to the function call
    */
  override def getSpecialProperties(arguments: Array[Expression]): Int = // Prevent inlining of stylesheet functions calling current-output-uri()
    super.getSpecialProperties(arguments) | StaticProperty.HAS_SIDE_EFFECTS

  def evaluateItem(context: XPathContext): AnyURIValue = {
    val uri: String = context.getCurrentOutputUri
    if (uri == null) null else new AnyURIValue(uri)
  }

  /**
    * Call the Callable.
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences.
    *                  <p>Generally it is advisable, if calling iterate() to process a supplied sequence, to
    *                  call it only once; if the value is required more than once, it should first be converted
    *                  to a {@link org.orbeon.saxon.om.GroundedValue} by calling the utility methd
    *                  SequenceTool.toGroundedValue().</p>
    *                  <p>If the expected value is a single item, the item should be obtained by calling
    *                  Sequence.head: it cannot be assumed that the item will be passed as an instance of
    *                  {@link org.orbeon.saxon.om.Item} or {@link org.orbeon.saxon.value.AtomicValue}.</p>
    *                  <p>It is the caller's responsibility to perform any type conversions required
    *                  to convert arguments to the type expected by the callee. An exception is where
    *                  this Callable is explicitly an argument-converting wrapper around the original
    *                  Callable.</p>
    * @return the result of the evaluation, in the form of a Sequence. It is the responsibility
    *         of the callee to ensure that the type of result conforms to the expected result type.
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext,
           arguments: Array[Sequence]): ZeroOrOne[AnyURIValue] =
    new ZeroOrOne(evaluateItem(context))

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the XSLT 3.0 function current-output-uri()
  */
