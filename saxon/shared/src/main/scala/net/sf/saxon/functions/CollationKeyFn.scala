////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Base64BinaryValue

import CollationKeyFn._




object CollationKeyFn {

  private def getCollationKey(s: String,
                              collator: StringCollator): Base64BinaryValue = {
    val `val`: AtomicValue = collator.getCollationKey(s).asAtomic()
    if (`val`.isInstanceOf[Base64BinaryValue]) {
      `val`.asInstanceOf[Base64BinaryValue]
    } else {
      throw new IllegalStateException("Collation key must be Base64Binary")
    }
  }

}

/**
  * Implements the collation-key function defined in the XSLT 3.0 and XPath 3.1 specifications
  */
class CollationKeyFn extends CollatingFunctionFixed {

  /**
    * Call the Callable.
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences.
    *                  <p>Generally it is advisable, if calling iterate() to process a supplied sequence, to
    *                  call it only once; if the value is required more than once, it should first be converted
    *                  to a {@link net.sf.saxon.om.GroundedValue} by calling the utility methd
    *                  SequenceTool.toGroundedValue().</p>
    *                  <p>If the expected value is a single item, the item should be obtained by calling
    *                  Sequence.head: it cannot be assumed that the item will be passed as an instance of
    *                  {@link net.sf.saxon.om.Item} or {@link net.sf.saxon.value.AtomicValue}.</p>
    *                  <p>It is the caller's responsibility to perform any type conversions required
    *                  to convert arguments to the type expected by the callee. An exception is where
    *                  this Callable is explicitly an argument-converting wrapper around the original
    *                  Callable.</p>
    * @return the result of the evaluation, in the form of a Sequence. It is the responsibility
    *         of the callee to ensure that the type of result conforms to the expected result type.
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext,
           arguments: Array[Sequence]): Base64BinaryValue = {
    val in: String = arguments(0).head.getStringValue
    val collator: StringCollator = getStringCollator
    getCollationKey(in, collator)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
