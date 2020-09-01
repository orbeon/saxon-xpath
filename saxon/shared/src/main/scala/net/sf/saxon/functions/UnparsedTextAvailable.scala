////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.StringValue




class UnparsedTextAvailable extends UnparsedTextFunction with Callable {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val hrefVal: StringValue = arguments(0).head.asInstanceOf[StringValue]
    if (hrefVal == null) {
      BooleanValue.FALSE
    }
    val encoding: String =
      if (getArity == 2) arguments(1).head.getStringValue else null
    BooleanValue.get(evalUnparsedTextAvailable(hrefVal, encoding, context))
  }

  def evalUnparsedTextAvailable(hrefVal: StringValue,
                                encoding: String,
                                context: XPathContext): Boolean =
    try {
      UnparsedText.evalUnparsedText(hrefVal,
                                    getStaticBaseUriString,
                                    encoding,
                                    context)
      true
    } catch {
      case err: XPathException => false

    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
