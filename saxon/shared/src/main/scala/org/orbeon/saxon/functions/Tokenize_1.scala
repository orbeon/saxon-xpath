////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.EmptySequence

import org.orbeon.saxon.value.Whitespace




class Tokenize_1 extends SystemFunction {

  /**
    * Evaluate the expression dynamically
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val sv: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
    if (sv == null) {
     return EmptySequence.getInstance
    }
    val input: CharSequence = sv.getStringValueCS
    SequenceTool.toLazySequence(new Whitespace.Tokenizer(input))
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the single-argument tokenize() function introduced in XPath 3.1
  */
