////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.Whitespace




class Tokenize_1 extends SystemFunction {

  /**
    * Evaluate the expression dynamically
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val sv: AtomicValue = arguments(0).head().asInstanceOf[AtomicValue]
    if (sv == null) {
      EmptySequence.getInstance
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
