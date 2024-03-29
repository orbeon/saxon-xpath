////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.EmptySequence

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.IntegerValue

import org.orbeon.saxon.value.StringValue




class StringToCodepoints extends SystemFunction {

  /**
    * For an expression that returns an integer or a sequence of integers, get
    * a lower and upper bound on the values of the integers that may be returned, from
    * static analysis. The default implementation returns null, meaning "unknown" or
    * "not applicable". Other implementations return an array of two IntegerValue objects,
    * representing the lower and upper bounds respectively. The values
    * UNBOUNDED_LOWER and UNBOUNDED_UPPER are used by convention to indicate that
    * the value may be arbitrarily large. The values MAX_STRING_LENGTH and MAX_SEQUENCE_LENGTH
    * are used to indicate values limited by the size of a string or the size of a sequence.
    *
    * @return the lower and upper bounds of integer values in the result, or null to indicate
    *         unknown or not applicable.
    */
  override def getIntegerBounds(): Array[IntegerValue] =
    Array(Int64Value.PLUS_ONE, Int64Value.makeIntegerValue(1114111))

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val `val`: StringValue = arguments(0).head.asInstanceOf[StringValue]
    if (`val` == null) {
      return EmptySequence.getInstance
    }
    SequenceTool.toLazySequence(`val`.iterateCharacters())
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the function string-to-codepoints()
  */
