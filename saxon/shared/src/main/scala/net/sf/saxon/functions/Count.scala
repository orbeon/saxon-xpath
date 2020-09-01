////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerValue

import Count._




object Count {

  def count(iter: SequenceIterator): Int =
    if (iter.getProperties.contains(
          SequenceIterator.Property.LAST_POSITION_FINDER)) {
      iter.asInstanceOf[LastPositionFinder].getLength
    } else {
      var n: Int = 0
      while (iter.next() != null) { n += 1; n - 1 }
      n
    }

  def steppingCount(iter: SequenceIterator): Int = {
    var n: Int = 0
    while (iter.next() != null) { n += 1; n - 1 }
    n
  }

}

/**
  * Implementation of the fn:count function
  */
class Count extends SystemFunction {

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
    Array(Int64Value.ZERO, Expression.MAX_SEQUENCE_LENGTH)

  /*
     Evaluate the expression

     @param context   the dynamic evaluation context
     @param arguments the values of the arguments, supplied as Sequences
     @return the result of the evaluation, in the form of a Sequence
     @throws net.sf.saxon.trans.XPathException
              if a dynamic error occurs during the evaluation of the expression*/

  def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue = {
    val arg: Sequence = arguments(0)
    val size: Int =
      if (arg.isInstanceOf[GroundedValue])
        arg.asInstanceOf[GroundedValue].getLength
      else count(arg.iterate())
    Int64Value.makeIntegerValue(size)
  }

  override def getCompilerName(): String = "CountCompiler"

  override def getStreamerName: String = "Count"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
