////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.Evaluator

import scala.beans.{BeanProperty, BooleanBeanProperty}




object EvaluationMode extends Enumeration {

  val UNDECIDED: EvaluationMode =
    new EvaluationMode(-1, Evaluator.EAGER_SEQUENCE)

  val EVALUATE_LITERAL: EvaluationMode =
    new EvaluationMode(0, Evaluator.LITERAL)

  val EVALUATE_VARIABLE: EvaluationMode =
    new EvaluationMode(1, Evaluator.VARIABLE)

  val MAKE_CLOSURE: EvaluationMode =
    new EvaluationMode(3, Evaluator.LAZY_SEQUENCE)

  val MAKE_MEMO_CLOSURE: EvaluationMode =
    new EvaluationMode(4, Evaluator.MEMO_CLOSURE)

  val RETURN_EMPTY_SEQUENCE: EvaluationMode =
    new EvaluationMode(5, Evaluator.EMPTY_SEQUENCE)

  val EVALUATE_AND_MATERIALIZE_VARIABLE: EvaluationMode =
    new EvaluationMode(6, Evaluator.VARIABLE)

  val CALL_EVALUATE_OPTIONAL_ITEM: EvaluationMode =
    new EvaluationMode(7, Evaluator.OPTIONAL_ITEM)

  val ITERATE_AND_MATERIALIZE: EvaluationMode =
    new EvaluationMode(8, Evaluator.EAGER_SEQUENCE)

  val PROCESS: EvaluationMode = new EvaluationMode(9, Evaluator.PROCESS)

  val LAZY_TAIL_EXPRESSION: EvaluationMode =
    new EvaluationMode(10, Evaluator.LAZY_TAIL)

  val SHARED_APPEND_EXPRESSION: EvaluationMode =
    new EvaluationMode(11, Evaluator.SHARED_APPEND)

  val MAKE_INDEXED_VARIABLE: EvaluationMode =
    new EvaluationMode(12, Evaluator.MAKE_INDEXED_VARIABLE)

  val MAKE_SINGLETON_CLOSURE: EvaluationMode =
    new EvaluationMode(13, Evaluator.SINGLETON_CLOSURE)

  val EVALUATE_SUPPLIED_PARAMETER: EvaluationMode =
    new EvaluationMode(14, Evaluator.SUPPLIED_PARAMETER)

  val STREAMING_ARGUMENT: EvaluationMode =
    new EvaluationMode(15, Evaluator.STREAMING_ARGUMENT)

  val CALL_EVALUATE_SINGLE_ITEM: EvaluationMode =
    new EvaluationMode(16, Evaluator.SINGLE_ITEM)

  class EvaluationMode(@BeanProperty val code: Int,
                       @BeanProperty val evaluator: Evaluator)
      extends Val

  def forCode(code: Int): EvaluationMode =
    EvaluationMode.values
      .find(_.getCode == code)
      .getOrElse(ITERATE_AND_MATERIALIZE)

  implicit def convertValue(v: Value): EvaluationMode =
    v.asInstanceOf[EvaluationMode]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An evaluation mode represents a way in which expressions can be evaluated
  */
