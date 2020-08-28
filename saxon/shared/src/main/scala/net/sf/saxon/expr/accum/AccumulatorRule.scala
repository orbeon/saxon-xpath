////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.accum

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import net.sf.saxon.trans.rules.RuleTarget

import scala.beans.{BeanProperty, BooleanBeanProperty}




class AccumulatorRule(@BeanProperty var newValueExpression: Expression,
                      @BeanProperty var stackFrameMap: SlotManager,
                      @BooleanBeanProperty var postDescent: Boolean)
    extends RuleTarget {

  @BooleanBeanProperty
  var capturing: Boolean = _

  def export(out: ExpressionPresenter): Unit = {
    newValueExpression.export(out)
  }

  /**
    * Register a rule for which this is the target
    *
    * @param rule a rule in which this is the target
    */
  def registerRule(rule: Rule): Unit = ()
// no action
// no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents one of the rules making up the definition of an accumulator
  */
