////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.trans.XPathException




object PatternMaker {

  /*@NotNull*/

  def fromExpression(expression: Expression,
                     config: Configuration,
                     is30: Boolean): Pattern = {
    val result: Pattern = expression.toPattern(config)
    ExpressionTool.copyLocationInfo(expression, result)
//result.setExecutable(expression.getExecutable());
    result
  }
//        result.setOriginalText(expression.toString());
//        result.setSystemId(expression.getSystemId);
//        result.setLineNumber(expression.getLineNumber());
//        result.setOriginalText(expression.toString());
//        result.setSystemId(expression.getSystemId);
//        result.setLineNumber(expression.getLineNumber());

  def getAxisForPathStep(step: Expression): Int =
    if (step.isInstanceOf[AxisExpression]) {
      AxisInfo.inverseAxis(step.asInstanceOf[AxisExpression].getAxis)
    } else if (step.isInstanceOf[FilterExpression]) {
      getAxisForPathStep(
        step.asInstanceOf[FilterExpression].getSelectExpression)
    } else if (step.isInstanceOf[FirstItemExpression]) {
      getAxisForPathStep(
        step.asInstanceOf[FirstItemExpression].getBaseExpression)
    } else if (step.isInstanceOf[SubscriptExpression]) {
      getAxisForPathStep(
        step.asInstanceOf[SubscriptExpression].getBaseExpression)
    } else if (step.isInstanceOf[SlashExpression]) {
      getAxisForPathStep(step.asInstanceOf[SlashExpression].getFirstStep)
    } else if (step.isInstanceOf[ContextItemExpression]) {
      AxisInfo.SELF
    } else {
      throw new XPathException(
        "The path in a pattern must contain simple steps")
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
