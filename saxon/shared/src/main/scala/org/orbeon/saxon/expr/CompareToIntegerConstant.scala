////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.expr.parser.Token

import org.orbeon.saxon.expr.sort.AtomicComparer

import org.orbeon.saxon.expr.sort.DoubleSortComparer

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.NumericValue

import scala.beans.{BeanProperty, BooleanBeanProperty}




class CompareToIntegerConstant(operand: Expression,
                               operator: Int,
                               @BeanProperty var comparand: Long)
    extends CompareToConstant(operand) {

  var oprt = operator
  this.oprt = operator

  def getRhsExpression(): Expression = new Literal(new Int64Value(comparand))

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val c2: CompareToIntegerConstant = new CompareToIntegerConstant(
      getLhsExpression.copy(rebindings),
      oprt,
      comparand)
    ExpressionTool.copyLocationInfo(this, c2)
    c2
  }

  /**
    * Is this expression the same as another expression?
    *
    * @param other the expression to be compared with this one
    * @return true if the two expressions are statically equivalent
    */
  override def equals(other: Any): Boolean =
    other.isInstanceOf[CompareToIntegerConstant] &&
      other
        .asInstanceOf[CompareToIntegerConstant]
        .getLhsExpression
        .isEqual(getLhsExpression) &&
      other.asInstanceOf[CompareToIntegerConstant].comparand ==
        comparand &&
      other.asInstanceOf[CompareToIntegerConstant].operator ==
        operator

 override def computeHashCode(): Int = {
    val h: Int = 0x836b12a0
    h + getLhsExpression.hashCode ^ comparand.toInt
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val n: NumericValue =
      getLhsExpression.evaluateItem(context).asInstanceOf[NumericValue]
    if (n.isNaN) {
      operator == Token.FNE
    }
    val c: Int = n.compareTo(comparand)
    interpretComparisonResult(c)
  }

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName: String = "compareToInt"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("compareToInt", this)
    destination.emitAttribute("op", Token.tokens(operator))
    destination.emitAttribute("val", comparand.toString)
    getLhsExpression.export(destination)
    destination.endElement()
  }

  /**
    * <p>The toString() method for an expression attempts to give a representation of the expression
    * in an XPath-like form.</p>
    * <p>For subclasses of Expression that represent XPath expressions, the result should always be a string that
    * parses as an XPath 3.0 expression</p>
    *
    * @return a representation of the expression as a string
    */
  override def toString: String =
    ExpressionTool.parenthesize(getLhsExpression) + " " +
      Token.tokens(operator) +
      " " +
      comparand

  /**
    * Produce a short string identifying the expression for use in error messages
    *
    * @return a short string, sufficient to identify the expression
    */
  override def toShortString: String =
    getLhsExpression.toShortString + " " + Token.tokens(operator) +
      " " +
      comparand

  def getAtomicComparer(): AtomicComparer = DoubleSortComparer.getInstance

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements a comparison of a numeric value to an integer constant using one of the operators
  * eq, ne, lt, gt, le, ge. The semantics are identical to ValueComparison, but this is a fast path for an
  * important common case.
  */
