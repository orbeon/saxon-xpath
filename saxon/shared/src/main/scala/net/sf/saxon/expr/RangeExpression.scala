////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.UType

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerRange

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType




class RangeExpression(start: Expression, end: Expression)
    extends BinaryExpression(start, Token.TO, end) {

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    val backCompat: Boolean =
      visitor.getStaticContext.isInBackwardsCompatibleMode
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(backCompat)
    val role0: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "to", 0)
    this.setLhsExpression(tc.staticTypeCheck(getLhsExpression,
                                            SequenceType.OPTIONAL_INTEGER,
                                            role0,
                                            visitor))
    val role1: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "to", 1)
    this.setRhsExpression(tc.staticTypeCheck(getRhsExpression,
                                            SequenceType.OPTIONAL_INTEGER,
                                            role1,
                                            visitor))
    makeConstantRange()
  }

  /*@NotNull*/

  override def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.optimize(visitor, contextInfo)
    getRhs.optimize(visitor, contextInfo)
    makeConstantRange()
  }

  private def makeConstantRange(): Expression = {
    if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression
          .isInstanceOf[Literal]) {
      val v0: GroundedValue = getLhsExpression.asInstanceOf[Literal].getValue
      val v1: GroundedValue = getRhsExpression.asInstanceOf[Literal].getValue
      if (v0.isInstanceOf[Int64Value] && v1.isInstanceOf[Int64Value]) {
        val i0: Long = v0.asInstanceOf[Int64Value].longValue()
        val i1: Long = v1.asInstanceOf[Int64Value].longValue()
        var result: Literal = null
        if (i0 > i1) {
          result = Literal.makeEmptySequence
        } else if (i0 == i1) {
          result = Literal.makeLiteral(Int64Value.makeIntegerValue(i0), this)
        } else {
          if (i1 - i0 > java.lang.Integer.MAX_VALUE) {
            throw new XPathException(
              "Maximum length of sequence in Saxon is " + java.lang.Integer.MAX_VALUE,
              "XPDY0130")
          }
          result = Literal.makeLiteral(new IntegerRange(i0, i1), this)
        }
        ExpressionTool.copyLocationInfo(this, result)
        return result
      }
    }
    this
  }

  /*@NotNull*/

  def getItemType: ItemType = BuiltInAtomicType.INTEGER

  /**
    * Get the static type of the expression as a UType, following precisely the type
    * inference rules defined in the XSLT 3.0 specification.
    *
    * @return the static item type of the expression according to the XSLT 3.0 defined rules
    * @param contextItemType
    */
  override def getStaticUType(contextItemType: UType): UType = UType.DECIMAL

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  /*@Nullable*/

  override def getIntegerBounds(): Array[IntegerValue] = {
    val start: Array[IntegerValue] = getLhsExpression.getIntegerBounds
    val end: Array[IntegerValue] = getLhsExpression.getIntegerBounds
    if (start == null || end == null) {
      null
    } else {
// range is from the smallest possible start value to the largest possible end value
      Array(start(0), end(1))
    }
  }

  /*@NotNull*/

  def copy(rebindings: RebindingMap): Expression = {
    val exp: RangeExpression = new RangeExpression(
      getLhsExpression.copy(rebindings),
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
    * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
    * This method indicates which of these methods is provided directly. The other methods will always be available
    * indirectly, using an implementation that relies on one of the other methods.
    *
    * @return the implementation method, for example {@link #ITERATE_METHOD} or {@link #EVALUATE_METHOD} or
    * {@link #PROCESS_METHOD}
    */
  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  /**
    * Get a name identifying the kind of expression, in terms meaningful to a user.
    *
    * @return a name identifying the kind of expression, in terms meaningful to a user.
    * The name will always be in the form of a lexical XML QName, and should match the name used
    * in export() output displaying the expression.
    */
  override def getExpressionName: String = "range"

  /**
    * Diagnostic print of expression structure. The abstract expression tree
    * is written to the supplied output destination.
    *
    * @param out the output destination for the displayed expression tree
    */
  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("to", this)
    getLhsExpression.export(out)
    getRhsExpression.export(out)
    out.endElement()
  }

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator = {
    val av1: IntegerValue =
      getLhsExpression.evaluateItem(context).asInstanceOf[IntegerValue]
    val av2: IntegerValue =
      getRhsExpression.evaluateItem(context).asInstanceOf[IntegerValue]
    RangeIterator.makeRangeIterator(av1, av2)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A RangeExpression is an expression that represents an integer sequence as
  * a pair of end-points (for example "x to y").
  * If the end-points are equal, the sequence is of length one.
  * <p>From Saxon 7.8, the sequence must be ascending; if the end-point is less
  * than the start-point, an empty sequence is returned. This is to allow
  * expressions of the form "for $i in 1 to count($seq) return ...." </p>
  */
