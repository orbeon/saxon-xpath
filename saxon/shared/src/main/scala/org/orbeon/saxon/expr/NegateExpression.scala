package org.orbeon.saxon.expr

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.DoubleValue

import org.orbeon.saxon.value.NumericValue

import org.orbeon.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

class NegateExpression(base: Expression) extends UnaryExpression(base) {

  @BooleanBeanProperty
  var backwardsCompatible: Boolean = _

   def getOperandRole(): OperandRole = OperandRole.SINGLE_ATOMIC

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.UNARY_EXPR, "-", 0)
    val operand: Expression = visitor.getConfiguration
      .getTypeChecker(backwardsCompatible)
      .staticTypeCheck(getBaseExpression,
        SequenceType.OPTIONAL_NUMERIC,
        role,
        visitor)
    this.setBaseExpression(operand)
    if (operand.isInstanceOf[Literal]) {
      val v: GroundedValue = operand.asInstanceOf[Literal].getValue
      if (v.isInstanceOf[NumericValue]) {
        return Literal.makeLiteral(v.asInstanceOf[NumericValue].negate(), this)
      }
    }
    this
  }

  override def getItemType: ItemType =
    getBaseExpression.getItemType.getPrimitiveItemType

  override def computeCardinality(): Int =
    getBaseExpression.getCardinality & ~StaticProperty.ALLOWS_MANY

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): NumericValue = {
    val v1: NumericValue =
      getBaseExpression.evaluateItem(context).asInstanceOf[NumericValue]
    if (v1 == null) {
      if (backwardsCompatible) return DoubleValue.NaN else return null
    }
    v1.negate()
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: NegateExpression = new NegateExpression(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override  def displayOperator(config: Configuration): String = "-"

  override def getExpressionName: String = "minus"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("minus", this)
    if (backwardsCompatible) {
      out.emitAttribute("vn", "1")
    }
    getBaseExpression.export(out)
    out.endElement()
  }

}
