package net.sf.saxon.expr

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.expr.parser.Token

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.CodepointCollatingComparer

import net.sf.saxon.expr.sort.CodepointCollator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}

class CompareToStringConstant(operand: Expression,
                              operator: Int,
                              @BeanProperty var comparand: String)
  extends CompareToConstant(operand) {

  def getRhsExpression(): Expression = new StringLiteral(comparand)

  def copy(rebindings: RebindingMap): Expression = {
    val c2: CompareToStringConstant = new CompareToStringConstant(
      getLhsExpression.copy(rebindings),
      operator,
      comparand)
    ExpressionTool.copyLocationInfo(this, c2)
    c2
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[CompareToStringConstant] &&
      other
        .asInstanceOf[CompareToStringConstant]
        .getLhsExpression
        .isEqual(getLhsExpression) &&
      other.asInstanceOf[CompareToStringConstant].comparand == comparand &&
      other.asInstanceOf[CompareToStringConstant].operator ==
        operator

  override def computeHashCode(): Int = {
    val h: Int = 0x884b12a0
    h + getLhsExpression.hashCode ^ comparand.hashCode
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean = {
    val s: CharSequence = getLhsExpression.evaluateAsString(context)
    val c: Int = CodepointCollator.compareCS(s, comparand)
    interpretComparisonResult(c)
  }

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def getExpressionName(): String = "compareToString"

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("compareToString", this)
    destination.emitAttribute("op", Token.tokens(operator))
    destination.emitAttribute("val", comparand)
    getLhsExpression.export(destination)
    destination.endElement()
  }

  override def toString(): String =
    ExpressionTool.parenthesize(getLhsExpression) + " " +
      Token.tokens(operator) +
      " \"" +
      comparand +
      "\""

  override def toShortString(): String =
    getLhsExpression.toShortString() + " " + Token.tokens(operator) +
      " \"" +
      comparand +
      "\""

  def getAtomicComparer(): AtomicComparer =
    CodepointCollatingComparer.getInstance

}
