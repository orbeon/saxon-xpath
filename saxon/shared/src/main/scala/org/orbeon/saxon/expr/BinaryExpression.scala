package org.orbeon.saxon.expr

import java.util
import java.util.List

import org.orbeon.saxon.expr.BinaryExpression._
import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.jiter.PairIterator
import org.orbeon.saxon.value.Cardinality

import scala.beans.BeanProperty


object BinaryExpression {

  def isCommutative(operator: Int): Boolean =
    operator == Token.AND       ||
    operator == Token.OR        ||
    operator == Token.UNION     ||
    operator == Token.INTERSECT ||
    operator == Token.PLUS      ||
    operator == Token.MULT      ||
    operator == Token.EQUALS    ||
    operator == Token.FEQ       ||
    operator == Token.NE        ||
    operator == Token.FNE

  def isAssociative(operator: Int): Boolean =
    operator == Token.AND       ||
    operator == Token.OR        ||
    operator == Token.UNION     ||
    operator == Token.INTERSECT ||
    operator == Token.PLUS      ||
    operator == Token.MULT

  def isInverse(op1: Int, op2: Int): Boolean =
    op1 != op2 && op1 == Token.inverse(op2)
}

abstract class BinaryExpression(p0: Expression,
                                val operator: Int,
                                p1: Expression)
  extends Expression {

  @BeanProperty
  var lhs: Operand = new Operand(this, p0, getOperandRole(0))

  @BeanProperty
  var rhs: Operand = new Operand(this, p1, getOperandRole(1))

  adoptChildExpression(p0)
  adoptChildExpression(p1)

  override def operands: java.lang.Iterable[Operand] =
    () => new PairIterator(lhs, rhs)

  def getOperandRole(arg: Int): OperandRole =
    OperandRole.SINGLE_ATOMIC

  def getLhsExpression: Expression = lhs.getChildExpression

  def setLhsExpression(child: Expression): Unit =
    lhs.setChildExpression(child)

  def getRhsExpression: Expression = rhs.getChildExpression

  def setRhsExpression(child: Expression): Unit =
    rhs.setChildExpression(child)

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    resetLocalStaticProperties()
    lhs.typeCheck(visitor, contextInfo)
    rhs.typeCheck(visitor, contextInfo)
    try
      if (getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal]) {
        val v = evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext()).materialize
        return Literal.makeLiteral(v, this)
      }
    catch {
      case _: XPathException =>
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    lhs.optimize(visitor, contextItemType)
    rhs.optimize(visitor, contextItemType)
    try {
      val opt: Optimizer = visitor.obtainOptimizer()
      if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING) && getLhsExpression.isInstanceOf[Literal] && getRhsExpression.isInstanceOf[Literal]) {
        val item = evaluateItem(visitor.getStaticContext.makeEarlyEvaluationContext())
        if (item != null) {
          val v = item.materialize
          return Literal.makeLiteral(v, this)
        }
      }
    } catch {
      case _: XPathException =>
    }
    this
  }

  override def setFlattened(flattened: Boolean): Unit = {
    getLhsExpression.setFlattened(flattened)
    getRhsExpression.setFlattened(flattened)
  }

  def getOperator: Int = operator

  def computeCardinality(): Int = {
    val lhs = getLhsExpression
    val rhs = getRhsExpression
    if (! Cardinality.allowsZero(lhs.getCardinality) &&
        lhs.getItemType.isInstanceOf[AtomicType] &&
        ! Cardinality.allowsZero(rhs.getCardinality) &&
        rhs.getItemType.isInstanceOf[AtomicType])
      StaticProperty.EXACTLY_ONE
    else
      StaticProperty.ALLOWS_ZERO_OR_ONE
  }

  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def getImplementationMethod: Int =
    EVALUATE_METHOD | ITERATE_METHOD

  override def equals(other: Any): Boolean = {
    other match {
      case b: BinaryExpression if hasCompatibleStaticContext(other.asInstanceOf[Expression]) =>
        val lhs1 = getLhsExpression
        val rhs1 = getRhsExpression
        val lhs2 = b.getLhsExpression
        val rhs2 = b.getRhsExpression
        if (operator == b.operator) {
          if (lhs1.isEqual(lhs2) && rhs1.isEqual(rhs2))
            return true
          if (isCommutative(operator) && lhs1.isEqual(rhs2) && rhs1.isEqual(lhs2))
            return true
          if (isAssociative(operator) &&
            pairwiseEqual(flattenExpression(new util.ArrayList(4)),
              b.flattenExpression(new util.ArrayList(4)))) {
            return true
          }
        }
        return isInverse(operator, b.operator) && lhs1.isEqual(rhs2) && rhs1.isEqual(lhs2)
      case _ =>
    }
    false
  }

  private def flattenExpression(list: List[Expression]): List[Expression] = {
    getLhsExpression match {
      case binaryExpr: BinaryExpression if binaryExpr.operator == operator =>
        binaryExpr.flattenExpression(list)
      case _ =>
        val h = getLhsExpression.hashCode
        list.add(getLhsExpression)
        var i = list.size - 1
        while (i > 0 && h > list.get(i - 1).hashCode) {
          list.set(i, list.get(i - 1))
          list.set(i - 1, getLhsExpression)
          i -= 1
        }
    }
    getRhsExpression match {
      case binaryExpr: BinaryExpression if binaryExpr.operator == operator =>
        binaryExpr.flattenExpression(list)
      case _ =>
        val h = getRhsExpression.hashCode
        list.add(getRhsExpression)
        var i = list.size - 1
        while (i > 0 && h > list.get(i - 1).hashCode) {
          list.set(i, list.get(i - 1))
          list.set(i - 1, getRhsExpression)
          i -= 1
        }
    }
    list
  }

  private def pairwiseEqual(a: List[_], b: List[_]): Boolean = {
    if (a.size != b.size)
      return false
    for (i <- 0 until a.size if a.get(i) != b.get(i))
      return false
    true
  }

  override def computeHashCode(): Int = {
    val op = Math.min(operator, Token.inverse(operator))
    ("BinaryExpression " + op).hashCode ^ getLhsExpression.hashCode ^ getRhsExpression.hashCode
  }

  override def toString: String =
    ExpressionTool.parenthesize(getLhsExpression) + " " +
      displayOperator() +
      " " +
      ExpressionTool.parenthesize(getRhsExpression)

  override def toShortString: String =
    parenthesize(getLhsExpression) + " " + displayOperator() +
      " " +
      parenthesize(getRhsExpression)

  private def parenthesize(operand: Expression): String = {
    var operandStr = operand.toShortString
    operand match {
      case binaryExpr: BinaryExpression if XPathParser.operatorPrecedence(binaryExpr.operator) < XPathParser.operatorPrecedence(operator) =>
        operandStr = "(" + operandStr + ")"
      case _ =>
    }
    operandStr
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement(tag(), this)
    out.emitAttribute("op", displayOperator())
    explainExtraAttributes(out)
    getLhsExpression.export(out)
    getRhsExpression.export(out)
    out.endElement()
  }

  def tag(): String = "operator"

  def explainExtraAttributes(out: ExpressionPresenter): Unit = ()
  def displayOperator(): String = Token.tokens(operator)
}
