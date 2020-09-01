package net.sf.saxon.expr.oper

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Operand

import net.sf.saxon.expr.OperandRole

import java.util.ArrayList

import java.util.Arrays

import java.util.Iterator

import java.util.List

import java.util.function.Predicate

import scala.jdk.CollectionConverters._

object OperandArray {

  def every[T](args: Array[T], condition: Predicate[T]): Boolean =
    args.find(!condition.test(_)).map(_ => false).getOrElse(true)

  def some[T](args: Array[T], condition: Predicate[T]): Boolean =
    args.find(condition.test(_)).map(_ => true).getOrElse(false)

}

class OperandArray extends java.lang.Iterable[Operand] {

  private var operandArray: Array[Operand] = _

  def this(parent: Expression, args: Array[Expression]) {
    this()
    this.operandArray = new Array[Operand](args.length)
    for (i <- 0 until args.length) {
      operandArray(i) = new Operand(parent, args(i), OperandRole.NAVIGATE)
    }
  }

  def this(parent: Expression,
           args: Array[Expression],
           roles: Array[OperandRole]) = {
    this()
    this.operandArray = Array.ofDim[Operand](args.length)
    for (i <- 0 until args.length) {
      operandArray(i) = new Operand(parent, args(i), roles(i))
    }
  }

  def this(parent: Expression, args: Array[Expression], role: OperandRole) = {
    this()
    this.operandArray = Array.ofDim[Operand](args.length)
    for (i <- 0 until args.length) {
      operandArray(i) = new Operand(parent, args(i), role)
    }
  }

  def this(operands: Array[Operand]) = {
    this()
    this.operandArray = operands
  }

  override def iterator(): Iterator[Operand] =
    Arrays.asList(operandArray: _*).iterator()

  def copy(): Array[Operand] = Arrays.copyOf(operandArray, operandArray.length)

  def getRoles: Array[OperandRole] = {
    val or: Array[OperandRole] = Array.ofDim[OperandRole](operandArray.length)
    for (i <- 0 until or.length) {
      or(i) = operandArray(i).getOperandRole
    }
    or
  }

  def getOperand(n: Int): Operand = operandArray(n)

  def getOperandExpression(n: Int): Expression =
    operandArray(n).getChildExpression

  def operands: java.lang.Iterable[Operand] = Arrays.asList(operandArray: _*)

  def operandExpressions(): java.lang.Iterable[Expression] = {
    val list: List[Expression] = new ArrayList[Expression](operandArray.length)
    for (o <- operands.asScala) {
      list.add(o.getChildExpression)
    }
    list
  }

  def setOperand(n: Int, child: Expression): Unit = {
    if (operandArray(n).getChildExpression != child) {
      operandArray(n).setChildExpression(child)
    }
  }

  def getNumberOfOperands: Int = operandArray.length

}
