package org.orbeon.saxon.expr.oper

import org.orbeon.saxon.expr.{Expression, Operand, OperandRole}

import java.util.Arrays
import java.util.function.Predicate

object OperandArray {

  def every[T](args: Array[T], condition: Predicate[T]): Boolean =
    args.forall(condition.test)

  def some[T](args: Array[T], condition: Predicate[T]): Boolean =
    args.exists(condition.test)
}

class OperandArray extends Iterable[Operand] {

  private var operandArray: Array[Operand] = _

  def this(parent: Expression, args: Array[Expression]) = {
    this()
    this.operandArray = new Array[Operand](args.length)
    for (i <- args.indices)
      operandArray(i) = new Operand(parent, args(i), OperandRole.NAVIGATE)
  }

  def this(parent: Expression,
           args: Array[Expression],
           roles: Array[OperandRole]) = {
    this()
    this.operandArray = Array.ofDim[Operand](args.length)
    for (i <- args.indices)
      operandArray(i) = new Operand(parent, args(i), roles(i))
  }

  def this(parent: Expression, args: Array[Expression], role: OperandRole) = {
    this()
    this.operandArray = Array.ofDim[Operand](args.length)
    for (i <- args.indices)
      operandArray(i) = new Operand(parent, args(i), role)
  }

  def this(operands: Array[Operand]) = {
    this()
    this.operandArray = operands
  }

  override def iterator: Iterator[Operand] =
    operandArray.iterator

  def copy(): Array[Operand] = Arrays.copyOf(operandArray, operandArray.length)

  def getRoles: Array[OperandRole] = {
    val or: Array[OperandRole] = Array.ofDim[OperandRole](operandArray.length)
    for (i <- or.indices) {
      or(i) = operandArray(i).getOperandRole
    }
    or
  }

  def getOperand(n: Int): Operand = operandArray(n)

  def getOperandExpression(n: Int): Expression =
    operandArray(n).getChildExpression

  def operands: Iterable[Operand] = operandArray

  def operandExpressions(): Iterable[Expression] =
    operandArray.toIndexedSeq.map(_.getChildExpression)

  def setOperand(n: Int, child: Expression): Unit =
    if (operandArray(n).getChildExpression ne child)
      operandArray(n).setChildExpression(child)

  def getNumberOfOperands: Int = operandArray.length
}
