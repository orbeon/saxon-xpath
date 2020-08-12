package net.sf.saxon.expr.flwor

import net.sf.saxon.expr._

import net.sf.saxon.expr.oper.OperandArray

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.Sequence

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

//remove if not needed
import scala.jdk.CollectionConverters._

class TupleExpression extends Expression {

  @BeanProperty
   var operanda: OperandArray = _

  override def operands(): java.lang.Iterable[Operand] = operanda.operands()

  def setVariables(refs: List[LocalVariableReference]): Unit = {
    val e: Array[Expression] = Array.ofDim[Expression](refs.size)
    for (i <- 0 until refs.size) {
      e(i) = refs.get(i)
    }
    this.operanda = new OperandArray(this, e, OperandRole.SAME_FOCUS_ACTION)
  }

  def getSize(): Int = getOperanda.getNumberOfOperands

  def getSlot(i: Int): LocalVariableReference =
    getOperanda.getOperandExpression(i).asInstanceOf[LocalVariableReference]

  def setSlot(i: Int, ref: LocalVariableReference): Unit = {
    getOperanda.setOperand(i, ref)
  }

  def includesBinding(binding: Binding): Boolean =
    operands().asScala
      .find(
        _.getChildExpression.asInstanceOf[LocalVariableReference].getBinding ==
          binding)
      .map(_ => true)
      .getOrElse(false)

  def getItemType(): ItemType =
    getConfiguration.getJavaExternalObjectType(classOf[AnyRef])

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    for (i <- 0 until getSize) {
      operanda.getOperand(i).typeCheck(visitor, contextInfo)
    }
    this
  }

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def equals(other: Any): Boolean =
    if (!(other.isInstanceOf[TupleExpression])) {
      false
    } else {
      val t2: TupleExpression = other.asInstanceOf[TupleExpression]
      if (getOperanda.getNumberOfOperands != t2.getOperanda.getNumberOfOperands) {
        false
      }
      for (i <- 0 until getSize if !getSlot(i).isEqual(t2.getSlot(i))) {
        false
      }
      true
    }

  override def computeHashCode(): Int = {
    var h: Int = 77
    for (o <- operands().asScala) {
      h ^= o.getChildExpression.hashCode
    }
    h
  }

  def copy(rebindings: RebindingMap): Expression = {
    val n: Int = getOperanda.getNumberOfOperands
    val refs2: List[LocalVariableReference] =
      new ArrayList[LocalVariableReference](n)
    for (i <- 0 until n) {
      refs2.add(
        getSlot(i).copy(rebindings).asInstanceOf[LocalVariableReference])
    }
    val t2: TupleExpression = new TupleExpression()
    ExpressionTool.copyLocationInfo(this, t2)
    t2.setVariables(refs2)
    t2
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("tuple", this)
    for (o <- operands().asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

  override def evaluateItem(context: XPathContext): Tuple = {
    val n: Int = getSize
    val tuple: Array[Sequence] = Array.ofDim[Sequence](n)
    for (i <- 0 until n) {
      tuple(i) = getSlot(i).evaluateVariable(context)
    }
    new Tuple(tuple)
  }

  override def getExpressionName(): String = "tuple"

  def setCurrentTuple(context: XPathContext, tuple: Tuple): Unit = {
    val members: Array[Sequence] = tuple.getMembers
    val n: Int = getSize
    for (i <- 0 until n) {
      context.setLocalVariable(getSlot(i).getBinding.getLocalSlotNumber,
        members(i))
    }
  }

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def getIntrinsicDependencies(): Int = 0

}
