package net.sf.saxon.expr

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.oper.OperandArray

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.Arrays

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

abstract class SimpleExpression extends Expression with Callable {

  @BeanProperty
   var operanda: OperandArray = _

  override def operands: java.lang.Iterable[Operand] = operanda.operands

  def setArguments(sub: Array[Expression]): Unit = {
    if (getOperanda != null && getOperanda.getNumberOfOperands > 0) {
      throw new IllegalArgumentException(
        "Cannot replace existing argument array")
    }
    val sub2: Array[Expression] = Arrays.copyOf(sub, sub.length)
    val roles: Array[OperandRole] = Array.ofDim[OperandRole](sub.length)
    Arrays.fill(roles.asInstanceOf[Array[Object]], OperandRole.NAVIGATE)
    this.operanda = new OperandArray(this, sub2, roles)
  }

  def copy(rebindings: RebindingMap): Expression = {
    val se2: SimpleExpression = getClass.newInstance()
    val a2: Array[Expression] =
      Array.ofDim[Expression](operanda.getNumberOfOperands)
    var i: Int = 0
    for (o <- operands.asScala) {
      i += 1
      a2(i) = o.getChildExpression.copy(rebindings)
    }
    val o2: OperandArray = new OperandArray(se2, a2, operanda.getRoles)
    se2.setOperanda(o2)
    se2
  }

   def copyOperandsFrom(se1: SimpleExpression): SimpleExpression = {
    val a2: Array[Expression] =
      Array.ofDim[Expression](se1.operanda.getNumberOfOperands)
    var i: Int = 0
    for (o <- se1.operands.asScala) {
      i += 1
      a2(i) = o.getChildExpression.copy(new RebindingMap())
    }
    val o2: OperandArray = new OperandArray(this, a2, se1.operanda.getRoles)
    this.operanda = o2
    this
  }

  def getItemType: ItemType = Type.ITEM_TYPE

  def computeCardinality(): Int =
    if ((getImplementationMethod & Expression.EVALUATE_METHOD) ==
      0) {
      StaticProperty.ALLOWS_ONE_OR_MORE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }

  override def evaluateItem(context: XPathContext): Item =
    call(context, evaluateArguments(context)).head

  override def iterate(context: XPathContext): SequenceIterator =
    call(context, evaluateArguments(context)).iterate()

  override def process(output: Outputter, context: XPathContext): Unit = {
    val iter: SequenceIterator =
      call(context, evaluateArguments(context)).iterate()
    iter.forEachOrFail((it) =>
      output.append(it, getLocation, ReceiverOption.ALL_NAMESPACES))
  }

  private def evaluateArguments(context: XPathContext): Array[Sequence] = {
    val iters: Array[Sequence] =
      SequenceTool.makeSequenceArray(getOperanda.getNumberOfOperands)
    var i: Int = 0
    for (o <- operands.asScala) {
      i += 1
      iters(i) =
        SequenceTool.toLazySequence(o.getChildExpression.iterate(context))
    }
    iters
  }

  def export(destination: ExpressionPresenter): Unit = {
    throw new XPathException(
      "In general, stylesheets using extension instructions cannot be exported")
  }

  def getExpressionType: String = getClass.getName

}
