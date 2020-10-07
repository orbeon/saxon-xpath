package org.orbeon.saxon.ma.arrays

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.oper.OperandArray

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Cardinality

import org.orbeon.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class SquareArrayConstructor(children: List[Expression]) extends Expression {
  val kids: Array[Expression] = children.toArray(Array.ofDim[Expression](0))

  @BeanProperty
  var operanda: OperandArray =
    new OperandArray(this, kids, OperandRole.NAVIGATE)

  for (e <- children.asScala) {
    adoptChildExpression(e)
  }

  def getOperand(i: Int): Operand = operanda.getOperand(i)

  override def operands: java.lang.Iterable[Operand] = operanda.operands

  override def getExpressionName: String = "SquareArrayConstructor"

  override def getStreamerName: String = "ArrayBlock"

  override def computeSpecialProperties(): Int = 0

  override def equals(other: Any): Boolean =
    if (!(other.isInstanceOf[SquareArrayConstructor])) {
      false
    } else {
      val ab2: SquareArrayConstructor =
        other.asInstanceOf[SquareArrayConstructor]
      if (ab2.getOperanda.getNumberOfOperands != getOperanda.getNumberOfOperands) {
        return        false
      }
      for (i <- 0 until getOperanda.getNumberOfOperands
           if getOperanda.getOperand(i) != ab2.getOperanda.getOperand(i)) {
        false
      }
      true
    }

  override def computeHashCode(): Int = {
    var h: Int = 0x878b92a0
    for (o <- operands.asScala) {
      h ^= o.getChildExpression.hashCode
    }
    h
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val e: Expression = super.typeCheck(visitor, contextInfo)
    if (e != this) {
      return e
    }
    preEvaluate(visitor)
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val e: Expression = super.optimize(visitor, contextInfo)
    if (e != this) {
      return      e
    }
    preEvaluate(visitor)
  }

  private def preEvaluate(visitor: ExpressionVisitor): Expression = {
    val allFixed: Boolean = false
    for (o <- operands.asScala if !(o.getChildExpression.isInstanceOf[Literal])) {
      this
    }
    try Literal.makeLiteral(evaluateItem(visitor.makeDynamicContext()), this)
    catch {
      case e: XPathException => this

    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val m2: List[Expression] =
      new ArrayList[Expression](getOperanda.getNumberOfOperands)
    for (o <- operands.asScala) {
      m2.add(o.getChildExpression.copy(rebindings))
    }
    val b2: SquareArrayConstructor = new SquareArrayConstructor(m2)
    ExpressionTool.copyLocationInfo(this, b2)
    b2
  }

  def getItemType: ItemType = {
    var contentType: ItemType = null
    var contentCardinality: Int = StaticProperty.EXACTLY_ONE
    val th: TypeHierarchy = getConfiguration.getTypeHierarchy
    for (e <- getOperanda.operandExpressions().asScala) {
      if (contentType == null) {
        contentType = e.getItemType
        contentCardinality = e.getCardinality
      } else {
        contentType = Type.getCommonSuperType(contentType, e.getItemType, th)
        contentCardinality =
          Cardinality.union(contentCardinality, e.getCardinality)
      }
    }
    if (contentType == null) {
      contentType = ErrorType
    }
    new ArrayItemType(
      SequenceType.makeSequenceType(contentType, contentCardinality))
  }

  override def getStaticUType(contextItemType: UType): UType = UType.FUNCTION

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("arrayBlock", this)
    for (o <- operands.asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

  override def toShortString: String = {
    val n: Int = getOperanda.getNumberOfOperands
    n match {
      case 0 => "[]"
      case 1 =>
        "[" +
          getOperanda.getOperand(0).getChildExpression.toShortString +
          "]"
      case 2 =>
        "[" +
          getOperanda.getOperand(0).getChildExpression.toShortString +
          ", " +
          getOperanda.getOperand(1).getChildExpression.toShortString +
          "]"
      case _ =>
        "[" +
          getOperanda.getOperand(0).getChildExpression.toShortString +
          ", ...]"

    }
  }

  def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def evaluateItem(context: XPathContext): Item = {
    val value: List[GroundedValue] =
      new ArrayList[GroundedValue](getOperanda.getNumberOfOperands)
    for (o <- operands.asScala) {
      val s: GroundedValue =
        ExpressionTool.eagerEvaluate(o.getChildExpression, context)
      value.add(s)
    }
    new SimpleArrayItem(value)
  }

}
