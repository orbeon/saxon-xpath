package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.parser.ContextItemStaticInfo
import net.sf.saxon.expr.parser.ExpressionTool
import net.sf.saxon.expr.parser.ExpressionVisitor
import net.sf.saxon.model.ItemType
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.jiter.MonoIterator

abstract class UnaryExpression(p0: Expression) extends Expression {

   var operand: Operand = new Operand(this, p0, getOperandRole)

  ExpressionTool.copyLocationInfo(p0, this)

  def getBaseExpression: Expression = operand.getChildExpression

  def setBaseExpression(child: Expression): Unit = {
    operand.setChildExpression(child)
  }

  def getOperand: Operand = operand

  override def operands: java.lang.Iterable[Operand] = new MonoIterator(operand).next()

   def getOperandRole: OperandRole

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    operand.typeCheck(visitor, contextInfo)
    try
      if (getBaseExpression.isInstanceOf[Literal]) {
        val e2 = Literal.makeLiteral(iterate(visitor.getStaticContext.makeEarlyEvaluationContext()).materialize(), this)
        ExpressionTool.copyLocationInfo(this, e2)
        return e2
      }
    catch {
      case _: Exception =>
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    operand.optimize(visitor, contextInfo)
    val base: Expression = getBaseExpression
    try if (base.isInstanceOf[Literal]) {
      Literal.makeLiteral(
        iterate(visitor.getStaticContext.makeEarlyEvaluationContext())
          .materialize(),
        this)
    } catch {
      case _: XPathException =>
    }
    this
  }

  override def computeSpecialProperties(): Int = getBaseExpression.getSpecialProperties

  def computeCardinality(): Int = getBaseExpression.getCardinality

  def getItemType: ItemType = getBaseExpression.getItemType

  override def equals(other: Any): Boolean =
    other != null && this.getClass == other.getClass &&
      this.getBaseExpression
        .isEqual(other.asInstanceOf[UnaryExpression].getBaseExpression)

  override def computeHashCode(): Int =
    ("UnaryExpression " + getClass).hashCode ^ getBaseExpression.hashCode

  override def toString: String =
    getExpressionName + "(" + getBaseExpression + ")"

  override def toShortString: String =
    getExpressionName + "(" + getBaseExpression.toShortString + ")"

  def export(out: ExpressionPresenter): Unit = {
    val name: String = getExpressionName
    if (name == null) {
      out.startElement("unaryOperator", this)
      val op: String = displayOperator(out.getConfiguration)
      if (op != null) {
        out.emitAttribute("op", op)
      }
    } else {
      out.startElement(name, this)
    }
    emitExtraAttributes(out)
    getBaseExpression.export(out)
    out.endElement()
  }

   def emitExtraAttributes(out: ExpressionPresenter): Unit = ()

   def displayOperator(config: Configuration): String = null

}
