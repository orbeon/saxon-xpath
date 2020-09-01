package net.sf.saxon.expr.sort

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.value.SequenceType

import ConditionalSorter._

object ConditionalSorter {

  @FunctionalInterface
  private trait RewriteAction {

    def rewrite(e: Expression): Expression

  }

  private val DOC_SORTER_ROLE: OperandRole = new OperandRole(
    OperandRole.CONSTRAINED_CLASS,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE)

}

class ConditionalSorter(condition: Expression, sorter: DocumentSorter)
  extends Expression {

  private var conditionOp: Operand =
    new Operand(this, condition, OperandRole.SINGLE_ATOMIC)

  private var sorterOp: Operand = new Operand(this, sorter, DOC_SORTER_ROLE)

  adoptChildExpression(condition)

  adoptChildExpression(sorter)

  override def operands: java.lang.Iterable[Operand] =
    operandList(conditionOp, sorterOp)

  def setCondition(condition: Expression): Unit = {
    conditionOp.setChildExpression(condition)
  }

  def setDocumentSorter(documentSorter: DocumentSorter): Unit = {
    sorterOp.setChildExpression(documentSorter)
  }

  def getCondition: Expression = conditionOp.getChildExpression

  def getDocumentSorter: DocumentSorter =
    sorterOp.getChildExpression.asInstanceOf[DocumentSorter]

  override def simplify(): Expression = rewrite(res => res.simplify())

 override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression =
    rewrite((exp) => exp.typeCheck(visitor, contextInfo))

  override def getCardinality(): Int = getDocumentSorter.getCardinality

 override  def computeSpecialProperties(): Int =
    getCondition.getSpecialProperties |
      StaticProperty.ORDERED_NODESET & ~StaticProperty.REVERSE_DOCUMENT_ORDER

  def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression =
    rewrite((exp) => exp.optimize(visitor, contextInfo))

  private def rewrite(rewriter: RewriteAction): Expression = {
    val base: Expression = rewriter.rewrite(getDocumentSorter)
    if (base.isInstanceOf[DocumentSorter]) {
      sorterOp.setChildExpression(base)
    } else {
      return base
    }
    val cond: Expression = rewriter.rewrite(getCondition)
    if (cond.isInstanceOf[Literal]) {
      val b: Boolean =
        cond.asInstanceOf[Literal].value.effectiveBooleanValue()
      if (b) {
        base
      } else {
        base.asInstanceOf[DocumentSorter].getBaseExpression
      }
    } else {
      conditionOp.setChildExpression(cond)
      this
    }
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    val base: Expression =
      getDocumentSorter.unordered(retainAllNodes, forStreaming)
    if (base.isInstanceOf[DocumentSorter]) {
      this
    } else {
      base
    }
  }

   def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  def copy(rebindings: RebindingMap): Expression = {
    val cs: ConditionalSorter = new ConditionalSorter(
      getCondition.copy(rebindings),
      getDocumentSorter.copy(rebindings).asInstanceOf[DocumentSorter])
    ExpressionTool.copyLocationInfo(this, cs)
    cs
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("conditionalSort", this)
    getCondition.export(out)
    getDocumentSorter.export(out)
    out.endElement()
  }

  def getItemType: ItemType = getDocumentSorter.getItemType

 override def iterate(context: XPathContext): SequenceIterator = {
    val b: Boolean = getCondition.effectiveBooleanValue(context)
    if (b) {
      getDocumentSorter.iterate(context)
    } else {
      getDocumentSorter.getBaseExpression.iterate(context)
    }
  }

  override def getExpressionName: String = "conditionalSort"

}
