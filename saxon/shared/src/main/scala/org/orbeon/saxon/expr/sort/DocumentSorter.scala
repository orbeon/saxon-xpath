package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.model.Affinity

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.pattern.AnyNodeTest

import org.orbeon.saxon.pattern.Pattern

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Cardinality

import org.orbeon.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.util.control.Breaks._

class DocumentSorter(base: Expression) extends UnaryExpression(base) {

  val props: Int = base.getSpecialProperties

  @BeanProperty
  var comparer: ItemOrderComparer =
    if (((props & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0) ||
      (props & StaticProperty.SINGLE_DOCUMENT_NODESET) != 0)
      LocalOrderComparer.getInstance
    else GlobalOrderComparer.getInstance

  def this(base: Expression, intraDocument: Boolean) = {
    this(???) /* TODO: Scala does not allow multiple super constructor calls
     * Change this code to call a constructor of the current class instead.
     * For your convenience, here is the invalid super constructor call:
     * }super(base)
     */

    comparer =
      if (intraDocument) LocalOrderComparer.getInstance
      else GlobalOrderComparer.getInstance
  }

  def getOperandRole(): OperandRole = OperandRole.SAME_FOCUS_ACTION

  override def getExpressionName: String = "docOrder"

  override def simplify(): Expression = {
    val operand: Expression = getBaseExpression.simplify()
    if (operand.hasSpecialProperty(StaticProperty.ORDERED_NODESET)) {
      return operand
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val e2: Expression = super.typeCheck(visitor, contextInfo)
    if (e2 != this) {
      return e2
    }
    val th = visitor.getConfiguration.getTypeHierarchy
    if (th.relationship(getBaseExpression.getItemType, AnyNodeTest) ==
      Affinity.DISJOINT) {
      getBaseExpression
    }
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.MISC, "document-order sorter", 0)
    val operand: Expression = visitor.getConfiguration
      .getTypeChecker(false)
      .staticTypeCheck(getBaseExpression,
        SequenceType.NODE_SEQUENCE,
        role,
        visitor)
    this.setBaseExpression(operand)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextInfo)
    val sortable: Expression = getBaseExpression
    var tryHarder: Boolean = sortable.isStaticPropertiesKnown
    breakable {
      while (true) {
        if (sortable.hasSpecialProperty(StaticProperty.ORDERED_NODESET)) {
          return sortable
        }
        if (!Cardinality.allowsMany(sortable.getCardinality)) {
          return sortable
        }
        if (sortable.isInstanceOf[SlashExpression]) {
          val slash: SlashExpression = sortable.asInstanceOf[SlashExpression]
          val lhs: Expression = slash.getLhsExpression
          val rhs: Expression = slash.getRhsExpression
          if (lhs.isInstanceOf[ConditionalSorter] &&
            slash.getRhsExpression.hasSpecialProperty(
              StaticProperty.PEER_NODESET)) {
            val c: ConditionalSorter = lhs.asInstanceOf[ConditionalSorter]
            val d: DocumentSorter = c.getDocumentSorter
            val condition: Expression = c.getCondition
            var s: Expression = new SlashExpression(d.getBaseExpression, rhs)
            s = s.optimize(visitor, contextInfo)
            new ConditionalSorter(condition, new DocumentSorter(s))
          }
          if (lhs.isInstanceOf[DocumentSorter] && rhs
            .isInstanceOf[AxisExpression] &&
            rhs.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD) {
            val s1: SlashExpression = new SlashExpression(
              lhs.asInstanceOf[DocumentSorter].getBaseExpression,
              rhs)
            ExpressionTool.copyLocationInfo(this, s1)
            new DocumentSorter(s1).optimize(visitor, contextInfo)
          }
          if (!ExpressionTool.dependsOnFocus(rhs) &&
            !rhs.hasSpecialProperty(StaticProperty.HAS_SIDE_EFFECTS) &&
            rhs.hasSpecialProperty(StaticProperty.NO_NODES_NEWLY_CREATED)) {
            this.setBaseExpression(slash.getRhsExpression)
            optimize(visitor, contextInfo)
          }
        }
        if (tryHarder) {
          sortable.resetLocalStaticProperties()
          tryHarder = false
        } else {
          break()
        }
      }
    }
    if (sortable
      .isInstanceOf[SlashExpression] && !visitor.isOptimizeForStreaming &&
      !(getParentExpression.isInstanceOf[ConditionalSorter])) {
      visitor
        .obtainOptimizer()
        .makeConditionalDocumentSorter(this,
          sortable.asInstanceOf[SlashExpression])
    }
    this
  }

  override def getNetCost(): Int = 30

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    val operand: Expression =
      getBaseExpression.unordered(retainAllNodes, forStreaming)
    if (operand.hasSpecialProperty(StaticProperty.ORDERED_NODESET)) {
      return operand
    }
    if (!retainAllNodes) {
      return  operand
    } else if (operand.isInstanceOf[SlashExpression]) {
      val exp: SlashExpression = operand.asInstanceOf[SlashExpression]
      var a: Expression = exp.getSelectExpression
      var b: Expression = exp.getActionExpression
      a = ExpressionTool.unfilteredExpression(a, allowPositional = false)
      b = ExpressionTool.unfilteredExpression(b, allowPositional = false)
      if (a.isInstanceOf[AxisExpression] &&
        (a.asInstanceOf[AxisExpression].getAxis == AxisInfo.DESCENDANT ||
          a.asInstanceOf[AxisExpression]
            .getAxis == AxisInfo.DESCENDANT_OR_SELF) &&
        b.isInstanceOf[AxisExpression] &&
        b.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD) {
        operand.unordered(retainAllNodes, forStreaming = false)
      }
    }
    this.setBaseExpression(operand)
    this
  }

  override def computeSpecialProperties(): Int =
    getBaseExpression.getSpecialProperties | StaticProperty.ORDERED_NODESET

  def copy(rebindings: RebindingMap): Expression = {
    val ds: DocumentSorter = new DocumentSorter(
      getBaseExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, ds)
    ds
  }

  override def toPattern(config: Configuration): Pattern =
    getBaseExpression.toPattern(config)

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def iterate(context: XPathContext): SequenceIterator =
    new DocumentOrderIterator(getBaseExpression.iterate(context), comparer)

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    getBaseExpression.effectiveBooleanValue(context)

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("docOrder", this)
    out.emitAttribute(
      "intra",
      if (comparer.isInstanceOf[LocalOrderComparer]) "1" else "0")
    getBaseExpression.export(out)
    out.endElement()
  }

  override def getStreamerName: String = "DocumentSorterAdjunct"

}
