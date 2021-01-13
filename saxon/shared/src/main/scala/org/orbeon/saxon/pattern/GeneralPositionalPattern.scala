package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor, RebindingMap}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.NumericValue

import scala.beans.BeanProperty


class GeneralPositionalPattern(base: NodeTest, @BeanProperty var positionExpr: Expression) extends Pattern {

  @BeanProperty
  var nodeTest: NodeTest = base

  private var usesPosition: Boolean = true

  override def operands: java.lang.Iterable[Operand] =
    new Operand(this, positionExpr, OperandRole.FOCUS_CONTROLLED_ACTION)

  def setUsesPosition(usesPosition: Boolean): Unit =
    this.usesPosition = usesPosition

  override def simplify(): Pattern = {
    positionExpr = positionExpr.simplify()
    this
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo): Pattern = {
    val cit = visitor.getConfiguration.makeContextItemStaticInfo(getItemType, maybeUndefined = false)
    positionExpr = positionExpr.typeCheck(visitor, cit)
    positionExpr = ExpressionTool.unsortedIfHomogeneous(positionExpr, forStreaming = false)
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Pattern = {
    val config = visitor.getConfiguration
    val cit    = config.makeContextItemStaticInfo(getItemType, maybeUndefined = false)
    positionExpr = positionExpr.optimize(visitor, cit)
    if (Literal.isConstantBoolean(positionExpr, value = true)) {
      return new NodeTestPattern(nodeTest)
    } else if (Literal.isConstantBoolean(positionExpr, value = false)) {
      return new NodeTestPattern(ErrorType)
    }
    if ((positionExpr.getDependencies & StaticProperty.DEPENDS_ON_POSITION) == 0)
      usesPosition = false
    if (!FilterExpression.isPositionalFilter(positionExpr, config.getTypeHierarchy)) {
      var axis = AxisInfo.CHILD.toByte
      if (nodeTest.getPrimitiveType == Type.ATTRIBUTE) {
        axis = AxisInfo.ATTRIBUTE.toByte
      } else if (nodeTest.getPrimitiveType == Type.NAMESPACE) {
        axis = AxisInfo.NAMESPACE.toByte
      }
      val ae = new AxisExpression(axis, nodeTest)
      val fe = new FilterExpression(ae, positionExpr)
      return PatternMaker
        .fromExpression(fe, config, is30 = true)
        .typeCheck(visitor, contextInfo)
    }
    this
  }

  override def getDependencies(): Int =
    positionExpr.getDependencies & (StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | StaticProperty.DEPENDS_ON_USER_FUNCTIONS)

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int =
    ExpressionTool.allocateSlots(positionExpr, nextFree, slotManager)

  def matches(item: Item, context: XPathContext): Boolean =
    item.isInstanceOf[NodeInfo] && matchesBeneathAnchor(item.asInstanceOf[NodeInfo], null, context)

  override def matchesBeneathAnchor(
    node    : NodeInfo,
    anchor  : NodeInfo,
    context : XPathContext
  ): Boolean =
    internalMatches(node, anchor, context)

  private def internalMatches(node: NodeInfo,
                              anchor: NodeInfo,
                              context: XPathContext): Boolean = {
    if (! nodeTest.test(node))
      return false

    val c2   = context.newMinorContext()
    val iter = new ManualIterator(node)
    c2.setCurrentIterator(iter)
    try {
      var c: XPathContext = c2
      var actualPosition  = -1
      if (usesPosition) {
        actualPosition = getActualPosition(node,
          java.lang.Integer.MAX_VALUE,
          context.getCurrentIterator)
        val man = new ManualIterator(node, actualPosition)
        val c3  = c2.newMinorContext
        c3.setCurrentIterator(man)
        c = c3
      }
      val predicate = positionExpr.evaluateItem(c)
      if (predicate.isInstanceOf[NumericValue]) {
        val position         = positionExpr.evaluateItem(context).asInstanceOf[NumericValue]
        val requiredPos: Int = position.asSubscript()
        if (actualPosition < 0 && requiredPos != -1) {
          actualPosition =
            getActualPosition(node, requiredPos, context.getCurrentIterator)
        }
        requiredPos != -1 && actualPosition == requiredPos
      } else {
        ExpressionTool.effectiveBooleanValue(predicate)
      }
    } catch {
      case e @ (_: XPathException.Circularity | _: XPathException.StackOverflow) =>
        throw e
      case e: XPathException =>
        handleDynamicError(e, c2)
        false
    }
  }

  private def getActualPosition(
    node     : NodeInfo,
    max      : Int,
    iterator : FocusIterator
  ): Int =
    iterator match {
      case fti: FocusTrackingIterator =>
        fti.getSiblingPosition(node, nodeTest, max)
      case _                          =>
        Navigator.getSiblingPosition(node, nodeTest, max)
    }

  override def getUType: UType = nodeTest.getUType
  override def getFingerprint: Int = nodeTest.getFingerprint
  override def getItemType: ItemType = nodeTest

  override def equals(other: Any): Boolean = other match {
    case fp: GeneralPositionalPattern =>
      nodeTest == fp.nodeTest && positionExpr.isEqual(fp.positionExpr)
    case _ =>
      false
  }

  override def computeHashCode(): Int = nodeTest.hashCode ^ positionExpr.hashCode

  def copy(rebindings: RebindingMap): Pattern = {
    val n = new GeneralPositionalPattern(
      nodeTest.copy(),
      positionExpr.copy(rebindings)
    )
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  override def toString: String = nodeTest.toString + "[" + positionExpr.toString + "]"

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.genPos")
    presenter.emitAttribute("test", AlphaCode.fromItemType(nodeTest))
    if (! usesPosition)
      presenter.emitAttribute("flags", "P")
    positionExpr.export(presenter)
    presenter.endElement()
  }
}
