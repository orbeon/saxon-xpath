package net.sf.saxon.pattern

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.model.UType

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}

class AncestorQualifiedPattern(base: Pattern, upper: Pattern, axis: Int)
  extends Pattern {

  @BeanProperty
  var basePattern: Pattern = base

  @BeanProperty
  var upperPattern: Pattern = upper

  @BeanProperty
  var upwardsAxis: Int = axis

  private var refinedItemType: ItemType = _

  private var testUpperPatternFirst: Boolean = false

  adoptChildExpression(base)

  adoptChildExpression(upper)

  override def operands: java.lang.Iterable[Operand] =
    operandList(new Operand(this, upperPattern, OperandRole.SAME_FOCUS_ACTION),
      new Operand(this, basePattern, OperandRole.SAME_FOCUS_ACTION))

  override def bindCurrent(binding: LocalBinding): Unit = {
    basePattern.bindCurrent(binding)
    upperPattern.bindCurrent(binding)
  }

  override def isMotionless(): Boolean =
    basePattern.isMotionless && upperPattern.isMotionless

  override def matchesCurrentGroup(): Boolean =
    upperPattern.matchesCurrentGroup()

  override def simplify(): Pattern = {
    upperPattern = upperPattern.simplify()
    basePattern = basePattern.simplify()
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    basePattern = basePattern.typeCheck(visitor, contextItemType)
    upperPattern = upperPattern.typeCheck(visitor, contextItemType)
    if (upwardsAxis == AxisInfo.PARENT) {
      val `type`: ItemType = basePattern.getItemType
      if (`type`.isInstanceOf[NodeTest]) {
        var step: AxisExpression = null
        step =
          if (`type`.getPrimitiveType == Type.ATTRIBUTE)
            new AxisExpression(AxisInfo.ATTRIBUTE,
              `type`.asInstanceOf[NodeTest])
          else
            new AxisExpression(AxisInfo.CHILD, `type`.asInstanceOf[NodeTest])
        ExpressionTool.copyLocationInfo(this, step)
        val exp: Expression = step.typeCheck(
          visitor,
          visitor.getConfiguration
            .makeContextItemStaticInfo(upperPattern.getItemType, maybeUndefined = false))
        refinedItemType = exp.getItemType
      }
    }
    testUpperPatternFirst = upperPattern.getCost < basePattern.getCost
    this
  }

  override def getDependencies(): Int =
    basePattern.getDependencies | upperPattern.getDependencies

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int = {
    var nxtFree = nextFree
    nxtFree = upperPattern.allocateSlots(slotManager, nxtFree)
    nxtFree = basePattern.allocateSlots(slotManager, nxtFree)
    nxtFree
  }

  def matches(item: Item, context: XPathContext): Boolean =
    item.isInstanceOf[NodeInfo] &&
      matchesBeneathAnchor(item.asInstanceOf[NodeInfo], null, context)

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    if (testUpperPatternFirst) {
      matchesUpperPattern(node, anchor, context) && basePattern.matches(
        node,
        context)
    } else {
      basePattern.matchesBeneathAnchor(node, anchor, context) &&
        matchesUpperPattern(node, anchor, context)
    }

  private def matchesUpperPattern(node: NodeInfo,
                                  anchor: NodeInfo,
                                  context: XPathContext): Boolean =
    upwardsAxis match {
      case AxisInfo.SELF =>
        upperPattern.matchesBeneathAnchor(node, anchor, context)
      case AxisInfo.PARENT =>
        var par: NodeInfo = node.getParent
        par != null &&
          upperPattern.matchesBeneathAnchor(par, anchor, context)
      case AxisInfo.ANCESTOR => {
        val anc: NodeInfo = node.getParent
        hasMatchingAncestor(anchor, anc, context)
      }
      case AxisInfo.ANCESTOR_OR_SELF =>
        hasMatchingAncestor(anchor, node, context)
      case _ =>
        throw new XPathException(
          "Unsupported axis " + AxisInfo.axisName(upwardsAxis) +
            " in pattern")

    }

  private def hasMatchingAncestor(anchor: NodeInfo,
                                  anc: NodeInfo,
                                  context: XPathContext): Boolean = {
    var ancNodeInfo = anc
    while (ancNodeInfo != null) {
      if (upperPattern.matchesBeneathAnchor(ancNodeInfo, anchor, context)) {
        return true
      }
      if (ancNodeInfo == anchor) {
        return false
      }
      ancNodeInfo = ancNodeInfo.getParent
    }
    false
  }

  override def getUType: UType = basePattern.getUType

  override def getFingerprint: Int = basePattern.getFingerprint

  override def getItemType: ItemType = {
    if (refinedItemType != null) {
      return refinedItemType
    }
    basePattern.getItemType
  }

  override def convertToTypedPattern(`val`: String): Pattern =
    if (upperPattern.getUType == UType.DOCUMENT) {
      val b2: Pattern = basePattern.convertToTypedPattern(`val`)
      if (b2 == basePattern) {
        this
      } else {
        new AncestorQualifiedPattern(b2, upperPattern, upwardsAxis)
      }
    } else {
      val u2: Pattern = upperPattern.convertToTypedPattern(`val`)
      if (u2 == upperPattern) {
        this
      } else {
        new AncestorQualifiedPattern(basePattern, u2, upwardsAxis)
      }
    }

  override def equals(other: Any): Boolean = other match {
    case other: AncestorQualifiedPattern => {
      val aqp: AncestorQualifiedPattern = other
      basePattern.isEqual(aqp.basePattern) && upperPattern.isEqual(
        aqp.upperPattern) &&
        upwardsAxis == aqp.upwardsAxis
    }
    case _ => false

  }

  override def computeHashCode(): Int =
    88267 ^ basePattern.hashCode ^ upperPattern.hashCode ^
      (upwardsAxis << 22)

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.withUpper")
    presenter.emitAttribute("axis", AxisInfo.axisName(getUpwardsAxis))
    presenter.emitAttribute("upFirst", "" + testUpperPatternFirst)
    basePattern.export(presenter)
    upperPattern.export(presenter)
    presenter.endElement()
  }

  override def toString: String = {
    val res = if (upwardsAxis == AxisInfo.PARENT) "/" else ""
    upperPattern.toString + res
  }

  def copy(rebindings: RebindingMap): AncestorQualifiedPattern = {
    val n = new AncestorQualifiedPattern(
      basePattern.copy(rebindings),
      upperPattern.copy(rebindings),
      upwardsAxis)
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

}
