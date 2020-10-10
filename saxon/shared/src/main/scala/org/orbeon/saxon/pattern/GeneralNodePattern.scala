package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.functions.Current
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.AxisInfo
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.utils.Configuration

import scala.beans.BeanProperty

class GeneralNodePattern(@BeanProperty var equivalentExpr: Expression,
                         @BeanProperty var itemTyp: NodeTest) extends Pattern {

  override def getItemType: ItemType = itemTyp

  override def operands: java.lang.Iterable[Operand] =
    new Operand(this, equivalentExpr, OperandRole.SAME_FOCUS_ACTION)

  override def isMotionless(): Boolean = false

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    val cit: ContextItemStaticInfo =
      visitor.getConfiguration.getDefaultContextItemStaticInfo
    equivalentExpr = equivalentExpr.typeCheck(visitor, cit)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Pattern = {
    val config: Configuration = visitor.getConfiguration
    val defaultInfo: ContextItemStaticInfo =
      config.getDefaultContextItemStaticInfo
    equivalentExpr = equivalentExpr.optimize(visitor, defaultInfo)
    if (equivalentExpr.isInstanceOf[FilterExpression] &&
      !equivalentExpr.asInstanceOf[FilterExpression].isFilterIsPositional) {
      try PatternMaker
        .fromExpression(equivalentExpr, config, is30 = true)
        .typeCheck(visitor, defaultInfo)
      catch {
        case _: XPathException =>

      }
    }
    this
  }

  override def getDependencies(): Int =
    equivalentExpr.getDependencies &
      (StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | StaticProperty.DEPENDS_ON_USER_FUNCTIONS)

  override def bindCurrent(binding: LocalBinding): Unit = {
    if (ExpressionTool.callsFunction(equivalentExpr,
      Current.FN_CURRENT,
      sameFocusOnly = false)) {
      if (equivalentExpr.isCallOn(classOf[Current])) {
        equivalentExpr = new LocalVariableReference(binding)
      } else {
        Pattern.replaceCurrent(equivalentExpr, binding)
      }
    }
  }

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int =
    ExpressionTool.allocateSlots(equivalentExpr, nextFree, slotManager)

  def matches(item: Item, context: XPathContext): Boolean = {
    val th = context.getConfiguration.getTypeHierarchy
    if (!itemTyp.matches(item, th)) {
      return false
    }
    val anc: AxisIterator =
      item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.ANCESTOR_OR_SELF)
    while (true) {
      val a: NodeInfo = anc.next()
      if (a == null) {
        return false
      }
      if (matchesBeneathAnchor(item.asInstanceOf[NodeInfo], a, context)) {
        true
      }
    }
    false
  }

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean = {
    if (!itemTyp.test(node)) {
      return false
    }
    if (anchor == null) {
      val ancestors: AxisIterator = node.iterateAxis(AxisInfo.ANCESTOR_OR_SELF)
      while (true) {
        val ancestor: NodeInfo = ancestors.next()
        if (ancestor == null) {
          return false
        }
        if (matchesBeneathAnchor(node, ancestor, context)) {
          true
        }
      }
    }
    val c2: XPathContext = context.newMinorContext()
    val iter: ManualIterator = new ManualIterator(anchor)
    c2.setCurrentIterator(iter)
    try {
      val nsv: SequenceIterator = equivalentExpr.iterate(c2)
      while (true) {
        val n: NodeInfo = nsv.next().asInstanceOf[NodeInfo]
        if (n == null) {
          return false
        }
        if (n == node) {
          true
        }
      }
      false
    } catch {
      case e@(_: XPathException.Circularity |
              _: XPathException.StackOverflow) =>
        throw e

      case e: XPathException => {
        handleDynamicError(e, c2)
        false
      }

    }
  }

  override def getUType: UType = itemTyp.getUType

  override def getFingerprint: Int = itemTyp.getFingerprint

  override def equals(other: Any): Boolean = other match {
    case other: GeneralNodePattern => {
      val lpp: GeneralNodePattern = other
      equivalentExpr.isEqual(lpp.equivalentExpr)
    }
    case _ => false

  }

  override def computeHashCode(): Int = 83641 ^ equivalentExpr.hashCode

  def copy(rebindings: RebindingMap): Pattern = {
    val n: GeneralNodePattern =
      new GeneralNodePattern(equivalentExpr.copy(rebindings), itemTyp)
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.genNode")
    presenter.emitAttribute("test", AlphaCode.fromItemType(itemTyp))
    equivalentExpr.export(presenter)
    presenter.endElement()
  }

}
