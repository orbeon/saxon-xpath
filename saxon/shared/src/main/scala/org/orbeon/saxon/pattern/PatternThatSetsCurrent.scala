package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.flwor.LocalVariableBinding

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.functions.Current

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.UType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

import org.orbeon.saxon.s9api.HostLanguage._

class PatternThatSetsCurrent(@BeanProperty var wrappedPattern: Pattern,
                             private var binding: LocalVariableBinding)
  extends Pattern {

  binding.setRequiredType(
    SequenceType.makeSequenceType(wrappedPattern.getItemType,
      StaticProperty.EXACTLY_ONE))

  adoptChildExpression(wrappedPattern)

  this.priority = wrappedPattern.getDefaultPriority

  def this(wrappedPattern: Pattern) =
    this(
      wrappedPattern,
      new LocalVariableBinding(Current.FN_CURRENT, SequenceType.SINGLE_ITEM))

  override def operands: java.lang.Iterable[Operand] =
    new Operand(this, wrappedPattern, OperandRole.SINGLE_ATOMIC)

  def getCurrentBinding: LocalBinding = binding

  override def hasVariableBinding(binding: Binding): Boolean =
    binding == this.binding

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int = {
    var nxtFree = nextFree
    slotManager.allocateSlotNumber(Current.FN_CURRENT)
    binding.setSlotNumber({
      nxtFree += 1; nxtFree - 1
    })
    wrappedPattern.allocateSlots(slotManager, nextFree)
  }

  override def matches(item: Item, context: XPathContext): Boolean = {
    context.setLocalVariable(binding.getLocalSlotNumber, item)
    wrappedPattern.matches(item, context)
  }

  override def getItemType: ItemType = wrappedPattern.getItemType

  override def simplify(): Pattern = {
    wrappedPattern = wrappedPattern.simplify()
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    wrappedPattern = wrappedPattern.typeCheck(visitor, contextItemType)
    this
  }

  override def getUType: UType = wrappedPattern.getUType

  override def getFingerprint: Int = wrappedPattern.getFingerprint

  override def toString: String = wrappedPattern.toString

  override def getHostLanguage(): HostLanguage = wrappedPattern.getHostLanguage

  override def isMotionless(): Boolean = wrappedPattern.isMotionless

  override def matchesBeneathAnchor(node: NodeInfo,
                                    anchor: NodeInfo,
                                    context: XPathContext): Boolean =
    wrappedPattern.matchesBeneathAnchor(node, anchor, context)

  override def convertToTypedPattern(`val`: String): Pattern = {
    val w2: Pattern = wrappedPattern.convertToTypedPattern(`val`)
    if (w2 == wrappedPattern) {
      this
    } else {
      new PatternThatSetsCurrent(w2)
    }
  }

  def copy(rebindings: RebindingMap): Pattern = {
    val newCurrent: LocalVariableBinding =
      new LocalVariableBinding(Current.FN_CURRENT, SequenceType.SINGLE_ITEM)
    rebindings.put(binding, newCurrent)
    val n: PatternThatSetsCurrent =
      new PatternThatSetsCurrent(wrappedPattern.copy(rebindings), newCurrent)
    ExpressionTool.copyLocationInfo(this, n)
    n
  }

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.withCurrent")
    wrappedPattern.export(presenter)
    presenter.endElement()
  }

}
