package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr.LocalBinding

import org.orbeon.saxon.expr.Operand

import org.orbeon.saxon.expr.OperandRole

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import java.util.HashSet

import java.util.Set

abstract class VennPattern( var p1: Pattern,
                            var p2: Pattern)
  extends Pattern {

  adoptChildExpression(p1)

  adoptChildExpression(p2)

  override def operands: java.lang.Iterable[Operand] =
    operandList(new Operand(this, p1, OperandRole.SAME_FOCUS_ACTION),
      new Operand(this, p2, OperandRole.SAME_FOCUS_ACTION))

  override def simplify(): Pattern = {
    p1 = p1.simplify()
    p2 = p2.simplify()
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextItemType: ContextItemStaticInfo): Pattern = {
    mustBeNodePattern(p1)
    p1 = p1.typeCheck(visitor, contextItemType)
    mustBeNodePattern(p2)
    p2 = p2.typeCheck(visitor, contextItemType)
    this
  }

  private def mustBeNodePattern(p: Pattern): Unit = {
    if (p.isInstanceOf[NodeTestPattern]) {
      val it: ItemType = p.getItemType
      if (!(it.isInstanceOf[NodeTest])) {
        val err = new XPathException(
          "The operands of a union, intersect, or except pattern " +
            "must be patterns that match nodes",
          "XPTY0004")
        err.setIsTypeError(true)
        throw err
      }
    }
  }

  override def bindCurrent(binding: LocalBinding): Unit = {
    p1.bindCurrent(binding)
    p2.bindCurrent(binding)
  }

  override def isMotionless(): Boolean = p1.isMotionless && p2.isMotionless

  override def allocateSlots(slotManager: SlotManager, nextFree: Int): Int = {
    var nxtFree = nextFree
    nxtFree = p1.allocateSlots(slotManager, nxtFree)
    nxtFree = p2.allocateSlots(slotManager, nxtFree)
    nxtFree
  }

  def gatherComponentPatterns(set: Set[Pattern]): Unit = {
    if (p1.isInstanceOf[VennPattern]) {
      p1.asInstanceOf[VennPattern].gatherComponentPatterns(set)
    } else {
      set.add(p1)
    }
    if (p2.isInstanceOf[VennPattern]) {
      p2.asInstanceOf[VennPattern].gatherComponentPatterns(set)
    } else {
      set.add(p2)
    }
  }

  override def getDependencies(): Int = p1.getDependencies | p2.getDependencies

  def getLHS: Pattern = p1

  def getRHS: Pattern = p2

  override def matchesCurrentGroup(): Boolean =
    p1.matchesCurrentGroup() || p2.matchesCurrentGroup()

  override def equals(other: Any): Boolean = other match {
    case other: VennPattern => {
      val s0: Set[Pattern] = new HashSet[Pattern](10)
      gatherComponentPatterns(s0)
      val s1: Set[Pattern] = new HashSet[Pattern](10)
      other.gatherComponentPatterns(s1)
      s0 == s1
    }
    case _ => false

  }

  override def computeHashCode(): Int = 0x9bd723a6 ^ p1.hashCode ^ p2.hashCode

   def getOperatorName: String

  override def toString: String = p1.toString + " " + getOperatorName + " " + p2.toString

  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("p.venn")
    presenter.emitAttribute("op", getOperatorName)
    p1.export(presenter)
    p2.export(presenter)
    presenter.endElement()
  }

}
