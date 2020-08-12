package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Operand

import net.sf.saxon.expr.OperandRole

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trace.ExpressionPresenter

import scala.jdk.CollectionConverters._

import java.util.Arrays

class Fork extends Instruction {

  var operanda: Array[Operand] = _

  def this(prongs: Array[Operand]) {
    this()
    operanda = new Array[Operand](prongs.length)
    for (i <- 0 until prongs.length) {
      operanda(i) = new Operand(this,
        prongs(i).getChildExpression,
        OperandRole.SAME_FOCUS_ACTION)
    }
  }


  def this(prong: Array[Expression]) = {
    this()
    operanda = Array.ofDim[Operand](prong.length)
    for (i <- 0 until prong.length) {
      operanda(i) = new Operand(this, prong(i), OperandRole.SAME_FOCUS_ACTION)
    }
  }

  override def operands(): java.lang.Iterable[Operand] =
    Arrays.asList(operanda: _*)

  override def getInstructionNameCode(): Int = StandardNames.XSL_FORK

  def getSize(): Int = operanda.length

  def getProng(i: Int): Expression = operanda(i).getChildExpression

  override def getItemType(): ItemType = {
    if (getSize == 0) {
      ErrorType.getInstance
    }
    var t1: ItemType = null
    for (o <- operands().asScala) {
      val t2: ItemType = o.getChildExpression.getItemType
      t1 = if (t1 == null) t2 else Type.getCommonSuperType(t1, t2)
      if (t1.isInstanceOf[AnyItemType]) {
        t1
      }
    }
    t1
  }

  override def getStreamerName(): String = "Fork"

  def copy(rebindings: RebindingMap): Expression = {
    val e2: Array[Expression] = Array.ofDim[Expression](getSize)
    var i: Int = 0
    for (o <- operands().asScala) {
      i += 1
      e2(i) = o.getChildExpression.copy(rebindings)
    }
    val f2: Fork = new Fork(e2)
    ExpressionTool.copyLocationInfo(this, f2)
    f2
  }

  override def processLeavingTail(output: Outputter,
                                  context: XPathContext): TailCall = {
    for (o <- operands().asScala) {
      o.getChildExpression.process(output, context)
    }
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("fork", this)
    for (o <- operands().asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

}
