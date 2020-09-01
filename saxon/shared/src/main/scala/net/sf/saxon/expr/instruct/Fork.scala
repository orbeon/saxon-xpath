package net.sf.saxon.expr.instruct

import java.util.Arrays

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.{Expression, Operand, OperandRole, XPathContext}
import net.sf.saxon.expr.parser.{ExpressionTool, RebindingMap}
import net.sf.saxon.model.{AnyItemType, ErrorType, ItemType, Type}
import net.sf.saxon.om.StandardNames
import net.sf.saxon.trace.ExpressionPresenter

import scala.jdk.CollectionConverters._

class Fork extends Instruction {

  var operanda: Array[Operand] = _

  def this(prongs: Array[Operand]) {
    this()
    operanda = new Array[Operand](prongs.length)
    for (i <- prongs.indices) {
      operanda(i) = new Operand(this,
        prongs(i).getChildExpression,
        OperandRole.SAME_FOCUS_ACTION)
    }
  }


  def this(prong: Array[Expression]) = {
    this()
    operanda = Array.ofDim[Operand](prong.length)
    for (i <- prong.indices)
      operanda(i) = new Operand(this, prong(i), OperandRole.SAME_FOCUS_ACTION)
  }

  override def operands(): java.lang.Iterable[Operand] =
    Arrays.asList(operanda: _*)

  override def getInstructionNameCode(): Int = StandardNames.XSL_FORK

  def getSize(): Int = operanda.length

  def getProng(i: Int): Expression = operanda(i).getChildExpression

  override def getItemType(): ItemType = {

    if (getSize == 0)
      return ErrorType

    var t1: ItemType = null
    for (o <- operands().asScala) {
      val t2 = o.getChildExpression.getItemType
      t1 = if (t1 == null) t2 else Type.getCommonSuperType(t1, t2)
      if (t1 eq AnyItemType)
        return t1
    }
    t1
  }

  override def getStreamerName(): String = "Fork"

  def copy(rebindings: RebindingMap): Expression = {
    val e2 = Array.ofDim[Expression](getSize)
    var i = 0
    for (o <- operands().asScala) {
      i += 1
      e2(i) = o.getChildExpression.copy(rebindings)
    }
    val f2 = new Fork(e2)
    ExpressionTool.copyLocationInfo(this, f2)
    f2
  }

  override def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    for (o <- operands().asScala)
      o.getChildExpression.process(output, context)
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("fork", this)
    for (o <- operands().asScala)
      o.getChildExpression.export(out)
    out.endElement()
  }
}
