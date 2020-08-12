package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.Arrays

import scala.jdk.CollectionConverters._

class LocalParamBlock(params: Array[LocalParam]) extends Instruction {

  var operanda: Array[Operand] = new Array[Operand](params.length)

  for (i <- 0 until params.length) {
    operanda(i) = new Operand(this, params(i), OperandRole.NAVIGATE)
  }

  override def getExpressionName(): String = "params"

  override def operands(): java.lang.Iterable[Operand] =
    Arrays.asList(operanda: _*)

  def getNumberOfParams(): Int = operanda.length

  override def computeSpecialProperties(): Int = 0

  def copy(rebindings: RebindingMap): Expression = {
    val lps2: Array[LocalParam] = Array.ofDim[LocalParam](getNumberOfParams)
    var i: Int = 0
    for (o <- operands().asScala) {
      val oldLps: LocalParam = o.getChildExpression.asInstanceOf[LocalParam]
      val newLps: LocalParam = oldLps.copy(rebindings)
      rebindings.put(oldLps, newLps)
      i += 1
      lps2(i) = newLps
    }
    new LocalParamBlock(lps2)
  }

  override def getItemType(): ItemType = ErrorType.getInstance

  override def getCardinality(): Int = StaticProperty.EMPTY

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("params", this)
    for (o <- operands().asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    for (o <- operands().asScala) {
      val param: LocalParam = o.getChildExpression.asInstanceOf[LocalParam]
      try context.setLocalVariable(param.getSlotNumber,
        param.getSelectValue(context))
      catch {
        case e: XPathException => {
          e.maybeSetLocation(param.getLocation)
          e.maybeSetContext(context)
          throw e
        }

      }
    }
    null
  }

  override def getImplementationMethod(): Int = Expression.PROCESS_METHOD

}
