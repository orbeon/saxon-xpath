package net.sf.saxon.expr.instruct

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}

object WithParam {

  var EMPTY_ARRAY: Array[WithParam] = new Array[WithParam](0)

  def simplify(params: Array[WithParam]): Unit = {
    if (params != null) {
      for (param <- params) {
        param.selectOp.setChildExpression(
          param.selectOp.getChildExpression.simplify())
      }
    }
  }

  def typeCheck(params: Array[WithParam],
                visitor: ExpressionVisitor,
                contextItemType: ContextItemStaticInfo): Unit = {
    if (params != null) {
      for (param <- params) {
        param.selectOp.typeCheck(visitor, contextItemType)
      }
    }
  }

  def optimize(visitor: ExpressionVisitor,
               params: Array[WithParam],
               contextItemType: ContextItemStaticInfo): Unit = {
    if (params != null) {
      for (param <- params) {
        param.selectOp.optimize(visitor, contextItemType)
        param.computeEvaluator()
      }
    }
  }

  def copy(parent: Expression,
           params: Array[WithParam],
           rebindings: RebindingMap): Array[WithParam] = {
    if (params == null) {
      null
    }
    val result: Array[WithParam] = Array.ofDim[WithParam](params.length)
    for (i <- 0 until params.length) {
      result(i) = new WithParam()
      result(i).slotNumber = params(i).slotNumber
      result(i).typeChecked = params(i).typeChecked
      result(i).selectOp = new Operand(
        parent,
        params(i).selectOp.getChildExpression.copy(rebindings),
        OperandRole.NAVIGATE)
      result(i).requiredType = params(i).requiredType
      result(i).variableQName = params(i).variableQName
    }
    result
  }

  def gatherOperands(parent: Expression,
                     params: Array[WithParam],
                     list: List[Operand]): Unit = {
    if (params != null) {
      for (param <- params) {
        list.add(param.selectOp)
      }
    }
  }

  def exportParameters(params: Array[WithParam],
                       out: ExpressionPresenter,
                       tunnel: Boolean): Unit = {
    if (params != null) {
      for (param <- params) {
        out.startElement("withParam")
        out.emitAttribute("name", param.variableQName)
        var flags: String = ""
        if (tunnel) {
          flags += "t"
        }
        if (param.isTypeChecked) {
          flags += "c"
        }
        if (!flags.isEmpty) {
          out.emitAttribute("flags", flags)
        }
        val options: ExpressionPresenter.ExportOptions =
          out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
        if (param.getRequiredType != SequenceType.ANY_SEQUENCE) {
          out.emitAttribute("as", param.getRequiredType.toAlphaCode)
        }
        if (param.getSlotNumber != -1) {
          out.emitAttribute("slot", param.getSlotNumber.toString)
        }
        param.selectOp.getChildExpression.export(out)
        out.endElement()
      }
    }
  }

}

class WithParam {

  private var selectOp: Operand = _

  @BooleanBeanProperty
  var typeChecked: Boolean = false

  @BeanProperty
  var slotNumber: Int = -1

  @BeanProperty
  var requiredType: SequenceType = _

  @BeanProperty
  var variableQName: StructuredQName = _

  private var evaluator: Evaluator = null

  def setSelectExpression(parent: Expression, select: Expression): Unit = {
    selectOp = new Operand(parent, select, OperandRole.NAVIGATE)
  }

  def getSelectOperand(): Operand = selectOp

  def getSelectExpression(): Expression = selectOp.getChildExpression

  def getInstructionNameCode(): Int = StandardNames.XSL_WITH_PARAM

  def getEvaluationMode(): EvaluationMode.EvaluationMode = {
    if (evaluator == null) {
      computeEvaluator()
    }
    evaluator.getEvaluationMode
  }

  private def computeEvaluator(): Unit = {
    evaluator = ExpressionTool.lazyEvaluator(selectOp.getChildExpression, true)
  }

  def getSelectValue(context: XPathContext): Sequence = {
    if (evaluator == null) {
      computeEvaluator()
    }
    val savedOutputState: Int = context.getTemporaryOutputState
    context.setTemporaryOutputState(StandardNames.XSL_WITH_PARAM)
    val result: Sequence =
      evaluator.evaluate(selectOp.getChildExpression, context)
    context.setTemporaryOutputState(savedOutputState)
    result
  }

}
