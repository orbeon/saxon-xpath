package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.lib.TraceListener

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.FocusIterator

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import IterateInstr._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object IterateInstr {

  private def containsBreakOrNextIterationWithinTryCatch(
                                                          exp: Expression,
                                                          withinTryCatch: Boolean): Boolean =
    if (exp.isInstanceOf[BreakInstr] || exp.isInstanceOf[NextIteration]) {
      withinTryCatch
    } else {
      var found: Boolean = false
      val inTryCatch: Boolean = withinTryCatch || exp.isInstanceOf[TryCatch]
      breakable {
        for (o <- exp.operands.asScala
             if containsBreakOrNextIterationWithinTryCatch(o.getChildExpression,
               inTryCatch)) {
          found = true
          break()
        }
      }
      found
    }

}

class IterateInstr(select: Expression,
                   initiallyExp: LocalParamBlock,
                   action: Expression,
                   var onCompletion: Expression)
  extends Instruction
    with ContextSwitchingExpression {

  private var selectOp: Operand =
    new Operand(this, select, OperandRole.FOCUS_CONTROLLING_SELECT)

  private var actionOp: Operand =
    new Operand(this, action, OperandRole.FOCUS_CONTROLLED_ACTION)

  private var initiallyOp: Operand = new Operand(
    this,
    initiallyExp,
    new OperandRole(OperandRole.CONSTRAINED_CLASS,
      OperandUsage.NAVIGATION,
      SequenceType.ANY_SEQUENCE))

  private var onCompletionOp: Operand = new Operand(
    this,
    onCompletion,
    new OperandRole(OperandRole.USES_NEW_FOCUS, OperandUsage.TRANSMISSION))

  if (onCompletion == null) {
    onCompletion = Literal.makeEmptySequence()
  }

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  def getInitiallyExp: LocalParamBlock =
    initiallyOp.getChildExpression.asInstanceOf[LocalParamBlock]

  def setInitiallyExp(initiallyExp: LocalParamBlock): Unit = {
    initiallyOp.setChildExpression(initiallyExp)
  }

  def setAction(action: Expression): Unit = {
    actionOp.setChildExpression(action)
  }

  def getOnCompletion: Expression = onCompletionOp.getChildExpression

  def setOnCompletion(onCompletion: Expression): Unit = {
    onCompletionOp.setChildExpression(onCompletion)
  }

  override def operands: java.lang.Iterable[Operand] =
    operandList(selectOp, actionOp, initiallyOp, onCompletionOp)

  override def getInstructionNameCode(): Int = StandardNames.XSL_ITERATE

  def getSelectExpression(): Expression = selectOp.getChildExpression

  def getActionExpression(): Expression = actionOp.getChildExpression

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.typeCheck(visitor, contextInfo)
    initiallyOp.typeCheck(visitor, contextInfo)
    var selectType: ItemType = getSelectExpression.getItemType
    if (selectType == ErrorType) {
      selectType = AnyItemType
    }
    val cit: ContextItemStaticInfo =
      visitor.getConfiguration.makeContextItemStaticInfo(selectType, maybeUndefined = false)
    cit.setContextSettingExpression(getSelectExpression)
    actionOp.typeCheck(visitor, cit)
    onCompletionOp.typeCheck(visitor, ContextItemStaticInfo.ABSENT)
    if (Literal.isEmptySequence(getOnCompletion)) {
      if (Literal.isEmptySequence(getSelectExpression) || Literal
        .isEmptySequence(getActionExpression)) {
        getOnCompletion
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.optimize(visitor, contextInfo)
    initiallyOp.optimize(visitor, contextInfo)
    val cit2: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(getSelectExpression.getItemType, maybeUndefined = false)
    cit2.setContextSettingExpression(getSelectExpression)
    actionOp.optimize(visitor, cit2)
    onCompletionOp.optimize(visitor, ContextItemStaticInfo.ABSENT)
    if (Literal.isEmptySequence(getOnCompletion)) {
      if (Literal.isEmptySequence(getSelectExpression) || Literal
        .isEmptySequence(getActionExpression)) {
        getOnCompletion
      }
    }
    this
  }

  def isCompilable: Boolean =
    !containsBreakOrNextIterationWithinTryCatch(this, withinTryCatch = false)

  override def getItemType: ItemType =
    if (Literal.isEmptySequence(getOnCompletion)) {
      getActionExpression.getItemType
    } else {
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      Type.getCommonSuperType(getActionExpression.getItemType,
        getOnCompletion.getItemType,
        th)
    }

  override def mayCreateNewNodes(): Boolean =
    (getActionExpression.getSpecialProperties & getOnCompletion.getSpecialProperties &
      StaticProperty.NO_NODES_NEWLY_CREATED) ==
      0

  override def hasVariableBinding(binding: Binding): Boolean = {
    val paramBlock: LocalParamBlock = getInitiallyExp
    for (o <- paramBlock.operands.asScala) {
      val setter: LocalParam = o.getChildExpression.asInstanceOf[LocalParam]
      if (setter == binding) {
        true
      }
    }
    false
  }

  override def getStreamerName: String = "Iterate"

  override def getImplementationMethod: Int = Expression.PROCESS_METHOD

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getActionExpression.checkPermittedContents(parentType, whole = false)
    getOnCompletion.checkPermittedContents(parentType, whole = false)
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: IterateInstr = new IterateInstr(
      getSelectExpression.copy(rebindings),
      getInitiallyExp.copy(rebindings).asInstanceOf[LocalParamBlock],
      getActionExpression.copy(rebindings),
      getOnCompletion.copy(rebindings)
    )
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    val iter: FocusIterator =
      c2.trackFocus(getSelectExpression.iterate(context))
    c2.setCurrentTemplateRule(null)
    val pipe: PipelineConfiguration = output.getPipelineConfiguration
    pipe.setXPathContext(c2)
    val tracing: Boolean = context.getController.isTracing
    val listener: TraceListener =
      if (tracing) context.getController.getTraceListener else null
    getInitiallyExp.process(output, context)
    breakable {
      while (true) {
        val item: Item = iter.next()
        if (item != null) {
          if (tracing) {
            listener.startCurrentItem(item)
          }
          getActionExpression.process(output, c2)
          if (tracing) {
            listener.endCurrentItem(item)
          }
          val comp: TailCallLoop.TailCallInfo = c2.getTailCallInfo
          if (comp == null) {} else if (comp.isInstanceOf[BreakInstr]) {
            iter.close()
            null
          } else {}
        } else {
          val c3: XPathContextMinor = context.newMinorContext()
          c3.setCurrentIterator(null)
          getOnCompletion.process(output, c3)
          break()
        }
      }
    }
    pipe.setXPathContext(context)
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("iterate", this)
    out.setChildRole("select")
    getSelectExpression.export(out)
    out.setChildRole("params")
    getInitiallyExp.export(out)
    if (!Literal.isEmptySequence(getOnCompletion)) {
      out.setChildRole("on-completion")
      getOnCompletion.export(out)
    }
    out.setChildRole("action")
    getActionExpression.export(out)
    out.endElement()
  }

}
