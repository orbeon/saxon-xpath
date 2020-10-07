package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.lib.TraceListener

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trace.TraceableComponent

import java.util.HashMap

class ComponentTracer(private var component: TraceableComponent)
  extends Instruction {

  private var baseOp: Operand =
    new Operand(this, component.getBody, OperandRole.SAME_FOCUS_ACTION)

  private var properties: HashMap[String, Any] = new HashMap(10)

  adoptChildExpression(component.getBody)

  component.setBody(this)

  component.gatherProperties((k, v) => properties.put(k, v))

  def getChild: Expression = baseOp.getChildExpression

  def getBody: Expression = baseOp.getChildExpression

  override def operands: java.lang.Iterable[Operand] = baseOp

  override def getExpressionName: String = "trace"

  override def getStreamerName: String = "TraceExpr"

  def copy(rebindings: RebindingMap): Expression = {
    val t: ComponentTracer = new ComponentTracer(component)
    t.setLocation(getLocation)
    t
  }

  override def isUpdatingExpression(): Boolean = getChild.isUpdatingExpression

  override def isVacuousExpression(): Boolean = getChild.isVacuousExpression

  override def checkForUpdatingSubexpressions(): Unit = {
    getChild.checkForUpdatingSubexpressions()
  }

  override def getImplementationMethod: Int = getChild.getImplementationMethod

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val listener: TraceListener = controller.getTraceListener
    if (controller.isTracing) {
      assert(listener != null)
      listener.enter(component, properties, context)
      getChild.process(output, context)
      listener.leave(component)
    } else {
      getChild.process(output, context)
    }
    null
  }

override  def getItemType: ItemType = getChild.getItemType

  override def getCardinality(): Int = getChild.getCardinality

  override  def getDependencies(): Int = getChild.getDependencies

  override  def mayCreateNewNodes(): Boolean =
    !getChild.hasSpecialProperty(StaticProperty.NO_NODES_NEWLY_CREATED)

  override def getNetCost(): Int = 0

  override def evaluateItem(context: XPathContext): Item = {
    val controller: Controller = context.getController
    assert(controller != null)
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      listener.enter(component, properties, context)
      val result: Item = getChild.evaluateItem(context)
      listener.leave(component)
      result
    } else {
      getChild.evaluateItem(context)
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val controller: Controller = context.getController
    assert(controller != null)
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      listener.enter(component, properties, context)
      val result: SequenceIterator = getChild.iterate(context)
      listener.leave(component)
      result
    } else {
      getChild.iterate(context)
    }
  }

  override def getInstructionNameCode(): Int =
    if (getChild.isInstanceOf[Instruction]) {
      getChild.asInstanceOf[Instruction].getInstructionNameCode
    } else {
      -1
    }

  def export(out: ExpressionPresenter): Unit = {
    getChild.export(out)
  }

  override def evaluatePendingUpdates(context: XPathContext,
                             pul: PendingUpdateList): Unit = {
    val controller: Controller = context.getController
    assert(controller != null)
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      listener.enter(component, properties, context)
      getChild.evaluatePendingUpdates(context, pul)
      listener.leave(component)
    } else {
      getChild.evaluatePendingUpdates(context, pul)
    }
  }

  override def toShortString: String = getChild.toShortString

}
