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
import org.orbeon.saxon.trace.Traceable
import java.util.HashMap
import java.util.Iterator

import scala.collection
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
class TraceExpression(child: Expression) extends Instruction with Traceable {

  private var baseOp: Operand =
    new Operand(this, child, OperandRole.SAME_FOCUS_ACTION)

  private var properties: HashMap[String, Any] = new HashMap(10)

  adoptChildExpression(child)

  child.gatherProperties((k, v) => properties.put(k, v))

  def getChild: Expression = baseOp.getChildExpression

  def getBody: Expression = baseOp.getChildExpression

  override def operands: java.lang.Iterable[Operand] = baseOp

  def setProperty(name: String, value: AnyRef): Unit = {
    properties.put(name, value)
  }

  override def getProperty(name: String): AnyRef = properties.get(name).asInstanceOf[AnyRef]

  override def getProperties: collection.Iterator[String] = properties.keySet.iterator.asScala

  override def getExpressionName: String = "trace"

  override def getStreamerName: String = "TraceExpr"

  def copy(rebindings: RebindingMap): Expression = {
    val t: TraceExpression = new TraceExpression(getChild.copy(rebindings))
    t.setLocation(getLocation)
    t.properties = properties
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
    val child = getChild
    if (controller.isTracing) {
      assert(listener != null)
      listener.enter(child, properties, context)
      child.process(output, context)
      listener.leave(child)
    } else {
      child.process(output, context)
    }
    null
  }

  override def getItemType: ItemType = getChild.getItemType

  override def getCardinality(): Int = getChild.getCardinality

  override def getDependencies(): Int = getChild.getDependencies

  override def mayCreateNewNodes(): Boolean =
    !getChild.hasSpecialProperty(StaticProperty.NO_NODES_NEWLY_CREATED)

  override def getNetCost(): Int = 0

  override def evaluateItem(context: XPathContext): Item = {
    val controller: Controller = context.getController
    assert(controller != null)
    val child = getChild
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      listener.enter(child, properties, context)
      val result: Item = child.evaluateItem(context)
      listener.leave(child)
      result
    } else {
      child.evaluateItem(context)
    }
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val controller: Controller = context.getController
    assert(controller != null)
    val child = getChild
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      listener.enter(child, properties, context)
      val result: SequenceIterator = child.iterate(context)
      listener.leave(child)
      result
    } else {
      child.iterate(context)
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
      listener.enter(getChild, properties, context)
      getChild.evaluatePendingUpdates(context, pul)
      listener.leave(getChild)
    } else {
      getChild.evaluatePendingUpdates(context, pul)
    }
  }

  override def toShortString: String = getChild.toShortString

}
