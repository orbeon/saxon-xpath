package org.orbeon.saxon.expr

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr.instruct.UserFunction
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, Evaluator, ExpressionVisitor, RebindingMap}
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.XPathException
import TailCallLoop._

object TailCallLoop {

  trait TailCallInfo

  class TailCallComponent extends TailCallInfo {

    var component: Component = _

    var function: UserFunction = _

  }

   class TailCallFunction extends TailCallInfo {

    var function: UserFunction = _

  }

}

class TailCallLoop(var containingFunction: UserFunction, body: Expression)
  extends UnaryExpression(body) {

  def getContainingFunction: UserFunction = containingFunction

 override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    this
  }

  def getImplementationMethod: Int =
    getBaseExpression.getImplementationMethod

   override def getOperandRole(): OperandRole =
    OperandRole.SAME_FOCUS_ACTION

  def copy(rebindings: RebindingMap): Expression =
    throw new UnsupportedOperationException("TailCallLoop.copy()")

  override def iterate(context: XPathContext): SequenceIterator = {
    val cm: XPathContextMajor = context.asInstanceOf[XPathContextMajor]
    var result:SequenceIterator = null
    while (true) {
      val iter: SequenceIterator = getBaseExpression.iterate(cm)
      val extent: GroundedValue = iter.materialize()
      val tail: TailCallInfo = cm.getTailCallInfo
      if (tail == null) {
        result = extent.iterate()
      } else {
        val target: UserFunction = establishTargetFunction(tail, cm)
        if (target != containingFunction) {
          result = tailCallDifferentFunction(target, cm).iterate()
        }
      }
    }
    result
  }

 override def evaluateItem(context: XPathContext): Item = {
    val cm: XPathContextMajor = context.asInstanceOf[XPathContextMajor]
    var itemRes: Item = null
    while (true) {
      val item: Item = getBaseExpression.evaluateItem(context)
      val tail: TailCallInfo = cm.getTailCallInfo
      if (tail == null) {
        itemRes = item
      } else {
        val target: UserFunction = establishTargetFunction(tail, cm)
        if (target != containingFunction) {
          itemRes= tailCallDifferentFunction(target, cm).head
        }
      }
    }
    itemRes
  }

  private def establishTargetFunction(tail: TailCallInfo,
                                      cm: XPathContextMajor): UserFunction =
    if (tail.isInstanceOf[TailCallFunction]) {
      tail.asInstanceOf[TailCallFunction].function
    } else if (tail.isInstanceOf[TailCallComponent]) {
      val targetComponent: Component =
        tail.asInstanceOf[TailCallComponent].component
      cm.setCurrentComponent(targetComponent)
      targetComponent.getActor.asInstanceOf[UserFunction]
    } else {
      throw new AssertionError()
    }

 override def process(output: Outputter, context: XPathContext): Unit = {
    val cm: XPathContextMajor = context.asInstanceOf[XPathContextMajor]
    val operand: Expression = getBaseExpression
    while (true) {
      operand.process(output, context)
      val tail: TailCallInfo = cm.getTailCallInfo
      if (tail == null) {
        return
      } else {
        val target: UserFunction = establishTargetFunction(tail, cm)
        if (target != containingFunction) {
          SequenceTool.process(tailCallDifferentFunction(target, cm),
            output,
            operand.getLocation)
          return
        }
      }
    }
  }

  private def tailCallDifferentFunction(userFunction: UserFunction,
                                        cm: XPathContextMajor): Sequence = {
    cm.resetStackFrameMap(userFunction.getStackFrameMap, userFunction.getArity)
    try userFunction.getEvaluator.evaluate(userFunction.getBody, cm)
    catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        err.maybeSetContext(cm)
        throw err
      }

    }
  }

  override def getItemType: ItemType = getBaseExpression.getItemType

  override def getExpressionName: String = "tailCallLoop"

}
