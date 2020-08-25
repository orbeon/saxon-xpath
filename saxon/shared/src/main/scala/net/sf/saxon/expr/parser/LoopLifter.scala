package net.sf.saxon.expr.parser

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.instruct.ConditionalInstruction

import net.sf.saxon.lib.Feature

import net.sf.saxon.lib.Logger

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.UType

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.SequenceType

import java.util.IdentityHashMap

import java.util.Map

import LoopLifter._

import scala.beans.{BeanProperty}

import scala.jdk.CollectionConverters._

import scala.util.control.Breaks._

object LoopLifter {

  def process(exp: Expression,
              visitor: ExpressionVisitor,
              contextInfo: ContextItemStaticInfo): Expression =
    if (exp.isInstanceOf[Literal] || exp.isInstanceOf[VariableReference]) {
      exp
    } else {
      val lifter: LoopLifter = new LoopLifter(exp,
        visitor.getConfiguration,
        visitor.isOptimizeForStreaming)
      val rsc: RetainedStaticContext = exp.getRetainedStaticContext
      lifter.gatherInfo(exp)
      lifter.loopLift(exp)
      lifter.root.setRetainedStaticContext(rsc)
      lifter.root.setParentExpression(null)
      if (lifter.changed) {
        ExpressionTool.resetPropertiesWithinSubtree(lifter.root)
        val e2: Expression = lifter.root.optimize(visitor, contextInfo)
        e2.setParentExpression(null)
        e2
      } else {
        lifter.root
      }
    }

  private class ExpInfo {

    var expression: Expression = _

    var loopLevel: Int = _

    var multiThreaded: Boolean = _

    var dependees: Map[Expression, Boolean] = new IdentityHashMap()

  }

}

class LoopLifter(@BeanProperty var root: Expression,
                 private var config: Configuration,
                 private var streaming: Boolean) {

  private var sequence: Int = 0

  private var changed: Boolean = false

  private var tracing: Boolean =
    config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)

  private var expInfoMap: Map[Expression, ExpInfo] = new IdentityHashMap()

  def gatherInfo(exp: Expression): Unit = {
    gatherInfo(exp, 0, 0, false)
  }

  private def gatherInfo(exp: Expression,
                         level: Int,
                         loopLevel: Int,
                         multiThreaded: Boolean): Unit = {
    val info: ExpInfo = new ExpInfo()
    info.expression = exp
    info.loopLevel = loopLevel
    info.multiThreaded = multiThreaded
    expInfoMap.put(exp, info)
    val scope: Expression = exp.getScopingExpression
    if (scope != null) {
      markDependencies(exp, scope)
    }
    val threaded: Boolean = multiThreaded || exp.isMultiThreaded(config)
    val choose: Expression = getContainingConditional(exp)
    if (choose != null) {
      markDependencies(exp, choose)
    }
    for (o <- exp.operands().asScala) {
      gatherInfo(o.getChildExpression,
        level + 1,
        if (o.isEvaluatedRepeatedly) loopLevel + 1 else loopLevel,
        threaded)
    }
  }

  private def getContainingConditional(exp: Expression): Expression = {
    var expressn = exp
    var parent: Expression = expressn.getParentExpression
    while (parent != null) {
      if (parent.isInstanceOf[ConditionalInstruction]) {
        val o: Operand = ExpressionTool.findOperand(parent, expressn)
        if (o == null) {
          throw new AssertionError()
        }
        if (o.getOperandRole.isInChoiceGroup) {
          return parent
        }
      } else if (parent.isInstanceOf[TryCatch]) {
        return parent
      }
      expressn = parent
      parent = parent.getParentExpression
    }
    null
  }

  private def mayReturnStreamedNodes(exp: Expression): Boolean =
    streaming &&
      exp.getItemType.getUType.intersection(UType.ANY_NODE) != UType.VOID

  private def markDependencies(exp: Expression,
                               variableSetter: Expression): Unit = {
    var parent: Expression = null
    if (variableSetter != null) {
      parent = exp
      while (parent != null && parent != variableSetter) {
        try expInfoMap.get(parent).dependees.put(variableSetter, true)
        catch {
          case e: NullPointerException => {
            e.printStackTrace()
            throw e
          }

        }
        parent = parent.getParentExpression
      }
    }
  }

  private def loopLift(exp: Expression): Unit = {
    val info: ExpInfo = expInfoMap.get(exp)
    if (!info.multiThreaded) {
      if (info.loopLevel > 0 && exp.getNetCost > 0) {
        if (info.dependees.isEmpty && exp.isLiftable(streaming) &&
          !mayReturnStreamedNodes(exp)) {
          root = lift(exp, root)
        } else {
          var child: Expression = exp
          val expInfo: ExpInfo = expInfoMap.get(exp)
          var parent: Expression = exp.getParentExpression
          breakable {
            while (parent != null) {
              if (expInfo.dependees.get(parent)) {
                val childInfo: ExpInfo = expInfoMap.get(child)
                if (expInfo.loopLevel != childInfo.loopLevel) {
                  val o: Operand = ExpressionTool.findOperand(parent, child)
                  assert(o != null)
                  if (exp.isLiftable(streaming) && !(child
                    .isInstanceOf[PseudoExpression]) &&
                    !o.getOperandRole.isConstrainedClass) {
                    val lifted: Expression = lift(exp, child)
                    o.setChildExpression(lifted)
                  }
                }
                break()
              }
              child = parent
              parent = parent.getParentExpression
            }
          }
        }
      }
      for (o <- exp.operands().asScala if !o.getOperandRole.isConstrainedClass) {
        loopLift(o.getChildExpression)
      }
    }
  }

  private def lift(child: Expression, newAction: Expression): Expression = {
    changed = true
    val childInfo: ExpInfo = expInfoMap.get(child)
    val actionInfo: ExpInfo = expInfoMap.get(newAction)
    val hoist: Int = childInfo.loopLevel - actionInfo.loopLevel
    val oldParent: Expression = child.getParentExpression
    val oldOperand: Operand = ExpressionTool.findOperand(oldParent, child)
    assert(oldOperand != null)
    val let: LetExpression = new LetExpression()
    let.setVariableQName(
      new StructuredQName("vv",
        NamespaceConstant.SAXON_GENERATED_VARIABLE,
        "v" + {
          sequence += 1;
          sequence - 1
        }))
    val `type`: SequenceType =
      SequenceType.makeSequenceType(child.getItemType, child.getCardinality)
    let.setRequiredType(`type`)
    ExpressionTool.copyLocationInfo(child, let)
    let.setSequence(child)
    let.setNeedsLazyEvaluation(true)
    let.setEvaluationMode(
      if (Cardinality.allowsMany(child.getCardinality))
        EvaluationMode.MAKE_MEMO_CLOSURE
      else EvaluationMode.MAKE_SINGLETON_CLOSURE)
    let.setAction(newAction)
    let.adoptChildExpression(newAction)
    val letInfo: ExpInfo = new ExpInfo()
    letInfo.expression = let
    letInfo.dependees = childInfo.dependees
    letInfo.dependees.putAll(actionInfo.dependees)
    letInfo.loopLevel = actionInfo.loopLevel
    expInfoMap.put(let, letInfo)
    try ExpressionTool.processExpressionTree(child,
      null,
      (expression, result) => {
        val info: ExpInfo =
          expInfoMap.get(expression)
        info.loopLevel -= hoist
        false
      })
    catch {
      case e: XPathException => e.printStackTrace()

    }
    val `var`: LocalVariableReference = new LocalVariableReference(let.asInstanceOf[StructuredQName])
    val properties: Int = child.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC
    `var`.setStaticType(`type`, null, properties)
    `var`.setInLoop(true)
    let.addReference(`var`, true)
    ExpressionTool.copyLocationInfo(child, `var`)
    oldOperand.setChildExpression(`var`)
    if (tracing) {
      val err: Logger = config.getLogger
      err.info(
        "OPT : At line " + child.getLocation.getLineNumber + " of " +
          child.getLocation.getSystemId)
      err.info(
        "OPT : Lifted (" + child.toShortString() + ") above (" +
          newAction.toShortString() +
          ") on line " +
          newAction.getLocation.getLineNumber)
      err.info("OPT : Expression after rewrite: " + let)
    }
    let
  }

}
