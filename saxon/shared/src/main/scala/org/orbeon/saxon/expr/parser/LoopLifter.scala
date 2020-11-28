package org.orbeon.saxon.expr.parser

import java.util.{IdentityHashMap, Map}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.ConditionalInstruction
import org.orbeon.saxon.expr.parser.LoopLifter._
import org.orbeon.saxon.lib.{Feature, Logger, NamespaceConstant}
import org.orbeon.saxon.model.UType
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{Cardinality, SequenceType}

import scala.beans.BeanProperty

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


object LoopLifter {

  def process(exp: Expression,
              visitor: ExpressionVisitor,
              contextInfo: ContextItemStaticInfo): Expression =
    if (exp.isInstanceOf[Literal] || exp.isInstanceOf[VariableReference]) {
      exp
    } else {
      val lifter = new LoopLifter(exp,
        visitor.getConfiguration,
        visitor.isOptimizeForStreaming)
      val rsc = exp.getRetainedStaticContext
      lifter.gatherInfo(exp)
      lifter.loopLift(exp)
      lifter.root.setRetainedStaticContext(rsc)
      lifter.root.setParentExpression(null)
      if (lifter.changed) {
        ExpressionTool.resetPropertiesWithinSubtree(lifter.root)
        val e2 = lifter.root.optimize(visitor, contextInfo)
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

  private val tracing: Boolean =
    config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)

  private val expInfoMap: Map[Expression, ExpInfo] = new IdentityHashMap()

  def gatherInfo(exp: Expression): Unit =
    gatherInfo(exp, 0, 0, multiThreaded = false)

  private def gatherInfo(exp: Expression,
                         level: Int,
                         loopLevel: Int,
                         multiThreaded: Boolean): Unit = {
    val info = new ExpInfo
    info.expression = exp
    info.loopLevel = loopLevel
    info.multiThreaded = multiThreaded
    expInfoMap.put(exp, info)
    val scope = exp.getScopingExpression
    if (scope != null)
      markDependencies(exp, scope)
    val threaded = multiThreaded || exp.isMultiThreaded(config)
    val choose = getContainingConditional(exp)
    if (choose != null)
      markDependencies(exp, choose)
    for (o <- exp.operands.asScala) {
      gatherInfo(o.getChildExpression,
        level + 1,
        if (o.isEvaluatedRepeatedly) loopLevel + 1 else loopLevel,
        threaded)
    }
  }

  private def getContainingConditional(exp: Expression): Expression = {
    var expressn = exp
    var parent = expressn.getParentExpression
    while (parent != null) {
      parent match {
        case _: ConditionalInstruction =>
          val o: Operand = ExpressionTool.findOperand(parent, expressn)
          if (o == null)
            throw new AssertionError()
          if (o.getOperandRole.isInChoiceGroup)
            return parent
        case _: TryCatch =>
          return parent
        case _ =>
      }
      expressn = parent
      parent = parent.getParentExpression
    }
    null
  }

  private def mayReturnStreamedNodes(exp: Expression): Boolean =
    streaming && exp.getItemType.getUType.intersection(UType.ANY_NODE) != UType.VOID

  private def markDependencies(exp: Expression,
                               variableSetter: Expression): Unit = {
    var parent: Expression = null
    if (variableSetter != null) {
      parent = exp
      while (parent != null && parent != variableSetter) {
        try
          expInfoMap.get(parent).dependees.put(variableSetter, true)
        catch {
          case e: NullPointerException =>
            e.printStackTrace()
            throw e
        }
        parent = parent.getParentExpression
      }
    }
  }

  private def loopLift(exp: Expression): Unit = {
    val info = expInfoMap.get(exp)
    if (!info.multiThreaded) {
      if (info.loopLevel > 0 && exp.getNetCost > 0) {
        if (info.dependees.isEmpty && exp.isLiftable(streaming) &&
          !mayReturnStreamedNodes(exp)) {
          root = lift(exp, root)
        } else {
          var child = exp
          val expInfo = expInfoMap.get(exp)
          var parent = exp.getParentExpression
          breakable {
            while (parent != null) {
              if (expInfo.dependees.get(parent)) {
                val childInfo = expInfoMap.get(child)
                if (expInfo.loopLevel != childInfo.loopLevel) {
                  val o = ExpressionTool.findOperand(parent, child)
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
      for (o <- exp.operands.asScala if !o.getOperandRole.isConstrainedClass)
        loopLift(o.getChildExpression)
    }
  }

  private def lift(child: Expression, newAction: Expression): Expression = {
    changed = true
    val childInfo = expInfoMap.get(child)
    val actionInfo = expInfoMap.get(newAction)
    val hoist = childInfo.loopLevel - actionInfo.loopLevel
    val oldParent = child.getParentExpression
    val oldOperand = ExpressionTool.findOperand(oldParent, child)
    assert(oldOperand != null)
    val let = new LetExpression
    let.setVariableQName(
      new StructuredQName("vv",
        NamespaceConstant.SAXON_GENERATED_VARIABLE,
        "v" + {
          sequence += 1;
          sequence - 1
        }))
    val `type` = SequenceType.makeSequenceType(child.getItemType, child.getCardinality)
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
    val letInfo = new ExpInfo
    letInfo.expression = let
    letInfo.dependees = childInfo.dependees
    letInfo.dependees.putAll(actionInfo.dependees)
    letInfo.loopLevel = actionInfo.loopLevel
    expInfoMap.put(let, letInfo)
    try
      ExpressionTool.processExpressionTree(child,
        null,
        (expression, result) => {
          val info: ExpInfo =
            expInfoMap.get(expression)
          info.loopLevel -= hoist
          false
        }
      )
    catch {
      case e: XPathException =>
        e.printStackTrace()
    }
    val `var` = new LocalVariableReference(let.asInstanceOf[StructuredQName])
    val properties = child.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC
    `var`.setStaticType(`type`, null, properties)
    `var`.setInLoop(true)
    let.addReference(`var`, isLoopingReference = true)
    ExpressionTool.copyLocationInfo(child, `var`)
    oldOperand.setChildExpression(`var`)
    if (tracing) {
      val err = config.getLogger
      err.info(
        "OPT : At line " + child.getLocation.getLineNumber + " of " +
          child.getLocation.getSystemId)
      err.info(
        "OPT : Lifted (" + child.toShortString + ") above (" +
          newAction.toShortString +
          ") on line " +
          newAction.getLocation.getLineNumber)
      err.info("OPT : Expression after rewrite: " + let)
    }
    let
  }
}
