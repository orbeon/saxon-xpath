package org.orbeon.saxon.expr.accum

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.XPathContextMajor

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.expr.parser.Evaluator

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.trans.rules.Rule

import org.orbeon.saxon.tree.iter.ManualIterator

import org.orbeon.saxon.tree.util.Navigator

import java.util.ArrayList

import java.util.List

import AccumulatorData._

import scala.beans.BeanProperty

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object AccumulatorData {

  private class Visit(var node: NodeInfo, var postDescent: Boolean)
    extends Comparable[Visit] {

    def compareTo(other: Visit): Int = {
      val relation: Int = Navigator.comparePosition(node, other.node)
      relation match {
        case AxisInfo.SELF =>
          if (postDescent == other.postDescent) {
            0
          } else {
            if (postDescent) +1 else -1
          }
        case AxisInfo.PRECEDING => -1
        case AxisInfo.FOLLOWING => +1
        case AxisInfo.ANCESTOR => if (postDescent) +1 else -1
        case AxisInfo.DESCENDANT => if (other.postDescent) -1 else +1
        case _ => throw new IllegalStateException

      }
    }

  }

  private class DataPoint(var visit: Visit, var value: Sequence)

}

class AccumulatorData(acc: Accumulator) extends IAccumulatorData {

  @BeanProperty
  var accumulator: Accumulator = acc

  private var values: List[DataPoint] = new ArrayList()

  private var building: Boolean = false

  def buildIndex(doc: NodeInfo, context: XPathContext): Unit = {
    if (building) {
      throw new XPathException(
        "Accumulator " + accumulator.accumulatorName.getDisplayName +
          " requires access to its own value",
        "XTDE3400")
    }
    building = true
    val initialValue: Expression = accumulator.initialValueExpression
    val c2: XPathContextMajor = context.newContext()
    val sf: SlotManager = accumulator.slotManagerForInitialValueExpression
    val slots: Array[Sequence] = Array.ofDim[Sequence](sf.getNumberOfVariables)
    c2.setStackFrame(sf, slots)
    c2.setCurrentIterator(new ManualIterator(doc))
    var `val`: Sequence = initialValue.iterate(c2).materialize
    values.add(new DataPoint(new Visit(doc, false), `val`))
    `val` = visit(doc, `val`, c2)
    values.add(new DataPoint(new Visit(doc, true), `val`))
    values.asInstanceOf[ArrayList[DataPoint]].trimToSize()
    building = false
  }

  private def visit(node: NodeInfo,
                    value: Sequence,
                    context: XPathContext): Sequence = {
    var seq = value
    try {
      context.getCurrentIterator
        .asInstanceOf[ManualIterator]
        .setContextItem(node)
      var rule: Rule = accumulator.preDescentRules.getRule(node, context)
      if (rule != null) {
        seq = processRule(rule, node, postDescent = false, seq, context)
        logChange(node, seq, context, " BEFORE ")
      }
      for (kid <- node.children) {
        seq = visit(kid, seq, context)
      }
      context.getCurrentIterator
        .asInstanceOf[ManualIterator]
        .setContextItem(node)
      rule = accumulator.postDescentRules.getRule(node, context)
      if (rule != null) {
        seq = processRule(rule, node, postDescent = true, seq, context)
        logChange(node, seq, context, " AFTER ")
      }
      seq
    } catch {
      case e: StackOverflowError => {
        val err = new XPathException.StackOverflow(
          "Too many nested accumulator evaluations. The accumulator definition may have cyclic dependencies",
          "XTDE3400",
          accumulator)
        err.setXPathContext(context)
        throw err
      }

    }
  }
  private def logChange(node: NodeInfo,
                        value: Sequence,
                        context: XPathContext,
                        phase: String): Unit = {
    if (accumulator.isTracing) {
      context.getConfiguration.getLogger.info(
        accumulator.accumulatorName.getDisplayName + phase +
          Navigator.getPath(node) +
          ": " +
          Err.depictSequence(value))
    }
  }

  private def processRule(rule: Rule,
                          node: NodeInfo,
                          postDescent: Boolean,
                          value: Sequence,
                          context: XPathContext): Sequence = {
    var seq = value
    val target: AccumulatorRule = rule.getAction.asInstanceOf[AccumulatorRule]
    val delta: Expression = target.newValueExpression
    val c2: XPathContextMajor = context.newCleanContext()
    val controller: Controller = c2.getController
    assert(controller != null)
    val initialNode: ManualIterator = new ManualIterator(node)
    c2.setCurrentIterator(initialNode)
    c2.openStackFrame(target.getStackFrameMap)
    c2.setLocalVariable(0, seq)
    c2.setCurrentComponent(accumulator.declaringComponent)
    c2.setTemporaryOutputState(StandardNames.XSL_ACCUMULATOR_RULE)
    seq = Evaluator.EAGER_SEQUENCE.evaluate(delta, c2)
    values.add(new DataPoint(new Visit(node, postDescent), seq))
    seq
  }

  def getValue(node: NodeInfo, postDescent: Boolean): Sequence = {
    val visit: Visit = new Visit(node, postDescent)
    search(0, values.size, visit)
  }

  private def search(start: Int, end: Int, sought: Visit): Sequence = {
    if (start == end) {
      val rel: Int = sought.compareTo(values.get(start).visit)
      if (rel < 0) {
        return values.get(start - 1).value
      } else {
        return values.get(start).value
      }
    }
    val mid: Int = (start + end) / 2
    if (sought.compareTo(values.get(mid).visit) <= 0) {
      search(start, mid, sought)
    } else {
      search(mid + 1, end, sought)
    }
  }

}
