package net.sf.saxon.expr.accum

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.SlotManager

import net.sf.saxon.expr.parser.Evaluator

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import net.sf.saxon.tree.iter.ManualIterator

import net.sf.saxon.tree.util.Navigator

import java.util.ArrayList

import java.util.List

import AccumulatorData._

import scala.beans.BeanProperty

import scala.jdk.CollectionConverters._

object AccumulatorData {

  private class Visit(var node: NodeInfo, var isPostDescent: Boolean)
    extends Comparable[Visit] {

    def compareTo(other: Visit): Int = {
      val relation: Int = Navigator.comparePosition(node, other.node)
      relation match {
        case AxisInfo.SELF =>
          if (isPostDescent == other.isPostDescent) {
            0
          } else {
            if (isPostDescent) +1 else -1
          }
        case AxisInfo.PRECEDING => -1
        case AxisInfo.FOLLOWING => +1
        case AxisInfo.ANCESTOR => if (isPostDescent) +1 else -1
        case AxisInfo.DESCENDANT => if (other.isPostDescent) -1 else +1
        case _ => throw new IllegalStateException()

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
        "Accumulator " + accumulator.getAccumulatorName.getDisplayName +
          " requires access to its own value",
        "XTDE3400")
    }
    building = true
    val initialValue: Expression = accumulator.getInitialValueExpression
    val c2: XPathContextMajor = context.newContext()
    val sf: SlotManager = accumulator.getSlotManagerForInitialValueExpression
    val slots: Array[Sequence] = Array.ofDim[Sequence](sf.getNumberOfVariables)
    c2.setStackFrame(sf, slots)
    c2.setCurrentIterator(new ManualIterator(doc))
    var `val`: Sequence = initialValue.iterate(c2).materialize()
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
      var rule: Rule = accumulator.getPreDescentRules.getRule(node, context)
      if (rule != null) {
        seq = processRule(rule, node, isPostDescent = false, seq, context)
        logChange(node, seq, context, " BEFORE ")
      }
      for (kid <- node.children) {
        seq = visit(kid, seq, context)
      }
      context.getCurrentIterator
        .asInstanceOf[ManualIterator]
        .setContextItem(node)
      rule = accumulator.getPostDescentRules.getRule(node, context)
      if (rule != null) {
        seq = processRule(rule, node, isPostDescent = true, seq, context)
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
        accumulator.getAccumulatorName.getDisplayName + phase +
          Navigator.getPath(node) +
          ": " +
          Err.depictSequence(value))
    }
  }

  private def processRule(rule: Rule,
                          node: NodeInfo,
                          isPostDescent: Boolean,
                          value: Sequence,
                          context: XPathContext): Sequence = {
    var seq = value
    val target: AccumulatorRule = rule.getAction.asInstanceOf[AccumulatorRule]
    val delta: Expression = target.getNewValueExpression
    val c2: XPathContextMajor = context.newCleanContext()
    val controller: Controller = c2.getController
    assert(controller != null)
    val initialNode: ManualIterator = new ManualIterator(node)
    c2.setCurrentIterator(initialNode)
    c2.openStackFrame(target.getStackFrameMap)
    c2.setLocalVariable(0, seq)
    c2.setCurrentComponent(accumulator.getDeclaringComponent)
    c2.setTemporaryOutputState(StandardNames.XSL_ACCUMULATOR_RULE)
    seq = Evaluator.EAGER_SEQUENCE.evaluate(delta, c2)
    values.add(new DataPoint(new Visit(node, isPostDescent), seq))
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
        values.get(start - 1).value
      } else {
        values.get(start).value
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
