package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StringLiteral

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.accum.Accumulator

import net.sf.saxon.expr.accum.AccumulatorManager

import net.sf.saxon.expr.accum.AccumulatorRegistry

import net.sf.saxon.expr.accum.IAccumulatorData

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XsltController

import org.jetbrains.annotations.NotNull

import AccumulatorFn._

object AccumulatorFn {

  object Phase extends Enumeration {

    val AFTER: Phase = new Phase()

    val BEFORE: Phase = new Phase()

    class Phase extends Val

    implicit def convertValue(v: Value): Phase = v.asInstanceOf[Phase]

  }

  class AccumulatorBefore extends AccumulatorFn {

    override def getPhase(): Phase.Phase = Phase.BEFORE

  }

  class AccumulatorAfter extends AccumulatorFn {

    override def getPhase(): Phase.Phase = Phase.AFTER

    override def getStreamerName(): String = "AccumulatorAfter"

  }

}

abstract class AccumulatorFn extends SystemFunction {

  def getPhase(): Phase.Phase

  private def getAccumulatorValue(name: String,
                                  phase: Phase.Phase,
                                  context: XPathContext): Sequence = {
    val registry: AccumulatorRegistry =
      getRetainedStaticContext.getPackageData.getAccumulatorRegistry
    val accumulator: Accumulator = getAccumulator(name, registry)
    val node: Item = context.getContextItem
    if (node == null) {
      throw new XPathException(
        "No context item for evaluation of accumulator function",
        "XTDE3350",
        context)
    }
    if (!(node.isInstanceOf[NodeInfo])) {
      throw new XPathException(
        "Context item for evaluation of accumulator function must be a node",
        "XTTE3360",
        context)
    }
    val kind: Int = node.asInstanceOf[NodeInfo].getNodeKind
    if (kind == Type.ATTRIBUTE || kind == Type.NAMESPACE) {
      throw new XPathException(
        "Context item for evaluation of accumulator function must not be an attribute or namespace node",
        "XTTE3360",
        context)
    }
    val streamedAccVal: Sequence = registry.getStreamingAccumulatorValue(
      node.asInstanceOf[NodeInfo],
      accumulator,
      phase)
    if (streamedAccVal != null) {
      streamedAccVal
    }
    val root: TreeInfo = node.asInstanceOf[NodeInfo].getTreeInfo
    val controller: XsltController =
      context.getController.asInstanceOf[XsltController]
    if (!accumulator.isUniversallyApplicable &&
      !controller.getAccumulatorManager.isApplicable(root, accumulator)) {
      throw new XPathException(
        "Accumulator " + name + " is not applicable to the current document",
        "XTDE3362")
    }
    val manager: AccumulatorManager = controller.getAccumulatorManager
    val data: IAccumulatorData =
      manager.getAccumulatorData(root, accumulator, context)
    data.getValue(node.asInstanceOf[NodeInfo], phase == Phase.AFTER)
  }

  @NotNull
  private def getAccumulator(name: String,
                             registry: AccumulatorRegistry): Accumulator = {
    var qName: StructuredQName = null
    qName = StructuredQName.fromLexicalQName(name,
      false,
      true,
      getRetainedStaticContext)
    val accumulator: Accumulator =
      if (registry == null) null else registry.getAccumulator(qName)
    if (accumulator == null) {
      throw new XPathException(
        "Accumulator " + name + " has not been declared",
        "XTDE3340")
    }
    accumulator
  }

 override def getResultItemType(args: Array[Expression]): ItemType = {
    try if (args(0).isInstanceOf[StringLiteral]) {
      val registry: AccumulatorRegistry =
        getRetainedStaticContext.getPackageData.getAccumulatorRegistry
      val accumulator: Accumulator = getAccumulator(
        args(0).asInstanceOf[StringLiteral].getStringValue,
        registry)
      accumulator.getSeqType.getPrimaryType
    } catch {
      case e: Exception => {}

    }
    super.getResultItemType(args)
  }

  override def getCardinality(args: Array[Expression]): Int = {
    try if (args(0).isInstanceOf[StringLiteral]) {
      val registry: AccumulatorRegistry =
        getRetainedStaticContext.getPackageData.getAccumulatorRegistry
      val accumulator: Accumulator = getAccumulator(
        args(0).asInstanceOf[StringLiteral].getStringValue,
        registry)
      accumulator.getSeqType.getCardinality
    } catch {
      case e: Exception => {}

    }
    super.getCardinality(args)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val name: String = arguments(0).head().getStringValue
    getAccumulatorValue(name, getPhase, context)
  }

}
