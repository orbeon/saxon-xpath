package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.Component

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.StandardNames

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Mode

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import java.util.Arrays

class NextMatch(var useTailRecursion: Boolean)
  extends ApplyNextMatchingTemplate() {

  override def getInstructionNameCode(): Int = StandardNames.XSL_NEXT_MATCH

  def copy(rebindings: RebindingMap): Expression = {
    val nm2: NextMatch = new NextMatch(useTailRecursion)
    nm2.setActualParams(WithParam.copy(nm2, getActualParams, rebindings))
    nm2.setTunnelParams(WithParam.copy(nm2, getTunnelParams, rebindings))
    ExpressionTool.copyLocationInfo(this, nm2)
    nm2
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val params: ParameterSet = Instruction.assembleParams(context, getActualParams)
    val tunnels: ParameterSet = Instruction.assembleTunnelParams(context, getTunnelParams)
    val currentRule: Rule = context.getCurrentTemplateRule
    if (currentRule == null) {
      val e: XPathException =
        new XPathException("There is no current template rule", "XTDE0560")
      e.setXPathContext(context)
      e.setLocation(getLocation)
      throw e
    }
    val modeComponent: Component.M = context.getCurrentMode
    if (modeComponent == null) {
      throw new AssertionError("Current mode is null")
    }
    val mode: Mode = modeComponent.getActor
    val currentItem: Item = context.getCurrentIterator.current
    val rule: Rule = mode.getNextMatchRule(currentItem, currentRule, context)
    if (rule == null) {
      mode.getBuiltInRuleSet.process(currentItem,
        params,
        tunnels,
        output,
        context,
        getLocation)
    } else if (useTailRecursion) {
      Arrays.fill(context.getStackFrame.getStackFrameValues.asInstanceOf[Array[Object]], null.asInstanceOf[Object])
      context
        .asInstanceOf[XPathContextMajor]
        .setCurrentComponent(modeComponent)
      new NextMatchPackage(rule, params, tunnels, output, context)
    } else {
      val nh: TemplateRule = rule.getAction.asInstanceOf[TemplateRule]
      nh.initialize()
      val c2: XPathContextMajor = context.newContext()
      c2.setOrigin(this)
      c2.openStackFrame(nh.getStackFrameMap)
      c2.setLocalParameters(params)
      c2.setTunnelParameters(tunnels)
      c2.setCurrentTemplateRule(rule)
      c2.setCurrentComponent(modeComponent)
      c2.setCurrentMergeGroupIterator(null)
      nh.apply(output, c2)
    }
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("nextMatch", this)
    var flags: String = "i"
    if (useTailRecursion) {
      flags = "t"
    }
    out.emitAttribute("flags", flags)
    if (getActualParams.length != 0) {
      WithParam.exportParameters(getActualParams, out, tunnel = false)
    }
    if (getTunnelParams.length != 0) {
      WithParam.exportParameters(getTunnelParams, out, tunnel = true)
    }
    out.endElement()
  }

  private class NextMatchPackage(private var rule: Rule,
                                 private var params: ParameterSet,
                                 private var tunnelParams: ParameterSet,
                                 private var output: Outputter,
                                 private var evaluationContext: XPathContext)
    extends TailCall {

    def processLeavingTail(): TailCall = {
      val nh: TemplateRule = rule.getAction.asInstanceOf[TemplateRule]
      nh.initialize()
      val c2: XPathContextMajor = evaluationContext.newContext()
      c2.setOrigin(NextMatch.this)
      c2.setLocalParameters(params)
      c2.setTunnelParameters(tunnelParams)
      c2.openStackFrame(nh.getStackFrameMap)
      c2.setCurrentTemplateRule(rule)
      c2.setCurrentComponent(evaluationContext.getCurrentComponent)
      c2.setCurrentMergeGroupIterator(null)
      nh.applyLeavingTail(output, c2)
    }

  }

  override def getStreamerName: String = "NextMatch"

}
