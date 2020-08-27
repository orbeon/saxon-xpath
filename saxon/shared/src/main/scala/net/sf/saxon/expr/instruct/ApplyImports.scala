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

class ApplyImports extends ApplyNextMatchingTemplate with ITemplateCall {

 override def getInstructionNameCode(): Int = StandardNames.XSL_APPLY_IMPORTS

  def copy(rebindings: RebindingMap): Expression = {
    val ai2: ApplyImports = new ApplyImports()
    ai2.setActualParams(WithParam.copy(ai2, getActualParams, rebindings))
    ai2.setTunnelParams(WithParam.copy(ai2, getTunnelParams, rebindings))
    ExpressionTool.copyLocationInfo(this, ai2)
    ai2
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val params: ParameterSet = Instruction.assembleParams(context, getActualParams)
    val tunnels: ParameterSet = Instruction.assembleTunnelParams(context, getTunnelParams)
    val currentTemplateRule: Rule = context.getCurrentTemplateRule
    if (currentTemplateRule == null) {
      val e: XPathException = new XPathException(
        "There is no current template rule")
      e.setXPathContext(context)
      e.setErrorCode("XTDE0560")
      e.setLocation(getLocation)
      throw e
    }
    val min: Int = currentTemplateRule.getMinImportPrecedence
    val max: Int = currentTemplateRule.getPrecedence - 1
    val modeComponent: Component.M = context.getCurrentMode
    if (modeComponent == null) {
      throw new AssertionError("Current mode is null")
    }
    val currentItem: Item = context.getCurrentIterator.current()
    val mode: Mode = modeComponent.getActor
    val rule: Rule = mode.getRule(currentItem, min, max, context)
    if (rule == null) {
      mode.getBuiltInRuleSet.process(currentItem,
        params,
        tunnels,
        output,
        context,
        getLocation)
    } else {
      val c2: XPathContextMajor = context.newContext()
      val nh: TemplateRule = rule.getAction.asInstanceOf[TemplateRule]
      nh.initialize()
      c2.setOrigin(this)
      c2.setLocalParameters(params)
      c2.setTunnelParameters(tunnels)
      c2.openStackFrame(nh.getStackFrameMap)
      c2.setCurrentTemplateRule(rule)
      c2.setCurrentComponent(modeComponent)
      c2.setCurrentMergeGroupIterator(null)
      nh.apply(output, c2)
    }
    null
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("applyImports", this)
    out.emitAttribute("flags", "i")
    if (getActualParams.length != 0) {
      WithParam.exportParameters(getActualParams, out, tunnel = false)
    }
    if (getTunnelParams.length != 0) {
      WithParam.exportParameters(getTunnelParams, out, tunnel = true)
    }
    out.endElement()
  }

  override def getStreamerName(): String = "ApplyImports"

}
