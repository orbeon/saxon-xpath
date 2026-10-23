package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.Component

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.XPathContextMajor

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.RebindingMap

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.Mode

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.trans.rules.Rule

class ApplyImports extends ApplyNextMatchingTemplate with ITemplateCall {

 override def getInstructionNameCode: Int = StandardNames.XSL_APPLY_IMPORTS

  def copy(rebindings: RebindingMap): Expression = {
    val ai2: ApplyImports = new ApplyImports()
    ai2.actualParams = WithParam.copy(ai2, actualParams, rebindings)
    ai2.tunnelParams = WithParam.copy(ai2, tunnelParams, rebindings)
    ExpressionTool.copyLocationInfo(this, ai2)
    ai2
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val params: ParameterSet = Instruction.assembleParams(context, actualParams)
    val tunnels: ParameterSet = Instruction.assembleTunnelParams(context, tunnelParams)
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
    val currentItem: Item = context.getCurrentIterator.current
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
      c2.openStackFrame(nh.stackFrameMap)
      c2.setCurrentTemplateRule(rule)
      c2.setCurrentComponent(modeComponent)
      c2.setCurrentMergeGroupIterator(null)
      nh.apply(output, c2)
    }
    null
  }

  def `export`(out: ExpressionPresenter): Unit = {
    out.startElement("applyImports", this)
    out.emitAttribute("flags", "i")
    if (actualParams.length != 0) {
      WithParam.exportParameters(actualParams, out, tunnel = false)
    }
    if (tunnelParams.length != 0) {
      WithParam.exportParameters(tunnelParams, out, tunnel = true)
    }
    out.endElement()
  }

  override def getStreamerName: String = "ApplyImports"

}
