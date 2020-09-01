////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.expr.instruct.TailCall

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import DeepSkipRuleSet._




object DeepSkipRuleSet {

  private var THE_INSTANCE: DeepSkipRuleSet = new DeepSkipRuleSet()

  def getInstance: DeepSkipRuleSet = THE_INSTANCE

}

/**
  * The rule set introduced in XSLT 3.0, which (for any kind of node) simply ignores the node
  * and its descendants.
  */
class DeepSkipRuleSet private () extends BuiltInRuleSet {

  def process(item: Item,
              parameters: ParameterSet,
              tunnelParams: ParameterSet,
              output: Outputter,
              context: XPathContext,
              locationId: Location): Unit = {
    if (item.isInstanceOf[NodeInfo] &&
        item.asInstanceOf[NodeInfo].getNodeKind == Type.DOCUMENT) {
      val c2: XPathContextMajor = context.newContext()
      c2.setOrigin(this)
      c2.trackFocus(item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.CHILD))
      c2.setCurrentComponent(c2.getCurrentMode)
      var tc: TailCall = c2.getCurrentMode.getActor.applyTemplates(
        parameters,
        tunnelParams,
        output,
        c2,
        locationId)
      while (tc != null) tc = tc.processLeavingTail()
    }
  }
// otherwise, do nothing
// otherwise, do nothing

  override def getName(): String = "deep-skip"

  /**
    * Get the default action for unmatched nodes
    *
    * @param nodeKind the node kind
    * @return the default action for unmatched element nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
    */
  def getActionForParentNodes(nodeKind: Int): Array[Int] =
    if (nodeKind == Type.DOCUMENT) {
      Array(APPLY_TEMPLATES_TO_CHILDREN)
    } else {
      Array(DEEP_SKIP)
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
