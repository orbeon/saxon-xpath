////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.XPathContextMajor

import net.sf.saxon.expr.instruct.ParameterSet

import net.sf.saxon.expr.instruct.TailCall

import net.sf.saxon.ma.arrays.ArrayFunctionSet

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import ShallowSkipRuleSet._


object ShallowSkipRuleSet {

  private var THE_INSTANCE: ShallowSkipRuleSet = new ShallowSkipRuleSet()

  def getInstance: ShallowSkipRuleSet = THE_INSTANCE

}

/**
  * A built-in set of template rules that ignores the current node and does an apply-templates
  * to its children.
  */
class ShallowSkipRuleSet private () extends BuiltInRuleSet {

  def process(item: Item,
              parameters: ParameterSet,
              tunnelParams: ParameterSet,
              output: Outputter,
              context: XPathContext,
              locationId: Location): Unit = {
    if (item.isInstanceOf[NodeInfo]) {
      val node: NodeInfo = item.asInstanceOf[NodeInfo]
      node.getNodeKind match {
        case Type.ELEMENT => {
          val c2: XPathContextMajor = context.newContext()
          c2.setOrigin(this)
          c2.trackFocus(node.iterateAxis(AxisInfo.ATTRIBUTE))
          c2.setCurrentComponent(c2.getCurrentMode)
          var tc: TailCall = c2.getCurrentMode.getActor.applyTemplates(
            parameters,
            tunnelParams,
            output,
            c2,
            locationId)
          while (tc != null) tc = tc.processLeavingTail()
        }
        case Type.DOCUMENT => {
          val c2: XPathContextMajor = context.newContext()
          c2.setOrigin(this)
          c2.trackFocus(node.iterateAxis(AxisInfo.CHILD))
          c2.setCurrentComponent(c2.getCurrentMode)
          var tc: TailCall = c2.getCurrentMode.getActor.applyTemplates(
            parameters,
            tunnelParams,
            output,
            c2,
            locationId)
          while (tc != null) tc = tc.processLeavingTail()
          return
        }

      }
      //                case Type.TEXT:
      //                case Type.ATTRIBUTE:
      //                case Type.COMMENT:
      //                case Type.PROCESSING_INSTRUCTION:
      //                case Type.NAMESPACE:
    } else if (item.isInstanceOf[ArrayItem]) {
      val seq: Sequence = ArrayFunctionSet.ArrayToSequence.toSequence(
        item.asInstanceOf[ArrayItem])
      val members: SequenceIterator = seq.iterate()
      val c2: XPathContextMajor = context.newContext()
      c2.setOrigin(this)
      c2.trackFocus(members)
      c2.setCurrentComponent(c2.getCurrentMode)
      var tc: TailCall = c2.getCurrentMode.getActor.applyTemplates(
        parameters,
        tunnelParams,
        output,
        c2,
        locationId)
      while (tc != null) tc = tc.processLeavingTail()
    } else {}
  }

  override def getName: String = "shallow-skip"

  /**
    * Get the default action for unmatched nodes
    *
    * @param nodeKind the node kind
    * @return the default action for unmatched nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
    */
  def getActionForParentNodes(nodeKind: Int): Array[Int] =
    Array(APPLY_TEMPLATES_TO_ATTRIBUTES, APPLY_TEMPLATES_TO_CHILDREN)

}

