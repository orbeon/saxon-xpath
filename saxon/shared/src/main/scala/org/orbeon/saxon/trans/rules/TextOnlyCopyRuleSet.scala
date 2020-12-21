////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans.rules

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.XPathContextMajor
import org.orbeon.saxon.expr.instruct.ParameterSet
import org.orbeon.saxon.expr.instruct.TailCall
import org.orbeon.saxon.ma.arrays.ArrayFunctionSet
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.AtomicValue

/**
 * The built-in rule set used for 1.0 and 2.0, which for document and element nodes does an apply-templates
 * to children, and for text nodes and attribute nodes copies the node.
 */
object TextOnlyCopyRuleSet {
  private val THE_INSTANCE = new TextOnlyCopyRuleSet

  /**
   * Get the singleton instance of this class
   *
   * @return the singleton instance
   */
  def getInstance: TextOnlyCopyRuleSet = THE_INSTANCE
}

class TextOnlyCopyRuleSet private() extends BuiltInRuleSet {
  /**
   * Perform the built-in template action for a given item.
   *
   * @param item         the item to be processed
   * @param parameters   the parameters supplied to apply-templates
   * @param tunnelParams the tunnel parameters to be passed through
   * @param output       the destination for the result
   * @param context      the dynamic evaluation context
   * @param locationId   location of the instruction (apply-templates, apply-imports etc) that caused
   *                     the built-in template to be invoked
   * @throws org.orbeon.saxon.trans.XPathException
   * if any dynamic error occurs
   */
  @throws[XPathException]
  override def process(item: Item, parameters: ParameterSet, tunnelParams: ParameterSet, /*@NotNull*/ output: Outputter, context: XPathContext, locationId: Location) = if (item.isInstanceOf[NodeInfo]) {
    val node = item.asInstanceOf[NodeInfo]
    node.getNodeKind match {
      case Type.DOCUMENT =>
      case Type.ELEMENT =>
        val c2 = context.newContext
        c2.setOrigin(this)
        c2.trackFocus(node.iterateAxis(AxisInfo.CHILD))
        c2.setCurrentComponent(c2.getCurrentMode) // Bug 3508
        var tc = c2.getCurrentMode.getActor.applyTemplates(parameters, tunnelParams, output, c2, locationId)
        while ( {
          tc != null
        }) tc = tc.processLeavingTail
      case Type.TEXT =>
      // NOTE: I tried changing this to use the text node's copy() method, but
      // performance was worse
      case Type.ATTRIBUTE =>
        output.characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.COMMENT =>
      case Type.PROCESSING_INSTRUCTION =>
      case Type.NAMESPACE =>
      // no action
    }
  }
  else if (item.isInstanceOf[ArrayItem]) {
    val seq = ArrayFunctionSet.ArrayToSequence.toSequence(item.asInstanceOf[ArrayItem])
    val members = seq.iterate
    val c2 = context.newContext
    c2.setOrigin(this)
    c2.trackFocus(members)
    c2.setCurrentComponent(c2.getCurrentMode)
    var tc = c2.getCurrentMode.getActor.applyTemplates(parameters, tunnelParams, output, c2, locationId)
    while ( {
      tc != null
    }) tc = tc.processLeavingTail
  }
  else if (item.isInstanceOf[AtomicValue]) output.characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
  else {
    // no action (e.g. for function items
  }

  /**
   * Identify this built-in rule set
   *
   * @return "text-only"
   */
  override def getName = "text-only"

  /**
   * Get the default action for unmatched nodes
   *
   * @param nodeKind the node kind
   * @return the default action for unmatched nodes: one of DEEP_COPY, SHALLOW_SKIP, DEEP_SKIP, FAIL, etc
   */
  override def getActionForParentNodes(nodeKind: Int): Array[Int] = Array[Int](APPLY_TEMPLATES_TO_CHILDREN)
}