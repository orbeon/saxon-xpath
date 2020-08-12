////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.{Outputter, ReceiverOption}
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.instruct.ParameterSet
import net.sf.saxon.model.{SimpleType, Type}
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.EmptyIterator

import scala.jdk.CollectionConverters._

/**
 * The built-in rule set introduced in XSLT 3.0, which is effectively an identity template.
 */
object ShallowCopyRuleSet {
  private val THE_INSTANCE = new ShallowCopyRuleSet

  /**
   * Get the singleton instance of this class
   *
   * @return the singleton instance
   */
  def getInstance = THE_INSTANCE
}

class ShallowCopyRuleSet private() extends BuiltInRuleSet {
  /**
   * Perform the built-in template action for a given node.
   *
   * @param item
   * @param parameters   the parameters supplied to apply-templates
   * @param tunnelParams the tunnel parameters to be passed through
   * @param out
   * @param context      the dynamic evaluation context
   * @param locationId   location of the instruction (apply-templates, apply-imports etc) that caused
   */
  @throws[XPathException]
  override def process(item: Item, parameters: ParameterSet, tunnelParams: ParameterSet, out: Outputter, context: XPathContext, locationId: Location) = if (item.isInstanceOf[NodeInfo]) {
    val node = item.asInstanceOf[NodeInfo]
    node.getNodeKind match {
      case Type.DOCUMENT =>
        val pipe = out.getPipelineConfiguration
        if (out.getSystemId == null) out.setSystemId(node.getBaseURI)
        out.startDocument(ReceiverOption.NONE)
        val c2 = context.newContext
        c2.setOrigin(this)
        c2.trackFocus(node.iterateAxis(AxisInfo.CHILD))
        c2.setCurrentComponent(c2.getCurrentMode) // Bug 3508
        pipe.setXPathContext(c2)
        var tc = context.getCurrentMode.getActor.applyTemplates(parameters, tunnelParams, out, c2, locationId)
        while ( {
          tc != null
        }) tc = tc.processLeavingTail
        out.endDocument()
        pipe.setXPathContext(context)
      case Type.ELEMENT =>
        val pipe = out.getPipelineConfiguration
        if (out.getSystemId == null) out.setSystemId(node.getBaseURI)
        val fqn = NameOfNode.makeName(node)
        out.startElement(fqn, node.getSchemaType, locationId, ReceiverOption.NONE)

        for (ns <- node.getAllNamespaces.asScala) {
          out.namespace(ns.getPrefix, ns.getURI, ReceiverOption.NONE)
        }
        val c2 = context.newContext
        c2.setCurrentComponent(c2.getCurrentMode)
        pipe.setXPathContext(c2)
        // apply-templates to all attributes
        val attributes = node.iterateAxis(AxisInfo.ATTRIBUTE)
        if (attributes ne EmptyIterator.ofNodes) {
          c2.setOrigin(this)
          c2.trackFocus(attributes)
          var tc = c2.getCurrentMode.getActor.applyTemplates(parameters, tunnelParams, out, c2, locationId)
          while ( {
            tc != null
          }) tc = tc.processLeavingTail
        }
        // apply-templates to all children
        if (node.hasChildNodes) {
          c2.trackFocus(node.iterateAxis(AxisInfo.CHILD))
          var tc = c2.getCurrentMode.getActor.applyTemplates(parameters, tunnelParams, out, c2, locationId)
          while ( {
            tc != null
          }) tc = tc.processLeavingTail
        }
        out.endElement()
        pipe.setXPathContext(context)
      case Type.TEXT =>
        out.characters(node.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.COMMENT =>
        out.comment(node.getStringValueCS, locationId, ReceiverOption.NONE)
      case Type.PROCESSING_INSTRUCTION =>
        out.processingInstruction(node.getLocalPart, node.getStringValue, locationId, ReceiverOption.NONE)
      case Type.ATTRIBUTE =>
        out.attribute(NameOfNode.makeName(node), node.getSchemaType.asInstanceOf[SimpleType], node.getStringValue, locationId, ReceiverOption.NONE)
      case Type.NAMESPACE =>
        out.namespace(node.getLocalPart, node.getStringValue, ReceiverOption.NONE)
      case _ =>
    }
  }
  else out.append(item, locationId, ReceiverOption.NONE)

  /**
   * Identify this built-in rule set
   *
   * @return "shallow-copy"
   */
  override def getName = "shallow-copy"

  /**
   * Get the default action for unmatched nodes
   *
   * @param nodeKind the node kind
   * @return the default action for unmatched nodes: one of DEEP_COPY, APPLY_TEMPLATES, DEEP_SKIP, FAIL
   */
  override def getActionForParentNodes(nodeKind: Int) = Array[Int](SHALLOW_COPY, APPLY_TEMPLATES_TO_ATTRIBUTES, APPLY_TEMPLATES_TO_CHILDREN)
}