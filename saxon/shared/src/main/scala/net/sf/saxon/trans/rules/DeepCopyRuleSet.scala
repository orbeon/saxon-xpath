////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans.rules

import net.sf.saxon.event.Outputter
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.instruct.ParameterSet
import net.sf.saxon.model.SimpleType
import net.sf.saxon.model.Type
import net.sf.saxon.om.CopyOptions
import net.sf.saxon.om.Item
import net.sf.saxon.om.NameOfNode
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.Navigator

/**
 * The built-in rule set introduced in XSLT 3.0, which performs a deep copy of any unmatched node.
 */
object DeepCopyRuleSet {
  private val THE_INSTANCE = new DeepCopyRuleSet

  /**
   * Get the singleton instance of this class
   *
   * @return the singleton instance
   */
  def getInstance = THE_INSTANCE
}

class DeepCopyRuleSet private() extends BuiltInRuleSet {
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
      case Type.ELEMENT =>
        // TODO: fast path for TinyTree
        if (out.getSystemId == null) out.setSystemId(node.getBaseURI)
        Navigator.copy(node, out, CopyOptions.ALL_NAMESPACES | CopyOptions.TYPE_ANNOTATIONS, locationId)
      case Type.TEXT =>
        out.characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
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
   * Get the default action for unmatched nodes
   *
   * @param nodeKind the node kind
   * @return the default action for unmatched element nodes: one of DEEP_COPY, APPLY_TEMPLATES, SKIP, FAIL
   */
  override def getActionForParentNodes(nodeKind: Int) = Array[Int](DEEP_COPY)

  /**
   * Identify this built-in rule set
   *
   * @return "deep-copy"
   */
  override def getName = "deep-copy"
}