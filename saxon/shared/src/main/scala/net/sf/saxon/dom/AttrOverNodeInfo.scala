////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class is an implementation of the DOM Attr class that wraps a Saxon NodeInfo
 * representation of an attribute or namespace node.
 */

package net.sf.saxon.dom

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import org.w3c.dom._

import java.util.ArrayList

import java.util.List


class AttrOverNodeInfo extends NodeOverNodeInfo with Attr {

  def getName: String = {
    if (node.getNodeKind == Type.NAMESPACE) {
      val local: String = node.getLocalPart
      if (local.==("")) {
        return "xmlns"
      } else {
        return "xmlns:" + local
      }
    }
    node.getDisplayName
  }

  def getValue(): String = node.getStringValue

  override def hasChildNodes: Boolean = true

  override def getFirstChild: Node = new TextOverAttrInfo(this)

  override def getLastChild: Node = getFirstChild

  override def getChildNodes: NodeList = {
    val list: List[Node] = new ArrayList[Node](1)
    list.add(getFirstChild)
    new DOMNodeList(list)
  }

  def getSpecified(): Boolean = true

  def setValue(value: String): Unit = {
    NodeOverNodeInfo.disallowUpdate()
  }

  def isId: Boolean = node.isId

  def getOwnerElement(): Element =
    if (node.getNodeKind == Type.ATTRIBUTE || node.getNodeKind == Type.NAMESPACE) {
      NodeOverNodeInfo.wrap(node.getParent).asInstanceOf[Element]
    } else {
      throw new UnsupportedOperationException(
        "This method is defined only on attribute and namespace nodes")
    }

  /*@Nullable*/

  def getSchemaTypeInfo(): TypeInfo = {
    val `type`: SchemaType = node.getSchemaType
    if (`type` == null || BuiltInAtomicType.UNTYPED_ATOMIC == `type`) {
      return null
    }
    new TypeInfoImpl(node.getConfiguration, `type`)
  }
}
