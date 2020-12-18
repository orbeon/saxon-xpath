////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.dom

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.AxisInfo
import org.orbeon.saxon.om.NamespaceBinding
import org.orbeon.saxon.om.NamespaceMap
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.tree.NamespaceNode
import org.orbeon.saxon.tree.iter.AxisIterator
import org.w3c.dom.DOMException
import org.w3c.dom.NamedNodeMap
import org.w3c.dom.Node

class DOMAttributeMap(private var element: NodeInfo) extends NamedNodeMap {

  private var namespaceDeltas: Array[NamespaceBinding] = _

  private var excludeNamespaceUndeclarations: Boolean = _

  if (element.getConfiguration.getXMLVersion == Configuration.XML10) {
    excludeNamespaceUndeclarations = true
  }

  private def getNamespaceDeltas: Array[NamespaceBinding] = {
    val allNamespaces = element.getAllNamespaces
    var bindings: Array[NamespaceBinding] = null
    val parent = element.getParent
    bindings =
      if (parent != null && parent.getNodeKind == Type.ELEMENT)
        allNamespaces.getDifferences(parent.getAllNamespaces, ! excludeNamespaceUndeclarations)
      else
        allNamespaces.getNamespaceBindings
    bindings
  }

  def getNamedItem(name: String): Node =
    if (name.==("xmlns")) {
      val nsarray: Array[NamespaceBinding] = getNamespaceBindings
      for (i <- nsarray.indices) {
        if (nsarray(i) == null) {
          return null
        } else if (nsarray(i).getPrefix().isEmpty) {
          val nn: NamespaceNode = new NamespaceNode(element, nsarray(i), i + 1)
         return NodeOverNodeInfo.wrap(nn)
        }
      }
     null
    } else if (name.startsWith("xmlns:")) {
      val prefix: String = name.substring(6)
      val nsarray: Array[NamespaceBinding] = getNamespaceBindings
      for (i <- nsarray.indices) {
        if (nsarray(i) == null) {
         return null
        } else if (prefix == nsarray(i).getPrefix()) {
          val nn: NamespaceNode = new NamespaceNode(element, nsarray(i), i + 1)
          return NodeOverNodeInfo.wrap(nn)
        }
      }
      null
    } else {
      val atts: AxisIterator = element.iterateAxis(
        AxisInfo.ATTRIBUTE,
        (att: NodeInfo) => att.getDisplayName == name)
      val att: NodeInfo = atts.next()
      if (att == null) null else NodeOverNodeInfo.wrap(att)
    }

  def item(index: Int): Node = {
    if (index < 0) {
      return null
    }
    val namespaces: Array[NamespaceBinding] = getNamespaceBindings
    if (index < namespaces.length) {
      val ns: NamespaceBinding = namespaces(index)
      val nn: NamespaceNode = new NamespaceNode(element, ns, index)
      return NodeOverNodeInfo.wrap(nn)
    }
    var pos: Int = 0
    val attNr: Int = index - namespaces.length
    val atts: AxisIterator = element.iterateAxis(AxisInfo.ATTRIBUTE)
    var att: NodeInfo = null
    while (({
      att = atts.next()
      att
    }) != null) {
      if (pos == attNr) {
        return NodeOverNodeInfo.wrap(att)
      }
      pos += 1
    }
    null
  }

  private def getNumberOfNamespaces: Int = getNamespaceBindings.length

  private def getNamespaceBindings: Array[NamespaceBinding] = {
    if (namespaceDeltas == null) {
      namespaceDeltas = getNamespaceDeltas
    }
    namespaceDeltas
  }

  def getLength: Int = {
    var length: Int = 0
    val atts: AxisIterator = element.iterateAxis(AxisInfo.ATTRIBUTE)
    while (atts.next() != null) { length += 1 }
    getNumberOfNamespaces + length
  }

  def getNamedItemNS(uri: String, localName: String): Node = {
    var res: Node = null
    var uRI = uri
    if (uri == null) {
      uRI = ""
    }
    if (NamespaceConstant.XMLNS == uri) {
      res = getNamedItem("xmlns:" + localName)
    }
    if (uRI.==("") && localName.==("xmlns")) {
      res= getNamedItem("xmlns")
    }
    val atts: AxisIterator = element.iterateAxis(AxisInfo.ATTRIBUTE)
    while (true) {
      val att: NodeInfo = atts.next()
      if (att == null) res = null
      if (uRI == att.getURI && localName == att.getLocalPart)
        res = NodeOverNodeInfo.wrap(att)
    }
    res
  }

  def setNamedItem(arg: Node): Node = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

  def removeNamedItem(name: String): Node = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

  def setNamedItemNS(arg: Node): Node = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

  /*@Nullable*/

  def removeNamedItemNS(uri: String, localName: String): Node = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Implementation of DOM NamedNodeMap used to represent the attributes of an element, for use when
 * Saxon element and attribute nodes are accessed using the DOM API.
 * <p>Note that namespaces are treated as attributes.</p>
 */
