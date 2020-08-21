////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import net.sf.saxon.utils.Configuration
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.Type
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.NamespaceBinding
import net.sf.saxon.om.NamespaceMap
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.tree.NamespaceNode
import net.sf.saxon.tree.iter.AxisIterator
import org.w3c.dom.DOMException
import org.w3c.dom.NamedNodeMap
import org.w3c.dom.Node

class DOMAttributeMap(private var element: NodeInfo) extends NamedNodeMap {

  private var namespaceDeltas: Array[NamespaceBinding] = _

  private var excludeNamespaceUndeclarations: Boolean = _

  if (element.getConfiguration.getXMLVersion == Configuration.XML10) {
    excludeNamespaceUndeclarations = true
  }

  private def getNamespaceDeltas(): Array[NamespaceBinding] = {
    val allNamespaces: NamespaceMap = element.getAllNamespaces
    var bindings: Array[NamespaceBinding] = null
    val parent: NodeInfo = element.getParent
    bindings =
      if (parent != null && parent.getNodeKind == Type.ELEMENT)
        allNamespaces.getDifferences(parent.getAllNamespaces,
          !excludeNamespaceUndeclarations)
      else allNamespaces.getNamespaceBindings
    bindings
  }

  def getNamedItem(name: String): Node =
    if (name.==("xmlns")) {
      val nsarray: Array[NamespaceBinding] = getNamespaceBindings
      for (i <- 0 until nsarray.length) {
        if (nsarray(i) == null) {
          null
        } else if (nsarray(i).getPrefix.isEmpty) {
          val nn: NamespaceNode = new NamespaceNode(element, nsarray(i), i + 1)
          NodeOverNodeInfo.wrap(nn)
        }
      }
      null
    } else if (name.startsWith("xmlns:")) {
      val prefix: String = name.substring(6)
      val nsarray: Array[NamespaceBinding] = getNamespaceBindings
      for (i <- 0 until nsarray.length) {
        if (nsarray(i) == null) {
          null
        } else if (prefix == nsarray(i).getPrefix) {
          val nn: NamespaceNode = new NamespaceNode(element, nsarray(i), i + 1)
          NodeOverNodeInfo.wrap(nn)
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
      NodeOverNodeInfo.wrap(nn)
    }
    var pos: Int = 0
    val attNr: Int = index - namespaces.length
    val atts: AxisIterator = element.iterateAxis(AxisInfo.ATTRIBUTE)
    var att: NodeInfo = null
    while ((att = atts.next()) != null) {
      if (pos == attNr) {
        NodeOverNodeInfo.wrap(att)
      }
      { pos += 1; pos - 1 }
    }
    null
  }

  private def getNumberOfNamespaces(): Int = getNamespaceBindings.length

  private def getNamespaceBindings(): Array[NamespaceBinding] = {
    if (namespaceDeltas == null) {
      namespaceDeltas = getNamespaceDeltas
    }
    namespaceDeltas
  }

  def getLength(): Int = {
    var length: Int = 0
    val atts: AxisIterator = element.iterateAxis(AxisInfo.ATTRIBUTE)
    while (atts.next() != null) { length += 1; length - 1 }
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
