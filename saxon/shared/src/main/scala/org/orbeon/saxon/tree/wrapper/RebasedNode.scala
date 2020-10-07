////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.Function

import RebasedNode._




object RebasedNode {

  /*@NotNull*/

  def makeWrapper(node: NodeInfo,
                  docWrapper: RebasedDocument,
                  parent: RebasedNode): RebasedNode = {
    val wrapper: RebasedNode = new RebasedNode(node, parent)
    wrapper.docWrapper = docWrapper
    wrapper
  }

}

class RebasedNode  ()
    extends AbstractVirtualNode
    with WrappingFunction {

  def this (node: NodeInfo, parent: RebasedNode) = {
    this()
    this.node = node
    this.parent = parent
  }

  /*@NotNull*/

  def makeWrapper(node: NodeInfo, parent: VirtualNode): RebasedNode = {
    val wrapper: RebasedNode =
      new RebasedNode(node, parent.asInstanceOf[RebasedNode])
    wrapper.docWrapper = this.docWrapper
    wrapper
  }

  private def getBaseUriMappingFunction: Function[NodeInfo, String] =
    docWrapper.asInstanceOf[RebasedDocument].getBaseUriMapper

  private def getSystemIdMappingFunction: Function[NodeInfo, String] =
    docWrapper.asInstanceOf[RebasedDocument].getSystemIdMapper

  /**
    * Get the Base URI for the node, that is, the URI used for resolving a relative URI contained
    * in the node.
    */
  override def getBaseURI: String = getBaseUriMappingFunction.apply(node)

  /**
    * Get the System ID for the node.
    *
    * @return the System Identifier of the entity in the source document containing the node,
    * or null if not known. Note this is not the same as the base URI: the base URI can be
    * modified by xml:base, but the system ID cannot.
    */
  override def getSystemId: String = getSystemIdMappingFunction.apply(node)

  override def equals(other: Any): Boolean = other match {
    case other: RebasedNode => node == other.node
    case _ => false

  }

  override def compareOrder(other: NodeInfo): Int =
    if (other.isInstanceOf[RebasedNode]) {
      node.compareOrder(other.asInstanceOf[RebasedNode].node)
    } else {
      node.compareOrder(other)
    }

  /*@Nullable*/

  def getParent: NodeInfo = {
    if (parent == null) {
      val realParent: NodeInfo = node.getParent
      if (realParent != null) {
        parent = RebasedNode.makeWrapper(realParent,
                             docWrapper.asInstanceOf[RebasedDocument],
                             null)
      }
    }
    parent
  }

  /*@Nullable*/

  override def iterateAxis(axisNumber: Int): AxisIterator =
    new WrappingIterator(node.iterateAxis(axisNumber), this, null)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A RebasedNode is a view of a node, in a virtual tree that maps the base URI and/or
  * system ID to new values
  */
