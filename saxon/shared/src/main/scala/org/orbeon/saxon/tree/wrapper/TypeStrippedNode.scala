////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.wrapper

import org.orbeon.saxon.event.Receiver
import org.orbeon.saxon.model.{BuiltInAtomicType, SchemaType, Type, Untyped}
import org.orbeon.saxon.om.{AtomicSequence, CopyOptions, NodeInfo}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.iter.AxisIterator
import org.orbeon.saxon.value.UntypedAtomicValue


/**
  * A TypeStrippedNode is a view of a node, in a virtual tree that has type
  * annotations stripped from it.
  */
object TypeStrippedNode {

  /*@NotNull*/
  def makeWrapper(node: NodeInfo,
                  docWrapper: TypeStrippedDocument,
                  parent: TypeStrippedNode): TypeStrippedNode = {
    val wrapper: TypeStrippedNode = new TypeStrippedNode(node, parent)
    wrapper.docWrapper = docWrapper
    wrapper
  }
}

class TypeStrippedNode ()
    extends AbstractVirtualNode
    with WrappingFunction {

  def this(node: NodeInfo, parent: TypeStrippedNode) = {
    this()
    this.node = node
    this.parent = parent
  }

  /*@NotNull*/
  def makeWrapper(node: NodeInfo, parent: VirtualNode): VirtualNode = {
    val wrapper = new TypeStrippedNode(node, parent.asInstanceOf[TypeStrippedNode])
    wrapper.docWrapper = this.docWrapper
    wrapper
  }

  /*@NotNull*/

  override def atomize(): AtomicSequence = new UntypedAtomicValue(getStringValueCS)

  /**
    * Get the type annotation
    *
    * @return the type annotation of the base node
    */
  override def getSchemaType: SchemaType =
    if (getNodeKind == Type.ELEMENT)
      Untyped.getInstance
    else
      BuiltInAtomicType.UNTYPED_ATOMIC

  override def equals(other: Any): Boolean =
    other match {
      case tsn: TypeStrippedNode =>
        node == tsn.node
      case _ =>
        node == other
    }

  override def compareOrder(other: NodeInfo): Int =
    other match {
      case tsn: TypeStrippedNode =>
        node.compareOrder(tsn.node)
      case _ =>
        node.compareOrder(other)
    }

  /*@Nullable*/

  def getParent: NodeInfo = {
    if (parent == null) {
      val realParent: NodeInfo = node.getParent
      if (realParent != null) {
        parent = TypeStrippedNode.makeWrapper(realParent,
                             docWrapper.asInstanceOf[TypeStrippedDocument],
                             null)
      }
    }
    parent
  }

  /*@Nullable*/
  override def iterateAxis(axisNumber: Int): AxisIterator =
    new WrappingIterator(node.iterateAxis(axisNumber), this, null)

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit =
    node.copy(out, copyOptions & ~CopyOptions.TYPE_ANNOTATIONS, locationId)

  override def isNilled: Boolean = false
}
