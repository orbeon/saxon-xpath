////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.Builder

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType




trait MutableNodeInfo extends NodeInfo {

  def setTypeAnnotation(`type`: SchemaType): Unit

  def insertChildren(source: Array[NodeInfo],
                     atStart: Boolean,
                     inherit: Boolean): Unit

  def insertSiblings(source: Array[NodeInfo],
                     before: Boolean,
                     inherit: Boolean): Unit

  def setAttributes(attributes: AttributeMap): Unit

  def removeAttribute(attribute: NodeInfo): Unit

  def addAttribute(name: NodeName,
                   attType: SimpleType,
                   value: CharSequence,
                   properties: Int): Unit

  def removeNamespace(prefix: String): Unit = ()
// default: no action
// default: no action

  def addNamespace(prefix: String, uri: String): Unit = ()
// default: no action
// default: no action

  def delete(): Unit

  def isDeleted: Boolean

  def replace(replacement: Array[NodeInfo], inherit: Boolean): Unit

  def replaceStringValue(stringValue: CharSequence): Unit

  def rename(newName: NodeName): Unit

  def addNamespace(nscode: NamespaceBinding): Unit

  def removeTypeAnnotation(): Unit

  def newBuilder(): Builder

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An extension of the NodeInfo interface defining a node that can be updated. The updating methods are
  * closely modelled on the updating primitives defined in the XQuery Update specification, however, their
  * semantics should not be assumed to be identical to those primitives.
  * <p>These primitives may leave the tree in a state where the type annotations of nodes do not correctly
  * describe the structure of the nodes. It is the caller's responsibility to maintain the integrity of
  * type annotations.</p>
  * <p>This interface was introduced in Saxon 9.1 and modified in Saxon 9.2.
  * Some aspects of the semantics are not clearly specified, for example the effect of each
  * operation on namespace bindings, and such details may change in the future. The primary purpose
  * of this interface is as an internal interface for use by XQuery Update; applications use it (or
  * implement it) at their own risk.</p>
  */
