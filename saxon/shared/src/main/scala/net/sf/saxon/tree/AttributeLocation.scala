////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree

import net.sf.saxon.lib.Feature

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.s9api.Location

import net.sf.saxon.tree.util.Navigator

import scala.beans.{BeanProperty, BooleanBeanProperty}




class AttributeLocation(element: NodeInfo,
                        @BeanProperty var attributeName: StructuredQName)
    extends Location {

  @BeanProperty
  var systemId: String = element.getSystemId

  @BeanProperty
  var lineNumber: Int = element.getLineNumber

  @BeanProperty
  var columnNumber: Int = element.getColumnNumber

  @BeanProperty
  var elementName: StructuredQName = Navigator.getNodeName(element)

  @BeanProperty
  var elementNode: NodeInfo = _

  if (element.getConfiguration.getBooleanProperty(
        Feature.RETAIN_NODE_FOR_DIAGNOSTICS)) {
    this.elementNode = element
  }

  def this(element: NodeInfo,
           elementName: StructuredQName,
           attributeName: StructuredQName,
           location: Location) = {
    this(element, attributeName)
    this.systemId = location.getSystemId
    this.lineNumber = location.getLineNumber
    this.columnNumber = location.getColumnNumber
    this.elementName = elementName
    this.attributeName = attributeName
  }

  /**
    * Get the Public ID
    *
    * @return usually null
    */
  def getPublicId(): String = null

  /**
    * Get an immutable copy of this Location object. By default Location objects may be mutable, so they
    * should not be saved for later use. The result of this operation holds the same location information,
    * but in an immutable form.
    */
  def saveLocation(): Location = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Location corresponding to an attribute in a document (often a stylesheet)
  */
