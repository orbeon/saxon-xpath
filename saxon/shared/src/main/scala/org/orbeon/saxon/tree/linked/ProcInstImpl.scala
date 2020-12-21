////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.NoNamespaceName

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue




class ProcInstImpl(var name: String, var content: String) extends NodeImpl {

  var systemId: String = _

  var lineNumber: Int = -1

  var columnNumber: Int = -1

  /**
    * Get the name of the node. Returns null for an unnamed node
    *
    * @return the name of the node
    */
  override def getNodeName: NodeName = new NoNamespaceName(name)

  def getStringValue: String = content

  /*@NotNull*/

  override def atomize(): AtomicSequence = new StringValue(getStringValue)

  def getNodeKind: Int = Type.PROCESSING_INSTRUCTION

  def setLocation(uri: String, lineNumber: Int, columnNumber: Int): Unit = {
    this.systemId = uri
    this.lineNumber = lineNumber
    this.columnNumber = columnNumber
  }

  override def getSystemId: String = systemId

  override def getLineNumber: Int = lineNumber

  override def getColumnNumber(): Int = columnNumber

  override def copy(out: Receiver, copyOptions: Int, locationId: Location): Unit = {
    out.processingInstruction(getLocalPart,
                              content,
                              locationId,
                              ReceiverOption.NONE)
  }

  override def rename(newNameCode: NodeName): Unit = {
    name = newNameCode.getLocalPart
  }

  def replaceStringValue(stringValue: CharSequence): Unit = {
    content = stringValue.toString
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * ProcInstImpl is an implementation of ProcInstInfo used by the Propagator to construct
  * its trees.
  *
  * @author Michael H. Kay
  */
