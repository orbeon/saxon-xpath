////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.{PipelineConfiguration, ReceiverOption, SequenceWriter}
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om._

// ORBEON: Unused class.

/**
  * This class implements a Receiver that can receive xsl:message output and send it to a
  * user-supplied MessageListener.
  */
class MessageListener2Proxy  (private var listener: MessageListener2,
                                       pipe: PipelineConfiguration)
    extends SequenceWriter(pipe) {

  private var terminate: Boolean = _
  private var locationId: Location = _
  private var errorCode: StructuredQName = _

// also because we store several messages in a single TinyTree; and because we fail to condense the tree.
  this.treeModel = TreeModel.LINKED_TREE

  def getMessageListener: MessageListener2 = listener

  override def startDocument(properties: Int): Unit = {
    terminate = ReceiverOption.contains(properties, ReceiverOption.TERMINATE)
    locationId = null
    errorCode = null
    super.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (this.locationId == null) {
      this.locationId = location
    }
    super.startElement(elemName,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
  }

  override def characters(s: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (this.locationId == null) {
      this.locationId = locationId
    }
    super.characters(s, locationId, properties)
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit =
    if (target.==("error-code") && errorCode == null)
      errorCode = StructuredQName.fromEQName(data)
    else
      super.processingInstruction(target, data, locationId, properties)

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    if (this.locationId == null)
      this.locationId = locationId
    super.append(item, locationId, copyNamespaces)
  }

  def write(item: Item): Unit = {
    var loc: Location = null
    loc = if (locationId == null) Loc.NONE else locationId.saveLocation
    listener.message(new XdmNode(item.asInstanceOf[NodeInfo]),
                     new QName(errorCode),
                     terminate,
                     loc)
  }

}
