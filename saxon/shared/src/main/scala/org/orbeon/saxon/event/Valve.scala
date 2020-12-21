////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException




class Valve(private var testNamespace: String,
            primary: Receiver,
            secondary: Receiver)
    extends ProxyReceiver(primary) {

  private var started: Boolean = false

  private var alternativeReceiver: Receiver = secondary

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (!started) {
      started = true
      if (elemName.getURI == testNamespace) {
        alternativeReceiver.open()
        alternativeReceiver.startDocument(ReceiverOption.NONE)
        try getNextReceiver.close()
        catch {
          case _: XPathException =>

        }
        super.setUnderlyingReceiver(alternativeReceiver)
      }
    }
    super.startElement(elemName,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
  }

  def wasDiverted(): Boolean = getNextReceiver == alternativeReceiver

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Valve is a general-purpose component for use in a pipeline of receivers. It selects an alternative
  * destination for the pipeline events based on the namespace of the first startElement event.
  *
  * There is a primary destination which is selected initially. If the namespace of the first element has
  * a given value, then subsequent output is sent to an alternative destination.
  *
  */
