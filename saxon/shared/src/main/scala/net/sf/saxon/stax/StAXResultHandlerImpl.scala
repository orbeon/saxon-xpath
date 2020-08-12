////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.stax

import net.sf.saxon.event.Receiver

import net.sf.saxon.lib.StAXResultHandler

import javax.xml.transform.Result

import javax.xml.transform.stax.StAXResult

import java.util.Properties




/**
  * StAxResultHandler is used to allow a transformation result to be supplied as a StAXResult.
  * It is loaded dynamically in case StAXResult is not present in the JDK.
  */
class StAXResultHandlerImpl extends StAXResultHandler {

  def getReceiver(result: Result, properties: Properties): Receiver =
    if (result.asInstanceOf[StAXResult].getXMLStreamWriter != null) {
      new ReceiverToXMLStreamWriter(
        result.asInstanceOf[StAXResult].getXMLStreamWriter)
    } else if (result.asInstanceOf[StAXResult].getXMLEventWriter != null) {
      throw new UnsupportedOperationException(
        "XMLEventWriter is currently not supported as a Saxon output destination")
    } else {
      throw new IllegalStateException(
        "StAXResult contains neither an XMLStreamWriter nor XMLEventWriter")
    }

}

// Copyright (c) 2015-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
