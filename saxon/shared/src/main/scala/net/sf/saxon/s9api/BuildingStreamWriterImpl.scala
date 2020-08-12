////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.event.Builder

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.StreamWriterToReceiver

import net.sf.saxon.trans.XPathException




class BuildingStreamWriterImpl(receiver: Receiver, var builder: Builder)
    extends StreamWriterToReceiver(receiver)
    with BuildingStreamWriter {

  builder.open()

  /*@Nullable*/

  def getDocumentNode(): XdmNode = {
    builder.close()
    new XdmNode(builder.getCurrentRoot)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an implementation of {@link javax.xml.stream.XMLStreamWriter}, allowing
  * a document to be constructed by means of a series of XMLStreamWriter method calls such
  * as writeStartElement(), writeAttribute(), writeCharacters(), and writeEndElement().
  * <p>The detailed way in which this class is packaged was carefully designed to ensure that
  * if the functionality is not used, the <code>DocumentBuilder</code> would still be usable under
  * JDK 1.5 (which does not include javax.xml.stream interfaces).</p>
  */
