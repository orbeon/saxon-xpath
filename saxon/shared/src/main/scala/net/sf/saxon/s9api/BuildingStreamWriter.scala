////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import javax.xml.stream.XMLStreamWriter




trait BuildingStreamWriter extends XMLStreamWriter {

  def getDocumentNode(): XdmNode

  def setCheckValues(check: Boolean): Unit

  def isCheckValues(): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A BuildingStreamWriter allows a document to be constructed by calling the methods defined in the
  * {@link javax.xml.stream.XMLStreamWriter} interface; after the document has been constructed, its root
  * node may be retrieved by calling the <code>getDocumentNode()</code> method.
  * <p>The class will attempt to generate namespace prefixes where none have been supplied, unless the
  * <code>inventPrefixes</code> option is set to false. The preferred mode of use is to call the versions
  * of <code>writeStartElement</code> and <code>writeAttribute</code> that supply the prefix, URI, and
  * local name in full. If the prefix is omitted, the class attempts to invent a prefix. If the URI is
  * omitted, the name is assumed to be in no namespace. The <code>writeNamespace</code> method should be
  * called only if there is a need to declare a namespace prefix that is not used on any element or
  * attribute name.</p>
  * <p>The class will check all names, URIs, and character content for conformance against XML well-formedness
  * rules unless the <code>checkValues</code> option is set to false.</p>
  * <p>A <code>BuildingStreamWriter</code> for a particular object model can be obtained by calling
  * {@link net.sf.saxon.s9api.DocumentBuilder#newBuildingStreamWriter()}.</p>
  */
