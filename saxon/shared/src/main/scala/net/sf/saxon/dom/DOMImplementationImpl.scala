////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import org.w3c.dom.DOMException

import org.w3c.dom.DOMImplementation

import org.w3c.dom.Document

import org.w3c.dom.DocumentType




class DOMImplementationImpl extends DOMImplementation {

  def hasFeature(feature: String, version: String): Boolean =
    (feature.equalsIgnoreCase("XML") || feature.equalsIgnoreCase("Core")) &&
      (version == null || version.isEmpty || version.==("3.0") ||
        version.==("2.0") ||
        version.==("1.0"))

  def getFeature(feature: String, version: String): AnyRef = null

  def createDocumentType(qualifiedName: String,
                         publicId: String,
                         systemId: String): DocumentType = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

  /*@Nullable*/

  def createDocument(namespaceURI: String,
                     qualifiedName: String,
                     doctype: DocumentType): Document = {
    NodeOverNodeInfo.disallowUpdate()
    null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A simple implementation of the DOMImplementation interface, for use when accessing
  * Saxon tree structure using the DOM API.
  */
