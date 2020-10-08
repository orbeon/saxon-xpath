////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.om.DocumentURI._


object DocumentURI {

//  val CASE_BLIND_FILES: Boolean = new File("a") == new File("A")

  def normalizeURI( uri: String): String = {
    var uriString = uri
    if (uri == null)
     return null
    if (uriString.startsWith("FILE:"))
      uriString = "file:" + uri.substring(5)
    if (uriString.startsWith("file:")) {
      if (uriString.startsWith("file:///"))
        uriString = "file:/" + uriString.substring(8)
      // ORBEON: No `File` support.
//      if (CASE_BLIND_FILES) {
//        uriString = uriString.toLowerCase()
//      }
    }
    uriString
  }
}

/**
  * This class encapsulates a string used as the value of the document-uri() property of a document,
  * together with a normalized representation of the string used for equality comparisons. The idea
  * is that on Windows systems, document URIs are compared using case-blind comparison, but the original
  * case is retained for display purposes.
  */
class DocumentURI(uri: String) {

  private val displayValue: String = uri
  private val normalizedValue: String = normalizeURI(uri)

  if (uri == null)
    throw new NullPointerException("uri")

  override def toString: String = displayValue
  override def equals(obj: Any): Boolean = obj match {
    case obj: DocumentURI => normalizedValue == obj.normalizedValue
    case _ => false
  }

  override def hashCode: Int = normalizedValue.hashCode
}

