////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import javax.xml.transform.Result

trait OutputURIResolver {
  def newInstance(): OutputURIResolver
  def resolve(href: String, base: String): Result
  def close(result: Result): Unit
}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface defines an OutputURIResolver. This is a counterpart to the JAXP
  * URIResolver, but is used to map the URI of a secondary result document to a Result object
  * which acts as the destination for the new document.
  * <p>From Saxon 9.9 this interface is obsolescent. It is unable to handle the full flexibility
  * of XSLT 3.0, for example it cannot handle raw output, JSON serialization, or the item-separator
  * serialization property. A new mechanism has therefore been introduced. This has a low-level
  * interface {@link XsltController#setResultDocumentResolver(ResultDocumentResolver)}, and a high-level
  * counterpart at the s9api level, using the setResultDocumentHandler() method on {@link org.orbeon.saxon.s9api.Xslt30Transformer}
  * and {@link org.orbeon.saxon.s9api.XsltTransformer}.</p>
  */
