////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.SequenceNormalizer

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.serialize.SerializationProperties

import org.orbeon.saxon.trans.SaxonErrorCode

import org.orbeon.saxon.trans.XPathException




trait ResultDocumentResolver {

  def resolve(context: XPathContext,
              href: String,
              baseUri: String,
              properties: SerializationProperties): Receiver

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The <tt>ResultDocumentResolver</tt> interface may be implemented by a user application;
  * it is a callback that is called whenever an xsl:result-document instruction
  * is executed.
  * <p>There is a single method: {@link #resolve(XPathContext, String, String, SerializationProperties)}.
  * Saxon calls this method supplying the dynamic evaluation context, together with the value of the href
  * attribute and the output base URI for the transformation.</p>
  * <p>The result of the callback is an application-supplied instance of the
  * {@link Outputter} interface. Saxon will send events to this {@code Receiver},
  * representing the <b>raw</b> results of the {@code xsl:result-document} instruction.
  * If the application wants results in the form of a document node generated using
  * sequence normalization, then it must include a {@link SequenceNormalizer} in the
  * output pipeline.</p>
  * <p>The {@code ResultDocumentResolver} is called for every {@code xsl:result-document}
  * instruction whether or not it specifies an {@code href} attribute.</p>
  * <p>The implementation must be thread-safe (calls to {@code xsl:result-document} may
  * occur in several threads concurrently). The returned {@code Receiver}
  * may be called in a different thread.</p>
  * <p>If the application wishes to take action when the {@code xsl:result-document} instruction
  * finishes, that is, when the results are available for use, then it should intercept the
  * {@link Outputter#close()} call on the returned {@code Receiver}. This can be done
  * (for example) by adding a {@link org.orbeon.saxon.event.CloseNotifier} to the output pipeline,
  * or by use of the {@link SequenceNormalizer#onClose} method.</p>
  * <p>This interface supersedes the {@link OutputURIResolver} interface provided in earlier
  * Saxon releases. The {@code OutputURIResolver} was limited because it did not have access
  * to the dynamic context, nor to the serialization parameters, and it did not handle
  * raw output as required by the XSLT 3.0 specification, needed in particular to create
  * JSON output documents.</p>
  * @since 9.9
  */
