////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.ResourceCollection

import net.sf.saxon.om.Item

import net.sf.saxon.om.LazySequence

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.UncheckedXPathException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import java.io.Closeable

import java.io.IOException

import java.net.URI

import java.net.URISyntaxException

import java.util.Iterator




class UriCollection extends SystemFunction {

  private def getUris(href: String, context: XPathContext): SequenceIterator = {
    val rCollection: ResourceCollection =
      context.getConfiguration.getCollectionFinder
        .findCollection(context, href)
    if (rCollection == null) {
// Should not happen, we're calling user code so we check for it.
      val err = new XPathException(
        "No collection has been defined for href: " + (if (href == null) ""
                                                       else href))
      err.setErrorCode("FODC0002")
      err.setXPathContext(context)
      throw err
    }
    val sources: Iterator[String] = rCollection.getResourceURIs(context)
    new SequenceIterator {
      def next(): AnyURIValue =
        if (sources.hasNext) {
          new AnyURIValue(sources.next())
        } else {
          null
        }

      override def close(): Unit = {
        if (sources.isInstanceOf[Closeable]) {
          sources.asInstanceOf[Closeable].close()
        }
      }
    }
  }

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    if (arguments.length == 0) {
      getDefaultUriCollection(context)
    } else {
      val arg: Item = arguments(0).head
      if (arg == null) {
        getDefaultUriCollection(context)
      }
      val href: String = arg.getStringValue
      var hrefURI: URI = null
      hrefURI = new URI(href)
      if (!hrefURI.isAbsolute) {
        val staticBaseUri: URI = getRetainedStaticContext.getStaticBaseUri
        if (staticBaseUri == null) {
          throw new XPathException("No base URI available for uri-collection",
                                   "FODC0002")
        }
        hrefURI = staticBaseUri.resolve(hrefURI)
      }
      new LazySequence(getUris(hrefURI.toString, context))
    }

  private def getDefaultUriCollection(context: XPathContext): Sequence = {
    val href: String = context.getConfiguration.getDefaultCollection
    if (href == null) {
      throw new XPathException("No default collection has been defined",
                               "FODC0002")
    } else {
      new LazySequence(getUris(href, context))
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implement the fn:uri-collection() function (new in XQuery 3.0/XSLT 3.0). This is responsible for calling the
  * registered {@link net.sf.saxon.lib.CollectionFinder}. For the effect of the default
  * system-supplied CollectionFinder, see {@link net.sf.saxon.resource.StandardCollectionFinder}
  */
