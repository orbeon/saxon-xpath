////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.tree.iter.UnparsedTextIterator

import org.orbeon.saxon.value.StringValue

import java.io.LineNumberReader

import java.io.StringReader

import java.net.URI


class UnparsedTextLines extends UnparsedTextFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val hrefVal: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val encoding: String =
      if (getArity == 2) arguments(1).head.getStringValue else null
    try SequenceTool.toLazySequence(
      evalUnparsedTextLines(hrefVal, encoding, context))
    catch {
      case e: XPathException => {
        if (getArity == 2 && e.getErrorCodeLocalPart.==("FOUT1200")) {
          e.setErrorCode("FOUT1190")
        }
        throw e
      }

    }
  }

  private def evalUnparsedTextLines(
                                     hrefVal: StringValue,
                                     encoding: String,
                                     context: XPathContext): SequenceIterator = {
    if (hrefVal == null) {
      EmptyIterator.ofAtomic()
    }
    val href: String = hrefVal.getStringValue
    val stable: Boolean =
      context.getConfiguration.getBooleanProperty(Feature.STABLE_UNPARSED_TEXT)
    if (stable) {
      // if results have to be stable, the text has to be read into memory and cached
      val content: StringValue = UnparsedText.evalUnparsedText(
        hrefVal,
        getStaticBaseUriString,
        encoding,
        context)
      assert(content != null)
      val abs: URI = UnparsedTextFunction.getAbsoluteURI(
        href,
        getStaticBaseUriString,
        context)
      val reader: LineNumberReader = new LineNumberReader(
        new StringReader(content.getStringValue))
      new UnparsedTextIterator(reader, abs, context, encoding, null)
    } else {
      // with unstable results, we avoid reading the whole file into memory
      val absoluteURI: URI = UnparsedTextFunction.getAbsoluteURI(
        href,
        getRetainedStaticContext.getStaticBaseUriString,
        context)
      new UnparsedTextIterator(absoluteURI, context, encoding, null)
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012-2020 Saxonica Limited
