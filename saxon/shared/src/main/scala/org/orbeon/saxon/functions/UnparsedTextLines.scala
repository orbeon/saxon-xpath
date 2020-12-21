////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import java.net.URI

import org.orbeon.saxon.expr.{Callable, XPathContext}
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.om.{Sequence, SequenceIterator, SequenceTool}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.EmptyIterator
import org.orbeon.saxon.value.StringValue


class UnparsedTextLines extends UnparsedTextFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val hrefVal = arguments(0).head.asInstanceOf[StringValue]
    val encoding = if (getArity == 2) arguments(1).head.getStringValue else null
    try
      SequenceTool.toLazySequence(evalUnparsedTextLines(hrefVal, encoding, context))
    catch {
      case e: XPathException =>
        if (getArity == 2 && e.getErrorCodeLocalPart.==("FOUT1200"))
          e.setErrorCode("FOUT1190")
        throw e
    }
  }

  private def evalUnparsedTextLines(hrefVal: StringValue,
                                    encoding: String,
                                    context: XPathContext): SequenceIterator = {
    if (hrefVal == null)
      return EmptyIterator.ofAtomic()

    val href = hrefVal.getStringValue
    val stable = context.getConfiguration.getBooleanProperty(Feature.STABLE_UNPARSED_TEXT)
    if (stable) {
      // if results have to be stable, the text has to be read into memory and cached
      val content = UnparsedText.evalUnparsedText(
        hrefVal,
        getStaticBaseUriString,
        encoding,
        context)
      assert(content != null)
      val abs: URI = UnparsedTextFunction.getAbsoluteURI(
        href,
        getStaticBaseUriString,
        context)
      // ORBEON: LineNumberReader
      ???
//      val reader = new LineNumberReader(new StringReader(content.getStringValue))
//      new UnparsedTextIterator(reader, abs, context, encoding, null)
    } else {
      // with unstable results, we avoid reading the whole file into memory
      val absoluteURI: URI = UnparsedTextFunction.getAbsoluteURI(
        href,
        getRetainedStaticContext.getStaticBaseUriString,
        context)
      // ORBEON: LineNumberReader
      ???
//      new UnparsedTextIterator(absoluteURI, context, encoding, null)
    }
  }
}
