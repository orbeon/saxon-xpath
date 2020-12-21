////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import java.io.Reader
import java.net.URI
import java.util.Map
import java.util.function.IntPredicate

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.{SystemFunction, UnparsedTextFunction}
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.om.{Item, Sequence}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.value.EmptySequence
import org.orbeon.saxon.z.IntSetPredicate

//remove if not needed

class JsonDoc extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val arg0: Item = arguments(0).head
    if (arg0 == null) {
      EmptySequence.getInstance
    }
    val href: String = arg0.getStringValue
    val config: Configuration = context.getConfiguration
// allow non-XML characters - bug 3911
    val checker: IntPredicate = IntSetPredicate.ALWAYS_TRUE
    val absoluteURI: URI = UnparsedTextFunction.getAbsoluteURI(
      href,
      getStaticBaseUriString,
      context)
// for now
    val encoding: String = "UTF-8"
    var reader: Reader = null
    try reader = context.getController.getUnparsedTextURIResolver
      .resolve(absoluteURI, encoding, config)
    catch {
      case err: XPathException => {
        err.maybeSetErrorCode("FOUT1170")
        throw err
      }

    }
    var content: CharSequence = null
    try content = UnparsedTextFunction.readFile(checker, reader)
    catch {
      case encErr: java.io.UnsupportedEncodingException => {
        val e: XPathException =
          new XPathException("Unknown encoding " + Err.wrap(encoding), encErr)
        e.setErrorCode("FOUT1190")
        throw e
      }

      case ioErr: java.io.IOException =>
        throw UnparsedTextFunction.handleIOError(absoluteURI, ioErr, context)

    }
    var checkedOptions: Map[String, Sequence] = null
    if (getArity == 2) {
      val options: MapItem = arguments(1).head.asInstanceOf[MapItem]
      checkedOptions =
        getDetails.optionDetails.processSuppliedOptions(options, context)
    } else {
      checkedOptions = ParseJsonFn.OPTION_DETAILS.getDefaultOptions
    }
    val result: Item =
      ParseJsonFn.parse(content.toString, checkedOptions, context)
    if (result == null) EmptySequence.getInstance else result
  }
// Use the URI machinery to validate and resolve the URIs
// Use the URI machinery to validate and resolve the URIs

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implements the json-to-xml function defined in XSLT 3.0.
  */
// Copyright (c) 2018-2020 Saxonica Limited
