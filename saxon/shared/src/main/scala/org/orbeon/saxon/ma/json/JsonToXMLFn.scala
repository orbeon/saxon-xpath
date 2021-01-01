////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import java.util.Map

import org.orbeon.saxon.expr.{StaticProperty, XPathContext}
import org.orbeon.saxon.functions.{OptionsParameter, SystemFunction}
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.model.SpecificFunctionType
import org.orbeon.saxon.om.{Item, Sequence}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{BooleanValue, EmptySequence, SequenceType}

//remove if not needed

object JsonToXMLFn {

  val jsonToXmlOptions: OptionsParameter = new OptionsParameter()

  var OPTION_DETAILS: OptionsParameter = jsonToXmlOptions

  val fallbackType: SpecificFunctionType = new SpecificFunctionType(
    Array(SequenceType.SINGLE_STRING),
    SequenceType.SINGLE_STRING)

  jsonToXmlOptions.addAllowedOption("liberal",
                                    SequenceType.SINGLE_BOOLEAN,
                                    BooleanValue.FALSE)

  jsonToXmlOptions.addAllowedOption("duplicates",
                                    SequenceType.SINGLE_STRING,
                                    null)

  jsonToXmlOptions.setAllowedValues("duplicates",
                                    "FOJS0005",
                                    "reject",
                                    "use-first",
                                    "retain")

  jsonToXmlOptions.addAllowedOption("validate",
                                    SequenceType.SINGLE_BOOLEAN,
                                    BooleanValue.FALSE)

  jsonToXmlOptions.addAllowedOption("escape",
                                    SequenceType.SINGLE_BOOLEAN,
                                    BooleanValue.FALSE)

  jsonToXmlOptions.addAllowedOption(
    "fallback",
    SequenceType.makeSequenceType(fallbackType, StaticProperty.EXACTLY_ONE),
    null)

}

class JsonToXMLFn extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val arg0: Item = arguments(0).head
    if (arg0 == null) {
      return EmptySequence.getInstance
    }
    val input: String = arg0.getStringValue
    var options: MapItem = null
    if (getArity == 2) {
      options = arguments(1).head.asInstanceOf[MapItem]
    }
    val result: Item = eval(input, options, context)
    if (result == null) EmptySequence.getInstance else result
  }

  /**
    * Parse the JSON string according to supplied options
    *
    * @param input   JSON input string
    * @param options options for the conversion as a map of xs:string : value pairs
    * @param context XPath evaluation context
    * @return the result of the parsing, as an XML element
    * @throws XPathException if the syntax of the input is incorrect
    */
   def eval(input: String,
                     options: MapItem,
                     context: XPathContext): Item = {
    val parser: JsonParser = new JsonParser()
    var flags: Int = 0
    var checkedOptions: Map[String, Sequence] = null
    if (options != null) {
      checkedOptions =
        getDetails.optionDetails.processSuppliedOptions(options, context)
      flags = JsonParser.getFlags(checkedOptions, context, allowValidate = true)
      if ((flags & JsonParser.DUPLICATES_LAST) != 0) {
        throw new XPathException(
          "json-to-xml: duplicates=use-last is not allowed",
          "FOJS0005")
      }
      if ((flags & JsonParser.DUPLICATES_SPECIFIED) == 0) {
        if ((flags & JsonParser.VALIDATE) != 0) {
          flags |= JsonParser.DUPLICATES_REJECTED
        } else {
          flags |= JsonParser.DUPLICATES_RETAINED
        }
      }
    } else {
      flags = JsonParser.DUPLICATES_RETAINED
    }
    val handler: JsonHandlerXML =
      new JsonHandlerXML(context, getStaticBaseUriString, flags)
    if (options != null) {
      handler.setFallbackFunction(checkedOptions, context)
    }
    parser.parse(input, flags, handler, context)
    handler.getResult
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Implements the json-to-xml function defined in XSLT 3.0.
  */
// Copyright (c) 2011-2020 Saxonica Limited
