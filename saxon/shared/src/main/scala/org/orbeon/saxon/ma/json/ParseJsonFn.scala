////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.OptionsParameter

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.SequenceType

import org.orbeon.saxon.value.StringValue

import java.util.Map

import ParseJsonFn._




object ParseJsonFn {

  val parseJsonOptions: OptionsParameter = new OptionsParameter()

  var OPTION_DETAILS: OptionsParameter = parseJsonOptions

  val fallbackType: SpecificFunctionType = new SpecificFunctionType(
    Array(SequenceType.SINGLE_STRING),
    SequenceType.SINGLE_STRING)

  parseJsonOptions.addAllowedOption("liberal",
                                    SequenceType.SINGLE_BOOLEAN,
                                    BooleanValue.FALSE)

  parseJsonOptions.addAllowedOption("duplicates",
                                    SequenceType.SINGLE_STRING,
                                    new StringValue("use-first"))

  parseJsonOptions.setAllowedValues("duplicates",
                                    "FOJS0005",
                                    "reject",
                                    "use-first",
                                    "use-last")

  parseJsonOptions.addAllowedOption("escape",
                                    SequenceType.SINGLE_BOOLEAN,
                                    BooleanValue.FALSE)

  parseJsonOptions.addAllowedOption(
    "fallback",
    SequenceType.makeSequenceType(fallbackType, StaticProperty.EXACTLY_ONE),
    null)

  def parse(input: String,
            options: Map[String, Sequence],
            context: XPathContext): Item = {
    val parser: JsonParser = new JsonParser()
    var flags: Int = 0
    if (options != null) {
      flags = JsonParser.getFlags(options, context, allowValidate = false)
    }
    val handler: JsonHandlerMap = new JsonHandlerMap(context, flags)
    if ((flags & JsonParser.DUPLICATES_RETAINED) != 0) {
      throw new XPathException("parse-json: duplicates=retain is not allowed",
                               "FOJS0005")
    }
    if ((flags & JsonParser.DUPLICATES_SPECIFIED) == 0) {
      flags |= JsonParser.DUPLICATES_FIRST
    }
    if (options != null) {
      handler.setFallbackFunction(options, context)
    }
    parser.parse(input, flags, handler, context)
    handler.getResult.head
  }

}

/**
  * Implements the parse-json function, as defined in XPath 3.1
  *
  * The parsing code, and the handling of options is shared with the json-to-xml function.
  */
class ParseJsonFn extends JsonToXMLFn {

  /**
    * Parse the JSON string according to supplied options
    *
    * @param input   JSON input string
    * @param options options for the conversion as a map of xs:string : value pairs
    * @param context XPath evaluation context
    * @return the result of the parsing, as an XML element
    * @throws XPathException if the syntax of the input is incorrect
    */
   override def eval(input: String,
                              options: MapItem,
                              context: XPathContext): Item = {
    var checkedOptions: Map[String, Sequence] = null
    if (options != null) {
      checkedOptions =
        getDetails.optionDetails.processSuppliedOptions(options, context)
    }
    parse(input, checkedOptions, context)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
