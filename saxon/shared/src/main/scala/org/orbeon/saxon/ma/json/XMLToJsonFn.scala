////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.json

import org.orbeon.saxon.event.DocumentValidator

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.functions.OptionsParameter

import org.orbeon.saxon.functions.PushableFunction

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.CharSequenceConsumer

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.EmptySequence

import org.orbeon.saxon.value.SequenceType

import org.orbeon.saxon.value.StringValue

import java.util.Map

import XMLToJsonFn._

object XMLToJsonFn {

  def makeOptionsParameter(): OptionsParameter = {
    val xmlToJsonOptions: OptionsParameter = new OptionsParameter()
    xmlToJsonOptions.addAllowedOption("indent",
      SequenceType.SINGLE_BOOLEAN,
      BooleanValue.FALSE)
    val nfType = new org.orbeon.saxon.model.SpecificFunctionType(
      Array(SequenceType.SINGLE_STRING),
      SequenceType.SINGLE_STRING
    )
    xmlToJsonOptions.addAllowedOption(
      "Q{" + org.orbeon.saxon.lib.NamespaceConstant.SAXON + "}number-formatter",
      SequenceType.makeSequenceType(nfType, org.orbeon.saxon.expr.StaticProperty.ALLOWS_ZERO_OR_ONE),
      EmptySequence.getInstance
    )
    xmlToJsonOptions
  }

  private class Options {
    var indent: Boolean = _
    var numberFormatter: org.orbeon.saxon.om.Function = _
  }

}

/**
 * Implement the XML to JSON conversion as a built-in function - fn:xml-to-json()
 */
class XMLToJsonFn extends SystemFunction with PushableFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val xml: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (xml == null) {
     return EmptySequence.getInstance
    }
    val options = getOptions(context, arguments)
    val pipe: PipelineConfiguration =
      context.getController.makePipelineConfiguration
    pipe.xpathContext = context
    val stringBuffer: FastStringBuffer = new FastStringBuffer(2048)
    convertToJson(xml, stringBuffer, options, context)
    new StringValue(stringBuffer.condense())
  }

  private def getOptions(context: XPathContext,
                         arguments: Array[Sequence]): Options = {
    if (getArity > 1) {
      val suppliedOptions: MapItem = arguments(1).head.asInstanceOf[MapItem]
      val options: Map[String, Sequence] = getDetails.optionDetails
        .processSuppliedOptions(suppliedOptions, context)
      val o = new Options
      o.indent = options.get("indent").head.asInstanceOf[BooleanValue].getBooleanValue
      val qn = options.get("Q{" + org.orbeon.saxon.lib.NamespaceConstant.SAXON + "}number-formatter")
      if (qn != null)
        o.numberFormatter = qn.head.asInstanceOf[org.orbeon.saxon.om.Function]
      o
    } else {
      new Options
    }
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val xml: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (xml != null) {
      val options = getOptions(context, arguments)
      val pipe: PipelineConfiguration =
        context.getController.makePipelineConfiguration
      pipe.xpathContext = context
      convertToJson(xml, destination.getStringReceiver(false), options, context)
    }
  }

  private def convertToJson(xml: NodeInfo,
                            output: CharSequenceConsumer,
                            options: Options,
                            context: XPathContext): Unit = {
    val pipe: PipelineConfiguration =
      context.getController.makePipelineConfiguration
    pipe.xpathContext = context
    val receiver: JsonReceiver = new JsonReceiver(pipe, output)
    receiver.setIndenting(options.indent)
    if (options.numberFormatter != null)
      receiver.setNumberFormatter(options.numberFormatter)
    var r: Receiver = receiver
    if (xml.getNodeKind == Type.DOCUMENT) {
      r = new DocumentValidator(r, "FOJS0006")
    }
    r.open()
    xml.copy(r, 0, Loc.NONE)
    r.close()
  }

  override def getStreamerName: String = "XmlToJsonFn"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
