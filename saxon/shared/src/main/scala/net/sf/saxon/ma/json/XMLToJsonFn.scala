////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.ma.json

import net.sf.saxon.event.DocumentValidator

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.functions.OptionsParameter

import net.sf.saxon.functions.PushableFunction

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.ma.map.MapItem

import net.sf.saxon.model.Type

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.CharSequenceConsumer

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue

import java.util.Map

import XMLToJsonFn._


object XMLToJsonFn {

  def makeOptionsParameter(): OptionsParameter = {
    val xmlToJsonOptions: OptionsParameter = new OptionsParameter()
    xmlToJsonOptions.addAllowedOption("indent",
      SequenceType.SINGLE_BOOLEAN,
      BooleanValue.FALSE)
    xmlToJsonOptions
  }

}

/**
 * Implement the XML to JSON conversion as a built-in function - fn:xml-to-json()
 */
class XMLToJsonFn extends SystemFunction with PushableFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val xml: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (xml == null) {
      EmptySequence.getInstance
    }
    val indent: Boolean = isindentingRequested(context, arguments)
    val pipe: PipelineConfiguration =
      context.getController.makePipelineConfiguration
    pipe.setXPathContext(context)
    val stringBuffer: FastStringBuffer = new FastStringBuffer(2048)
    convertToJson(xml, stringBuffer, indent, context)
    new StringValue(stringBuffer.condense())
  }

  private def isindentingRequested(context: XPathContext,
                                   arguments: Array[Sequence]): Boolean = {
    if (getArity > 1) {
      val suppliedOptions: MapItem = arguments(1).head.asInstanceOf[MapItem]
      val options: Map[String, Sequence] = getDetails.optionDetails
        .processSuppliedOptions(suppliedOptions, context)
      options.get("indent").head.asInstanceOf[BooleanValue].getBooleanValue
    }
    false
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val xml: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (xml != null) {
      val indent: Boolean = isindentingRequested(context, arguments)
      val pipe: PipelineConfiguration =
        context.getController.makePipelineConfiguration
      pipe.setXPathContext(context)
      convertToJson(xml, destination.getStringReceiver(false), indent, context)
    }
  }

  private def convertToJson(xml: NodeInfo,
                            output: CharSequenceConsumer,
                            indent: Boolean,
                            context: XPathContext): Unit = {
    val pipe: PipelineConfiguration =
      context.getController.makePipelineConfiguration
    pipe.setXPathContext(context)
    val receiver: JsonReceiver = new JsonReceiver(pipe, output)
    receiver.setIndenting(indent)
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
