package org.orbeon.saxon.functions

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.Sink

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.QuitParsingException

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import StreamAvailable._

object StreamAvailable {

  private class StreamTester(pipe: PipelineConfiguration)
    extends ProxyReceiver(new Sink(pipe)) {

    override def startElement(elemName: NodeName,
                              `type`: SchemaType,
                              attributes: AttributeMap,
                              namespaces: NamespaceMap,
                              location: Location,
                              properties: Int): Unit = {
      throw new QuitParsingException(false)
    }

  }

}

class StreamAvailable extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val result: Boolean =
      isAvailable(arguments(0).head.getStringValue, context)
    BooleanValue.get(result)
  }

  private def isAvailable(uri: String, context: XPathContext): Boolean = {
    try {
      val tester: Receiver = new StreamTester(
        context.getConfiguration.makePipelineConfiguration)
      DocumentFn.sendDoc(uri,
        getRetainedStaticContext.getStaticBaseUriString,
        context,
        null,
        tester,
        new ParseOptions())
    } catch {
      case e: QuitParsingException => return true
      case e: XPathException => return false
    }
    false
  }

}
