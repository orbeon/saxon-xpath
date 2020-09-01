package net.sf.saxon.functions

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.Sink

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.om.Sequence

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.QuitParsingException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

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

class StreamAvailable extends SystemFunction with Callable {

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
