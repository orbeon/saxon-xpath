package net.sf.saxon.event

import net.sf.saxon.utils.Controller

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XsltController

import javax.xml.transform.TransformerException

import scala.beans.{BeanProperty, BooleanBeanProperty}


class TransformerReceiver(var controller: XsltController) extends ProxyReceiver(controller.makeBuilder) {

  private var builder: Builder = getNextReceiver.asInstanceOf[Builder]

  @BeanProperty
  var destination: Receiver = _

  this.builder.setUseEventLocation(false)

  override def open(): Unit = {
    builder.setSystemId(systemId)
    var stripper: Receiver = controller.makeStripper(builder)
   /* if (controller.isStylesheetStrippingTypeAnnotations) {
      stripper = controller.getConfiguration.getAnnotationStripper(stripper)
    }*/
    super.setUnderlyingReceiver(stripper)
    nextReceiver.open()
  }

  override def setSystemId(systemId: String): Unit = {
    super.setSystemId(systemId)
    controller.setBaseOutputURI(systemId)
  }

  override def close(): Unit = {
    if (builder == null) {} else {
      nextReceiver.close()
      val doc: NodeInfo = builder.getCurrentRoot
      builder.reset()
      builder = null
      if (doc == null) {
        throw new XPathException("No source document has been built")
      }
      doc.getTreeInfo.setSpaceStrippingRule(controller.getSpaceStrippingRule)
      if (destination == null) {
        throw new XPathException("No output destination has been supplied")
      }
      controller.setGlobalContextItem(doc)
      controller.applyTemplates(doc, destination)
    }
  }

}
