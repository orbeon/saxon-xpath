package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.EventSource

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.lib.InvalidityHandler

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.serialize.SerializationProperties

import javax.xml.transform.ErrorListener

import javax.xml.transform.Source


abstract class SchemaValidator extends AbstractDestination {

  def setLax(lax: Boolean): Unit

  def isLax: Boolean

  def setErrorListener(listener: ErrorListener): Unit

  def getErrorListener: ErrorListener

  def setInvalidityHandler(handler: InvalidityHandler): Unit

  def getInvalidityHandler: InvalidityHandler

  def setCollectStatistics(collect: Boolean): Unit

  def isCollectStatistics: Boolean

  def reportValidationStatistics(destination: Destination): Unit

  def setValidityReporting(destination: Destination): Unit

  def setUseXsiSchemaLocation(recognize: Boolean): Unit

  def isUseXsiSchemaLocation: Boolean

  def setDestination(destination: Destination): Unit

  def getDestination: Destination

  def setDocumentElementName(name: QName): Unit

  def getDocumentElementName: QName

  def setDocumentElementTypeName(name: QName): Unit

  def getDocumentElementTypeName: QName

  def getDocumentElementType: SchemaType

  def setExpandAttributeDefaults(expand: Boolean): Unit

  def isExpandAttributeDefaults: Boolean

  def setParameter(name: QName, value: XdmValue): Unit

  def getParameter(name: QName): XdmValue

  def validate(source: Source): Unit

  def validateMultiple(sources: java.lang.Iterable[Source]): Unit

  def asSource(input: Source): Source = new EventSource() {
    this.systemId = input.getSystemId

    override def send(out: Receiver): Unit = {
    //  setDestination(new ReceivingDestination(out)) // ReceivingDestination not exist
      setDestination(null)
      validate(input)
    }
  }

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver

  def close(): Unit

}