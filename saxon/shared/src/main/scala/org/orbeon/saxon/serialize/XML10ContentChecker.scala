package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeInfo

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet

import org.orbeon.saxon.serialize.charcode.XMLCharacterData

import org.orbeon.saxon.trans.XPathException

class XML10ContentChecker(next: Receiver) extends ProxyReceiver(next) {

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    for (att <- attributes) {
      checkString(att.getValue, att.getLocation)
    }
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    checkString(chars, locationId)
    nextReceiver.characters(chars, locationId, properties)
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    checkString(chars, locationId)
    nextReceiver.comment(chars, locationId, properties)
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    checkString(data, locationId)
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  private def checkString(in: CharSequence, locationId: Location): Unit = {
    val len = in.length
    for (c <- 0 until len) {
      var ch = c
      var ch32: Int = in.charAt(ch)
      if (UTF16CharacterSet.isHighSurrogate(ch32)) {
        ch += 1
        val low: Char = in.charAt(ch)
        ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
      }
      if (!XMLCharacterData.isValid10(ch32)) {
        val err = new XPathException(
          "The result tree contains a character not allowed by XML 1.0 (hex " +
            java.lang.Integer.toHexString(ch32) +
            ')')
        err.setErrorCode("SERE0006")
        err.setLocator(locationId)
        throw err
      }
    }
  }

}
