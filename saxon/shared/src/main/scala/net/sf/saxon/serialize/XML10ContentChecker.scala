package net.sf.saxon.serialize

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeInfo

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.serialize.charcode.UTF16CharacterSet

import net.sf.saxon.serialize.charcode.XMLCharacterData

import net.sf.saxon.trans.XPathException

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
