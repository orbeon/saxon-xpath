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

import org.orbeon.saxon.serialize.codenorm.Normalizer

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.Whitespace

import scala.beans.{BeanProperty, BooleanBeanProperty}

class UnicodeNormalizer(form: String, next: Receiver)
  extends ProxyReceiver(next) {

  var fb: Byte = 0

  @BeanProperty
  var normalizer: Normalizer = Normalizer.make(fb, getConfiguration)

  form match {
    case "NFC" => fb = Normalizer.C.toByte
    case "NFD" => fb = Normalizer.D.toByte
    case "NFKC" => fb = Normalizer.KC.toByte
    case "NFKD" => fb = Normalizer.KD.toByte
    case _ =>
      var err: XPathException = new XPathException(
        "Unknown normalization form " + form)
      err.setErrorCode("SESU0011")
      throw err

  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    val am2: AttributeMap = attributes.apply((attInfo) => {
      var newValue: String = normalize(
        attInfo.getValue,
        ReceiverOption.contains(attInfo.getProperties,
          ReceiverOption.USE_NULL_MARKERS)).toString
      new AttributeInfo(attInfo.getNodeName,
        attInfo.getType,
        newValue,
        attInfo.getLocation,
        attInfo.getProperties)
    })
    nextReceiver.startElement(elemName,
      `type`,
      am2,
      namespaces,
      location,
      properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (Whitespace.isWhite(chars)) {
      nextReceiver.characters(chars, locationId, properties)
    } else {
      nextReceiver.characters(
        normalize(chars,
          ReceiverOption.contains(properties,
            ReceiverOption.USE_NULL_MARKERS)),
        locationId,
        properties)
    }
  }

  def normalize(in: CharSequence, containsNullMarkers: Boolean): CharSequence =
    if (containsNullMarkers) {
      val out: FastStringBuffer = new FastStringBuffer(in.length)
      val s: String = in.toString
      var start: Int = 0
      var nextNull: Int = s.indexOf(0.toChar)
      while (nextNull >= 0) {
        out.cat(normalizer.normalize(s.substring(start, nextNull)))
        out.cat(0.toChar)
        start = nextNull + 1
        nextNull = s.indexOf(0.toChar, start)
        out.append(s.substring(start, nextNull))
        out.cat(0.toChar)
        start = nextNull + 1
        nextNull = s.indexOf(0.toChar, start)
      }
      out.cat(normalizer.normalize(s.substring(start)))
      out.condense()
    } else {
      normalizer.normalize(in)
    }

}
