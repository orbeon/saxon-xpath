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

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

class CharacterMapExpander(next: Receiver) extends ProxyReceiver(next) {

  private var charMap: CharacterMap = _

  private var useNullMarkers: Boolean = true

  def setCharacterMap(map: CharacterMap): Unit = {
    charMap = map
  }

  def getCharacterMap(): CharacterMap = charMap

  def setUseNullMarkers(use: Boolean): Unit = {
    useNullMarkers = use
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    val atts2: List[AttributeInfo] =
      new ArrayList[AttributeInfo](attributes.size)
    for (att <- attributes) {
      val oldValue: String = att.getValue
      if (!ReceiverOption.contains(att.getProperties,
        ReceiverOption.DISABLE_CHARACTER_MAPS)) {
        val mapped: CharSequence = charMap.map(oldValue, useNullMarkers)
        if (mapped != oldValue) {
          val p2: Int = (att.getProperties | ReceiverOption.USE_NULL_MARKERS) &
            ~ReceiverOption.NO_SPECIAL_CHARS
          atts2.add(
            new AttributeInfo(att.getNodeName,
              att.getType,
              mapped.toString,
              att.getLocation,
              p2))
        } else {
          atts2.add(att)
        }
      } else {
        atts2.add(att)
      }
    }
    nextReceiver.startElement(elemName,
      `type`,
      AttributeMap.fromList(atts2),
      namespaces,
      location,
      properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (!ReceiverOption.contains(properties,
      ReceiverOption.DISABLE_CHARACTER_MAPS)) {
      val mapped: CharSequence = charMap.map(chars, useNullMarkers)
      var props = properties
      if (mapped != chars) {
        props = (props | ReceiverOption.USE_NULL_MARKERS) & ~ReceiverOption.NO_SPECIAL_CHARS
      }
      nextReceiver.characters(mapped, locationId, props)
    } else {
      nextReceiver.characters(chars, locationId, properties)
    }
  }

}
