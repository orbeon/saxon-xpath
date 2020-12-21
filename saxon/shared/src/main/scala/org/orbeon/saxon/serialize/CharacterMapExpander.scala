package org.orbeon.saxon.serialize

import java.{util => ju}

import org.orbeon.saxon.event.{ProxyReceiver, Receiver, ReceiverOption}
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.{AttributeInfo, AttributeMap, NamespaceMap, NodeName}
import org.orbeon.saxon.s9api.Location

import scala.jdk.CollectionConverters._

class CharacterMapExpander(next: Receiver) extends ProxyReceiver(next) {

  private var charMap: CharacterMap = _

  private var useNullMarkers: Boolean = true

  def setCharacterMap(map: CharacterMap): Unit = {
    charMap = map
  }

  def getCharacterMap: CharacterMap = charMap

  def setUseNullMarkers(use: Boolean): Unit = {
    useNullMarkers = use
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    val atts2: ju.List[AttributeInfo] =
      new ju.ArrayList[AttributeInfo](attributes.size)
    for (att <- attributes.iterator.asScala) {
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
