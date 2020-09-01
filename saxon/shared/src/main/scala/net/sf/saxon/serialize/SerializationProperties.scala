package net.sf.saxon.serialize

import net.sf.saxon.event._

import net.sf.saxon.lib.SaxonOutputKeys

import javax.xml.transform.OutputKeys

import java.util.Properties

import scala.jdk.CollectionConverters._

class SerializationProperties {

  var properties: Properties = new Properties()

  var charMapIndex: CharacterMapIndex = _

  var validationFactory: FilterFactory = _

  def this(props: Properties) = {
    this()
    this.properties = props
  }

  def this(props: Properties, charMapIndex: CharacterMapIndex) = {
    this()
    this.properties = props
    this.charMapIndex = charMapIndex
  }

  def setProperty(name: String, value: String): Unit = {
    properties.setProperty(name, value)
  }

  def getProperty(name: String): String = getProperties.getProperty(name)

  def getProperties: Properties = properties

  def getCharacterMapIndex: CharacterMapIndex = charMapIndex

  def setValidationFactory(validationFactory: FilterFactory): Unit = {
    this.validationFactory = validationFactory
  }

  def getValidationFactory: FilterFactory = validationFactory

  def makeSequenceNormalizer(next: Receiver): SequenceNormalizer = {
    var nextRec = next
    if (getValidationFactory != null) {
      nextRec = getValidationFactory.makeFilter(nextRec)
    }
    val itemSeparator: String =
      properties.getProperty(SaxonOutputKeys.ITEM_SEPARATOR)
    if (itemSeparator == null || "#absent" == itemSeparator)
      new SequenceNormalizerWithSpaceSeparator(nextRec)
    else new SequenceNormalizerWithItemSeparator(nextRec, itemSeparator)
  }

  def combineWith(defaults: SerializationProperties): SerializationProperties = {
    var charMap: CharacterMapIndex = this.charMapIndex
    if (charMap == null || charMap.isEmpty) {
      charMap = defaults.getCharacterMapIndex
    }
    var validationFactory: FilterFactory = this.validationFactory
    if (validationFactory == null) {
      validationFactory = defaults.validationFactory
    }
    val props: Properties = new Properties(defaults.getProperties)
    for (prop <- this.getProperties.stringPropertyNames().asScala) {
      val value: String = this.getProperties.getProperty(prop)
      if (prop == OutputKeys.CDATA_SECTION_ELEMENTS || prop == SaxonOutputKeys.SUPPRESS_INDENTATION) {
        val existing: String = defaults.getProperty(prop)
        if (existing == null || existing == value) {
          props.setProperty(prop, value)
        } else {
          props.setProperty(prop, existing + " " + value)
        }
      } else {
        props.setProperty(prop, value)
      }
    }
    val newParams: SerializationProperties =
      new SerializationProperties(props, charMap)
    newParams.setValidationFactory(validationFactory)
    newParams
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder()
    for (k <- properties.stringPropertyNames().asScala) {
      sb.append(k).append("=").append(properties.getProperty(k)).append(" ")
    }
    if (charMapIndex != null) {
      for (cm <- charMapIndex.asScala) {
        sb.append(cm.getName.getEQName)
          .append("={")
          .append(cm.toString)
          .append("} ")
      }
    }
    sb.toString
  }

}
