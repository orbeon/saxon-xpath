package org.orbeon.saxon.serialize

import org.orbeon.saxon.expr.instruct.ResultDocument
import org.orbeon.saxon.lib.{NamespaceConstant, SaxonOutputKeys}
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.NodeKindTest
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.serialize.SerializationParamsHandler._
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.z.IntHashMap

import java.util.Properties
import java.{util => ju}
import scala.jdk.CollectionConverters._


object SerializationParamsHandler {

  val NAMESPACE: String = NamespaceConstant.OUTPUT

  private def restrictAttributes(element: NodeInfo, allowedNames: String*): Unit = {
    for (att <- element.attributes.iterator.asScala) {
      val name = att.getNodeName
      if ("" == name.getURI && ju.Arrays.binarySearch(allowedNames.asInstanceOf[Array[AnyRef]], name.getLocalPart) < 0)
        throw new XPathException(
          "In serialization parameters, attribute @" + name.getLocalPart +
            " must not appear on element " +
            element.getDisplayName,
          "SEPM0017")
    }
  }

  private def getAttribute(element: NodeInfo, localName: String): String = {
    val value = element.getAttributeValue("", localName)
    if (value == null)
      throw new XPathException(
        "In serialization parameters, attribute @" + localName +
          " is missing on element " +
          element.getDisplayName)
    value
  }
}

class SerializationParamsHandler {

  var properties: Properties = _
  var characterMap: CharacterMap = _
  var locator: Location = _

  def this(props: Properties) = {
    this()
    this.properties = props
  }

  def setLocator(locator: Location): Unit =
    this.locator = locator

  def setSerializationParams(node: NodeInfo): Unit = {
    var _node = node
    if (properties == null)
      properties = new Properties
    if (_node.getNodeKind == Type.DOCUMENT)
      _node = Navigator.getOutermostElement(_node.getTreeInfo)
    if (_node.getNodeKind != Type.ELEMENT)
      throw new XPathException("Serialization params: node must be a document or element node")
    if (_node.getLocalPart.!=("serialization-parameters"))
      throw new XPathException("Serialization params: element name must be 'serialization-parameters")
    if (_node.getURI != NAMESPACE)
      throw new XPathException("Serialization params: element namespace must be " + NAMESPACE)
    restrictAttributes(_node)
    val nodeNames = new ju.HashSet[NodeName]
    val kids      = _node.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    var child: NodeInfo = null
    while ({
      child = kids.next()
      child
    } != null) {
      if (!nodeNames.add(NameOfNode.makeName(child)))
        throw new XPathException(
          "Duplicated serialization parameter " + child.getDisplayName,
          "SEPM0019")
      val lname = child.getLocalPart
      var uri = child.getURI
      if (uri.isEmpty)
        throw new XPathException(
          "Serialization parameter " + lname + " is in no namespace",
          "SEPM0017")
      if (NamespaceConstant.OUTPUT == uri)
        uri = ""
      if ("" == uri && lname == "use-character-maps") {
        restrictAttributes(child)
        val gKids = child.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
        var gChild: NodeInfo = null
        val map = new IntHashMap[String]()
        while ({
          gChild = gKids.next()
          gChild
        } != null) {
          restrictAttributes(gChild, "character", "map-string")
          if (!(gChild.getURI == NAMESPACE && gChild.getLocalPart == "character-map"))
            if (gChild.getURI == NAMESPACE || gChild.getURI.isEmpty)
              throw new XPathException(
                "Invalid child of use-character-maps: " + gChild.getDisplayName,
                "SEPM0017"
              )
          val ch      = getAttribute(gChild, "character")
          val str     = getAttribute(gChild, "map-string")
          val chValue = UnicodeString.makeUnicodeString(ch)
          if (chValue.uLength != 1)
            throw new XPathException(
              "In the serialization parameters, the value of @character in the character map " +
                "must be a single Unicode character",
              "SEPM0017")
          val code = chValue.uCharAt(0)
          val prev = map.put(code, str)
          if (prev != null)
            throw new XPathException(
              "In the serialization parameters, the character map contains two entries for the character \\u" +
                java.lang.Integer.toHexString(65536 + code).substring(1),
              "SEPM0018"
            )
        }
        characterMap = new CharacterMap(NameOfNode.makeName(_node).getStructuredQName, map)
      } else {
        restrictAttributes(child, "value")
        val value: String = getAttribute(child, "value")
        try ResultDocument.setSerializationProperty(properties,
          uri,
          lname,
          value,
          child.getAllNamespaces,
          prevalidated = false,
          _node.getConfiguration)
        catch {
          case err: XPathException =>
            if ("XQST0109" == err.getErrorCodeLocalPart || "SEPM0016" == err.getErrorCodeLocalPart) {
              if ("" == uri) {
                val e2: XPathException = new XPathException(
                  "Unknown serialization parameter " + Err.depict(child),
                  "SEPM0017")
                e2.setLocator(locator)
                throw e2
              }
            } else {
              throw err
            }
        }
      }
    }
  }

  def getSerializationProperties: SerializationProperties = {
    val index = new CharacterMapIndex
    if (characterMap != null) {
      index.putCharacterMap(new StructuredQName("", "", "charMap"), characterMap)
      properties.put(SaxonOutputKeys.USE_CHARACTER_MAPS, "charMap")
    }
    new SerializationProperties(properties, index)
  }

  def getCharacterMap: CharacterMap = characterMap
}
