package org.orbeon.saxon.event

import org.orbeon.saxon.expr.instruct.WherePopulated

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.SimpleType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location


class WherePopulatedOutputter(next: Outputter) extends ProxyOutputter(next) {

  private var level: Int = 0

  private var pendingStartTag: Boolean = false

  private var pendingElemName: NodeName = _

  private var pendingSchemaType: SchemaType = _

  private var pendingLocationId: Location = _

  private var pendingProperties: Int = _

  private var pendingAttributes: AttributeMap = _

  private var pendingNamespaces: NamespaceMap = _

  override def startDocument(properties: Int): Unit = {
    level += 1
    if (level == 0) {
      pendingStartTag = true
      pendingElemName = null
      pendingProperties = properties
    } else {
      super.startDocument(properties)
    }
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            location: Location,
                            properties: Int): Unit = {
    releaseStartTag()
    level += 1
    if (level == 0) {
      pendingStartTag = true
      pendingElemName = elemName
      pendingSchemaType = `type`
      pendingLocationId = location.saveLocation
      pendingProperties = properties
      pendingAttributes = EmptyAttributeMap.getInstance
      pendingNamespaces = NamespaceMap.emptyMap
    } else {
      super.startElement(elemName, `type`, location, properties)
    }
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    releaseStartTag()
    level += 1
    if (level == 0) {
      pendingStartTag = true
      pendingElemName = elemName
      pendingSchemaType = `type`
      pendingLocationId = location.saveLocation
      pendingProperties = properties
      pendingAttributes = attributes
      pendingNamespaces = namespaces
    } else {
      super.startElement(elemName,
        `type`,
        attributes,
        namespaces,
        location,
        properties)
    }
  }

  override def namespace(prefix: String,
                         namespaceUri: String,
                         properties: Int): Unit = {
    if (level == 1) {
      pendingNamespaces = pendingNamespaces.put(prefix, namespaceUri)
    } else {
      super.namespace(prefix, namespaceUri, properties)
    }
  }

  override def attribute(attName: NodeName,
                         typeCode: SimpleType,
                         value: CharSequence,
                         location: Location,
                         properties: Int): Unit = {
    if (level == 1) {
      pendingAttributes = pendingAttributes.put(
        new AttributeInfo(attName,
          typeCode,
          value.toString,
          location,
          properties))
    } else if (value.length > 0) {
      super.attribute(attName, typeCode, value, location, properties)
    }
  }

  override def endDocument(): Unit = {
    level -= 1
    if (level == 0) {
      if (!pendingStartTag) {
        super.endDocument()
      }
    } else {
      super.endDocument()
    }
  }

  override def endElement(): Unit = {
    level -= 1
    if (level == 0) {
      if (!pendingStartTag) {
        super.endElement()
      }
    } else {
      super.endElement()
    }
    pendingStartTag = false
  }

  def releaseStartTag(): Unit = {
    if (level >= 1 && pendingStartTag) {
      if (pendingElemName == null) {
        getNextOutputter.startDocument(pendingProperties)
      } else {
        getNextOutputter.startElement(pendingElemName,
          pendingSchemaType,
          pendingAttributes,
          pendingNamespaces,
          pendingLocationId,
          pendingProperties)
      }
      pendingStartTag = false
    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (level == 0) {
      if (chars.length > 0) {
        super.characters(chars, locationId, properties)
      }
    } else if (level == 1) {
      if (chars.length > 0) {
        releaseStartTag()
        super.characters(chars, locationId, properties)
      }
    } else {
      super.characters(chars, locationId, properties)
    }
  }

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = {
    if (level == 0) {
      if (data.length > 0) {
        super.processingInstruction(name, data, location, properties)
      }
    } else if (level == 1) {
      if (data.length > 0) {
        releaseStartTag()
        super.processingInstruction(name, data, location, properties)
      }
    } else {
      super.processingInstruction(name, data, location, properties)
    }
  }

  override def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit = {
    if (level == 0) {
      if (content.length > 0) {
        super.comment(content, location, properties)
      }
    } else if (level == 1) {
      if (content.length > 0) {
        releaseStartTag()
        super.comment(content, location, properties)
      }
    } else {
      super.comment(content, location, properties)
    }
  }

  override def append(item: Item): Unit = {
    if (level == 0) {
      if (!WherePopulated.isDeemedEmpty(item)) {
        getNextOutputter.append(item)
      }
    } else if (level == 1 && pendingStartTag) {
      if (item.isInstanceOf[NodeInfo]) {
        val node: NodeInfo = item.asInstanceOf[NodeInfo]
        node.getNodeKind match {
          case Type.TEXT =>
            if (node.getNodeKind == Type.TEXT && node.getStringValueCS.length == 0) {
              return
            }
          case Type.DOCUMENT =>
            if (node.getNodeKind == Type.DOCUMENT && !node.hasChildNodes) {
              return
            }
          case Type.ATTRIBUTE =>
            attribute(NameOfNode.makeName(node),
              node.getSchemaType.asInstanceOf[SimpleType],
              node.getStringValue,
              Loc.NONE,
              0)
            return
          case Type.NAMESPACE =>
            namespace(node.getLocalPart, node.getStringValue, 0)
            return
          case _ =>

        }
      }
      releaseStartTag()
      getNextOutputter.append(item)
    } else {
      super.append(item)
    }
  }

  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    if (level == 0) {
      if (!WherePopulated.isDeemedEmpty(item)) {
        getNextOutputter.append(item, locationId, copyNamespaces)
      }
    } else if (level == 1 && pendingStartTag) {
      if (item.isInstanceOf[NodeInfo]) {
        val node: NodeInfo = item.asInstanceOf[NodeInfo]
        node.getNodeKind match {
          case Type.TEXT =>
            if (node.getNodeKind == Type.TEXT && node.getStringValueCS.length == 0) {
              return
            }
          case Type.DOCUMENT =>
            if (node.getNodeKind == Type.DOCUMENT && !node.hasChildNodes) {
              return
            }
          case Type.ATTRIBUTE =>
            attribute(NameOfNode.makeName(node),
              node.getSchemaType.asInstanceOf[SimpleType],
              node.getStringValue,
              locationId,
              0)
            return
          case Type.NAMESPACE =>
            namespace(node.getLocalPart, node.getStringValue, 0)
            return
          case _ =>

        }
      }
      releaseStartTag()
      getNextOutputter.append(item, locationId, copyNamespaces)
    } else {
      super.append(item, locationId, copyNamespaces)
    }
  }

}
