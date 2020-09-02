package net.sf.saxon.event

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.expr.parser.Token

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern.CombinedNodeTest

import net.sf.saxon.pattern.ContentTypeTest

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.Orphan

import net.sf.saxon.value.Cardinality

import java.util.HashSet

import java.util.function.Supplier

class TypeCheckingFilter(next: Outputter) extends ProxyOutputter(next) {

  private var itemType: ItemType = _

  private var cardinality: Int = _

  private var role: RoleDiagnostic = _

  private var locator: Location = _

  private var count: Int = 0

  private var level: Int = 0

  private var checkedElements: HashSet[Long] = new HashSet(10)

  private var typeHierarchy: TypeHierarchy = getConfiguration.getTypeHierarchy

  def setRequiredType(`type`: ItemType,
                      cardinality: Int,
                      role: RoleDiagnostic,
                      locator: Location): Unit = {
    itemType = `type`
    this.cardinality = cardinality
    this.role = role
    this.locator = locator
  }

  override def namespace(prefix: String,
                         namespaceUri: String,
                         properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(Loc.NONE)
      }
      checkItemType(NodeKindTest.NAMESPACE, null, Loc.NONE)
    }
    getNextOutputter.namespace(prefix, namespaceUri, properties)
  }

  override def attribute(attName: NodeName,
                         typeCode: SimpleType,
                         value: CharSequence,
                         location: Location,
                         properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(location)
      }
      val `type`: ItemType = new CombinedNodeTest(
        new NameTest(Type.ATTRIBUTE, attName, getConfiguration.getNamePool),
        Token.INTERSECT,
        new ContentTypeTest(Type.ATTRIBUTE, typeCode, getConfiguration, false))
      checkItemType(`type`,
        nodeSupplier(Type.ATTRIBUTE, attName, typeCode, value),
        location)
    }
    getNextOutputter.attribute(attName, typeCode, value, location, properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(locationId)
      }
      checkItemType(NodeKindTest.TEXT,
        nodeSupplier(Type.TEXT, null, null, chars),
        locationId)
    }
    getNextOutputter.characters(chars, locationId, properties)
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(locationId)
      }
      checkItemType(NodeKindTest.COMMENT,
        nodeSupplier(Type.COMMENT, null, null, chars),
        locationId)
    }
    getNextOutputter.comment(chars, locationId, properties)
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(locationId)
      }
      checkItemType(NodeKindTest.PROCESSING_INSTRUCTION,
        nodeSupplier(Type.PROCESSING_INSTRUCTION,
          new NoNamespaceName(target),
          null,
          data),
        locationId)
    }
    getNextOutputter.processingInstruction(target,
      data,
      locationId,
      properties)
  }

  override def startDocument(properties: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(Loc.NONE)
      }
      checkItemType(NodeKindTest.DOCUMENT,
        nodeSupplier(Type.DOCUMENT, null, null, ""),
        Loc.NONE)
    }
    {
      level += 1;
    }
    getNextOutputter.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            elemType: SchemaType,
                            location: Location,
                            properties: Int): Unit = {
    checkElementStart(elemName, elemType, location)
    getNextOutputter.startElement(elemName, elemType, location, properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    checkElementStart(elemName, `type`, location)
    getNextOutputter.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  private def checkElementStart(elemName: NodeName,
                                elemType: SchemaType,
                                location: Location): Unit = {
    val config: Configuration = getConfiguration
    val namePool: NamePool = config.getNamePool
    if (level == 0) {
      count+=1
      if (count == 1) {
        val `type`: ItemType = new CombinedNodeTest(
          new NameTest(Type.ELEMENT, elemName, namePool),
          Token.INTERSECT,
          new ContentTypeTest(Type.ELEMENT, elemType, config, false))
        checkItemType(`type`,
          nodeSupplier(Type.ELEMENT, elemName, elemType, ""),
          location)
      } else {
        if (count == 2) {
          checkAllowsMany(location)
        }
        val key: Long = elemName
          .obtainFingerprint(namePool)
          .toLong << 32 | elemType.getFingerprint.toLong
        if (!checkedElements.contains(key)) {
          val `type`: ItemType = new CombinedNodeTest(
            new NameTest(Type.ELEMENT, elemName, namePool),
            Token.INTERSECT,
            new ContentTypeTest(Type.ELEMENT, elemType, config, false))
          checkItemType(`type`,
            nodeSupplier(Type.ELEMENT, elemName, elemType, ""),
            location)
          checkedElements.add(key)
        }
      }
    }
    level += 1
  }

  override def endDocument(): Unit = {
    level -= 1
    getNextOutputter.endDocument()
  }

  override def endElement(): Unit = {
    level -= 1
    getNextOutputter.endElement()
  }

  override def close(): Unit = {
    finalCheck()
    super.close()
  }

  def finalCheck(): Unit = {
    if (count == 0 && !Cardinality.allowsZero(cardinality)) {
      val err = new XPathException(
        "An empty sequence is not allowed as the " + role.getMessage)
      val errorCode: String = role.getErrorCode
      err.setErrorCode(errorCode)
      if ("XPDY0050" != errorCode) {
        err.setIsTypeError(true)
      }
      throw err
    }
  }

  private def nodeSupplier(nodeKind: Short,
                           name: NodeName,
                           `type`: SchemaType,
                           value: CharSequence): Supplier[NodeInfo] = () => {
    val o: Orphan = new Orphan(getPipelineConfiguration.getConfiguration)
    o.setNodeKind(nodeKind)
    if (name != null) {
      o.setNodeName(name)
    }
    o.setTypeAnnotation(`type`)
    o.setStringValue(value)
    o
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(locationId)
      }
      checkItem(item, locationId)
    }
    getNextOutputter.append(item, locationId, copyNamespaces)
  }

  override def append(item: Item): Unit = {
    if (level == 0) {
      count+=1
      if (count == 2) {
        checkAllowsMany(Loc.NONE)
      }
      checkItem(item, Loc.NONE)
    }
    getNextOutputter.append(item)
  }

  override def usesTypeAnnotations: Boolean = true

  private def checkItemType(`type`: ItemType,
                            itemSupplier: Supplier[_ <: Item],
                            locationId: Location): Unit = {
    if (!typeHierarchy.isSubType(`type`, itemType)) {
      throwTypeError(`type`,
        if (itemSupplier == null) null else itemSupplier.get,
        locationId)
    }
  }

  private def checkItem(item: Item, locationId: Location): Unit = {
    if (!itemType.matches(item, typeHierarchy)) {
      throwTypeError(null, item, locationId)
    }
  }

  private def throwTypeError(suppliedType: ItemType,
                             item: Item,
                             locationId: Location): Unit = {
    var message: String = null
    message =
      if (item == null) role.composeErrorMessage(itemType, suppliedType)
      else role.composeErrorMessage(itemType, item, typeHierarchy)
    val errorCode: String = role.getErrorCode
    val err = new XPathException(message)
    err.setErrorCode(errorCode)
    if ("XPDY0050" != errorCode) {
      err.setIsTypeError(true)
    }
    if (locationId == null) {
      err.setLocation(locator)
    } else {
      err.setLocation(locationId.saveLocation())
    }
    throw err
  }

  private def checkAllowsMany(locationId: Location): Unit = {
    if (!Cardinality.allowsMany(cardinality)) {
      val err = new XPathException(
        "A sequence of more than one item is not allowed as the " +
          role.getMessage)
      val errorCode: String = role.getErrorCode
      err.setErrorCode(errorCode)
      if ("XPDY0050" != errorCode) {
        err.setIsTypeError(true)
      }
      if (locationId == null || locationId == Loc.NONE) {
        err.setLocator(locator)
      } else {
        err.setLocator(locationId)
      }
      throw err
    }
  }

}
