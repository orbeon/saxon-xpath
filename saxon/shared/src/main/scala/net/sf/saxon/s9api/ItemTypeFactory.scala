package net.sf.saxon.s9api

import net.sf.saxon.expr.parser.Token
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.model
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.StandardNames
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.pattern._
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.ExternalObject
import net.sf.saxon.value.ObjectValue
import java.util.Map

import XdmNodeKind._
import net.sf.saxon.model.{AnyExternalObjectType, AtomicType, JavaExternalObjectType, SchemaDeclaration, SchemaType, Type}
import net.sf.saxon.utils.Configuration


class ItemTypeFactory(private var processor: Processor) {

  def getAtomicType(name: QName): ItemType =
    getAtomicType(name.getStructuredQName)

  private def getAtomicType(name: StructuredQName): ItemType = {
    val uri: String = name.getURI
    val local: String = name.getLocalPart
    if (NamespaceConstant.SCHEMA == uri) {
      val fp: Int = StandardNames.getFingerprint(uri, local)
      val config: Configuration = processor.getUnderlyingConfiguration
      if (config.getXsdVersion == Configuration.XSD10 && config.getXMLVersion == Configuration.XML10) {
        getBuiltInAtomicType(fp)
      } else {
        ItemType.BuiltInAtomicItemType.makeVariant(
          getBuiltInAtomicType(fp)
            .asInstanceOf[ItemType.BuiltInAtomicItemType],
          config.getConversionRules)
      }
    } else {
      val config: Configuration = processor.getUnderlyingConfiguration
      val `type`: SchemaType =
        config.getSchemaType(new StructuredQName("", uri, local))
      if (`type` == null || !`type`.isAtomicType) {
        throw new SaxonApiException("Unknown atomic type " + name.getClarkName)
      }
      new ConstructedItemType(`type`.asInstanceOf[AtomicType], processor)
    }
  }

  private def getBuiltInAtomicType(fp: Int): ItemType = fp match {
    case StandardNames.XS_ANY_ATOMIC_TYPE => ItemType.ANY_ATOMIC_VALUE
    case StandardNames.XS_STRING => ItemType.STRING
    case StandardNames.XS_BOOLEAN => ItemType.BOOLEAN
    case StandardNames.XS_DURATION => ItemType.DURATION
    case StandardNames.XS_DATE_TIME => ItemType.DATE_TIME
    case StandardNames.XS_DATE => ItemType.DATE
    case StandardNames.XS_TIME => ItemType.TIME
    case StandardNames.XS_G_YEAR_MONTH => ItemType.G_YEAR_MONTH
    case StandardNames.XS_G_MONTH => ItemType.G_MONTH
    case StandardNames.XS_G_MONTH_DAY => ItemType.G_MONTH_DAY
    case StandardNames.XS_G_YEAR => ItemType.G_YEAR
    case StandardNames.XS_G_DAY => ItemType.G_DAY
    case StandardNames.XS_HEX_BINARY => ItemType.HEX_BINARY
    case StandardNames.XS_BASE64_BINARY => ItemType.BASE64_BINARY
    case StandardNames.XS_ANY_URI => ItemType.ANY_URI
    case StandardNames.XS_QNAME => ItemType.QNAME
    case StandardNames.XS_NOTATION => ItemType.NOTATION
    case StandardNames.XS_UNTYPED_ATOMIC => ItemType.UNTYPED_ATOMIC
    case StandardNames.XS_DECIMAL => ItemType.DECIMAL
    case StandardNames.XS_FLOAT => ItemType.FLOAT
    case StandardNames.XS_DOUBLE => ItemType.DOUBLE
    case StandardNames.XS_INTEGER => ItemType.INTEGER
    case StandardNames.XS_NON_POSITIVE_INTEGER => ItemType.NON_POSITIVE_INTEGER
    case StandardNames.XS_NEGATIVE_INTEGER => ItemType.NEGATIVE_INTEGER
    case StandardNames.XS_LONG => ItemType.LONG
    case StandardNames.XS_INT => ItemType.INT
    case StandardNames.XS_SHORT => ItemType.SHORT
    case StandardNames.XS_BYTE => ItemType.BYTE
    case StandardNames.XS_NON_NEGATIVE_INTEGER => ItemType.NON_NEGATIVE_INTEGER
    case StandardNames.XS_POSITIVE_INTEGER => ItemType.POSITIVE_INTEGER
    case StandardNames.XS_UNSIGNED_LONG => ItemType.UNSIGNED_LONG
    case StandardNames.XS_UNSIGNED_INT => ItemType.UNSIGNED_INT
    case StandardNames.XS_UNSIGNED_SHORT => ItemType.UNSIGNED_SHORT
    case StandardNames.XS_UNSIGNED_BYTE => ItemType.UNSIGNED_BYTE
    case StandardNames.XS_YEAR_MONTH_DURATION => ItemType.YEAR_MONTH_DURATION
    case StandardNames.XS_DAY_TIME_DURATION => ItemType.DAY_TIME_DURATION
    case StandardNames.XS_NORMALIZED_STRING => ItemType.NORMALIZED_STRING
    case StandardNames.XS_TOKEN => ItemType.TOKEN
    case StandardNames.XS_LANGUAGE => ItemType.LANGUAGE
    case StandardNames.XS_NAME => ItemType.NAME
    case StandardNames.XS_NMTOKEN => ItemType.NMTOKEN
    case StandardNames.XS_NCNAME => ItemType.NCNAME
    case StandardNames.XS_ID => ItemType.ID
    case StandardNames.XS_IDREF => ItemType.IDREF
    case StandardNames.XS_ENTITY => ItemType.ENTITY
    case StandardNames.XS_DATE_TIME_STAMP => ItemType.DATE_TIME_STAMP
    case _ =>
      throw new SaxonApiException(
        "Unknown atomic type " +
          processor.getUnderlyingConfiguration.getNamePool.getClarkName(fp))

  }

  def getNodeKindTest(kind: XdmNodeKind): ItemType = kind match {
    case DOCUMENT => ItemType.DOCUMENT_NODE
    case ELEMENT => ItemType.ELEMENT_NODE
    case ATTRIBUTE => ItemType.ATTRIBUTE_NODE
    case TEXT => ItemType.TEXT_NODE
    case COMMENT => ItemType.COMMENT_NODE
    case PROCESSING_INSTRUCTION => ItemType.PROCESSING_INSTRUCTION_NODE
    case NAMESPACE => ItemType.NAMESPACE_NODE
    case _ => throw new IllegalArgumentException("XdmNodeKind")

  }

  def getItemType(kind: XdmNodeKind, name: QName): ItemType = {
    val k: Int = kind.getNumber
    if (k == Type.ELEMENT || k == Type.ATTRIBUTE || k == Type.PROCESSING_INSTRUCTION) {
      if (k == Type.PROCESSING_INSTRUCTION && name.getNamespaceURI.isEmpty) {
        throw new IllegalArgumentException(
          "The name of a processing instruction must not be in a namespace")
      }
      val `type`: NameTest = new NameTest(
        k,
        name.getNamespaceURI,
        name.getLocalName,
        processor.getUnderlyingConfiguration.getNamePool)
      new ConstructedItemType(`type`, processor)
    } else {
      throw new IllegalArgumentException(
        "Node kind must be element, attribute, or processing-instruction")
    }
  }

  def getSchemaElementTest(name: QName): ItemType = {
    val config: Configuration = processor.getUnderlyingConfiguration
    val decl: SchemaDeclaration =
      config.getElementDeclaration(name.getStructuredQName)
    if (decl == null) {
      throw new SaxonApiException(
        "No global declaration found for element " + name.getClarkName)
    }
    val test: NodeTest = decl.makeSchemaNodeTest()
    new ConstructedItemType(test, processor)
  }

  def getElementTest(name: QName,
                     schemaType: QName,
                     nillable: Boolean): ItemType = {
    val config: Configuration = processor.getUnderlyingConfiguration
    var nameTest: NameTest = null
    var contentTest: ContentTypeTest = null
    if (name != null) {
      val elementFP: Int = config.getNamePool
        .allocateFingerprint(name.getNamespaceURI, name.getLocalName)
      nameTest = new NameTest(Type.ELEMENT, elementFP, config.getNamePool)
    }
    if (schemaType != null) {
      val `type`: SchemaType = config.getSchemaType(
        new StructuredQName("",
          schemaType.getNamespaceURI,
          schemaType.getLocalName))
      if (`type` == null) {
        throw new SaxonApiException(
          "Unknown schema type " + schemaType.getClarkName)
      }
      contentTest = new ContentTypeTest(Type.ELEMENT, `type`, config, nillable)
    }
    if (contentTest == null) {
      if (nameTest == null) {
        getNodeKindTest(XdmNodeKind.ELEMENT)
      } else {
        new ConstructedItemType(nameTest, processor)
      }
    } else {
      if (nameTest == null) {
        new ConstructedItemType(contentTest, processor)
      } else {
        val combo: CombinedNodeTest =
          new CombinedNodeTest(nameTest, Token.INTERSECT, contentTest)
        new ConstructedItemType(combo, processor)
      }
    }
  }

  def getSchemaAttributeTest(name: QName): ItemType = {
    val config: Configuration = processor.getUnderlyingConfiguration
    val nn: StructuredQName =
      new StructuredQName("", name.getNamespaceURI, name.getLocalName)
    val decl: SchemaDeclaration = config.getAttributeDeclaration(nn)
    if (decl == null) {
      throw new SaxonApiException(
        "No global declaration found for attribute " + name.getClarkName)
    }
    val test: NodeTest = decl.makeSchemaNodeTest()
    new ConstructedItemType(test, processor)
  }

  def getAttributeTest(name: QName, schemaType: QName): ItemType = {
    var nameTest: NameTest = null
    var contentTest: ContentTypeTest = null
    val config: Configuration = processor.getUnderlyingConfiguration
    if (name != null) {
      val attributeFP: Int = config.getNamePool
        .allocateFingerprint(name.getNamespaceURI, name.getLocalName)
      nameTest = new NameTest(Type.ATTRIBUTE, attributeFP, config.getNamePool)
    }
    if (schemaType != null) {
      val `type`: SchemaType = config.getSchemaType(
        new StructuredQName("",
          schemaType.getNamespaceURI,
          schemaType.getLocalName))
      if (`type` == null) {
        throw new SaxonApiException(
          "Unknown schema type " + schemaType.getClarkName)
      }
      contentTest = new ContentTypeTest(Type.ATTRIBUTE, `type`, config, false)
    }
    if (contentTest == null) {
      if (nameTest == null) {
        getNodeKindTest(XdmNodeKind.ATTRIBUTE)
      } else {
        new ConstructedItemType(nameTest, processor)
      }
    } else {
      if (nameTest == null) {
        new ConstructedItemType(contentTest, processor)
      } else {
        val combo: CombinedNodeTest =
          new CombinedNodeTest(nameTest, Token.INTERSECT, contentTest)
        new ConstructedItemType(combo, processor)
      }
    }
  }

  def getDocumentTest(elementTest: ItemType): ItemType = {
    val test: net.sf.saxon.model.ItemType = elementTest.getUnderlyingItemType
    if (test.getPrimitiveType != Type.ELEMENT) {
      throw new IllegalArgumentException(
        "Supplied itemType is not an element test")
    }
    val docTest: DocumentNodeTest = new DocumentNodeTest(
      test.asInstanceOf[NodeTest])
    new ConstructedItemType(docTest, processor)
  }

  def getExternalObjectType(externalClass: Class[_]): ItemType = {
    val `type`: JavaExternalObjectType = processor.getUnderlyingConfiguration
      .getJavaExternalObjectType(externalClass)
    new ConstructedItemType(`type`, processor)
  }

  def getExternalObject(`object`: AnyRef): XdmItem =
    XdmValue.wrap(new ObjectValue(`object`)).asInstanceOf[XdmItem]

  def getMapType(keyType: ItemType, valueType: SequenceType): ItemType = {
    if (!(keyType.getUnderlyingItemType.isInstanceOf[AtomicType])) {
      throw new IllegalArgumentException("Map key must be atomic")
    }
    new ConstructedItemType(
      new MapType(keyType.getUnderlyingItemType.asInstanceOf[AtomicType],
        valueType.getUnderlyingSequenceType),
      processor)
  }

  def getArrayType(memberType: SequenceType): ItemType =
    new ConstructedItemType(
      new ArrayItemType(memberType.getUnderlyingSequenceType),
      processor)

  def newMap(map: Map[_, _]): XdmMap = XdmMap.makeMap(map)

  def getItemType(item: XdmItem): ItemType =
    if (item.isAtomicValue) {
      val value: AtomicValue =
        item.getUnderlyingValue.asInstanceOf[AtomicValue]
      val `type`: AtomicType = value.getItemType
      new ConstructedItemType(`type`, processor)
    } else if (item.isNode) {
      val node: NodeInfo = item.getUnderlyingValue.asInstanceOf[NodeInfo]
      val kind: Int = node.getNodeKind
      if (node.getLocalPart.isEmpty) {
        new ConstructedItemType(NodeKindTest.makeNodeKindTest(kind), processor)
      } else {
        new ConstructedItemType(new SameNameTest(node), processor)
      }
    } else {
      val it: Item = item.getUnderlyingValue
      if (it.isInstanceOf[MapItem]) {
        ItemType.ANY_MAP
      } else if (it.isInstanceOf[ArrayItem]) {
        ItemType.ANY_ARRAY
      } else if (it.isInstanceOf[ExternalObject[_]]) {
        new ConstructedItemType(AnyExternalObjectType.THE_INSTANCE, processor)
      } else {
        ItemType.ANY_FUNCTION
      }
    }

  def exposeItemType(it: net.sf.saxon.model.ItemType): ItemType =
    new ConstructedItemType(it, processor)

}
