package net.sf.saxon.s9api

import net.sf.saxon.lib.{ConversionRules, NamespaceConstant, StandardURIChecker}
import net.sf.saxon.ma.arrays.{ArrayItem, ArrayItemType}
import net.sf.saxon.ma.map.{MapItem, MapType}
import net.sf.saxon.model._
import net.sf.saxon.om.{Function, Item, NodeInfo, StructuredQName}
import net.sf.saxon.pattern.{AnyNodeTest, NodeKindTest, NodeTest}
import net.sf.saxon.s9api.ItemType._
import net.sf.saxon.value.{AtomicValue, NumericValue}

import scala.beans.BeanProperty

object ItemType {

  private val defaultConversionRules: ConversionRules = new ConversionRules()

  defaultConversionRules.setStringToDoubleConverter(StringToDouble.getInstance)
  defaultConversionRules.setNotationSet(null)
  defaultConversionRules.setURIChecker(StandardURIChecker.getInstance)

  val ANY_ITEM: ItemType = new ItemType {
    override def getConversionRules(): ConversionRules = defaultConversionRules
    def matches(item: XdmItem): Boolean = true
    def subsumes(other: ItemType): Boolean = true
    def getUnderlyingItemType(): net.sf.saxon.model.ItemType = AnyItemType
  }

  val ANY_FUNCTION: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = item.getUnderlyingValue.isInstanceOf[Function]
    def subsumes(other: ItemType): Boolean = other.getUnderlyingItemType.isInstanceOf[FunctionItemType]
    def getUnderlyingItemType(): net.sf.saxon.model.ItemType = AnyFunctionType
  }

  val ANY_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean =
      item.getUnderlyingValue.isInstanceOf[NodeInfo]

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.isInstanceOf[NodeTest]

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      AnyNodeTest.getInstance
  }

  val ATTRIBUTE_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.ATTRIBUTE
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.ATTRIBUTE

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.ATTRIBUTE
  }

  val COMMENT_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.COMMENT
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.COMMENT

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.COMMENT
  }

  val TEXT_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] && it
        .asInstanceOf[NodeInfo]
        .getNodeKind == Type.TEXT
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.TEXT

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.TEXT
  }

  val ELEMENT_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.ELEMENT
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.ELEMENT

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.ELEMENT
  }

  val DOCUMENT_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.DOCUMENT
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.DOCUMENT

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.DOCUMENT
  }

  val NAMESPACE_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.NAMESPACE
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.NAMESPACE

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.NAMESPACE
  }

  val PROCESSING_INSTRUCTION_NODE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = {
      val it: Item = item.getUnderlyingValue
      it.isInstanceOf[NodeInfo] &&
        it.asInstanceOf[NodeInfo].getNodeKind == Type.PROCESSING_INSTRUCTION
    }

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.getUType == UType.PI

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NodeKindTest.PROCESSING_INSTRUCTION
  }

  val ANY_MAP: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean =
      item.getUnderlyingValue.isInstanceOf[MapItem]

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.isInstanceOf[MapType]

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      MapType.ANY_MAP_TYPE
  }

  val ANY_ARRAY: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean =
      item.getUnderlyingValue.isInstanceOf[ArrayItem]

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.isInstanceOf[ArrayItemType]

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      ArrayItemType.ANY_ARRAY_TYPE
  }

  val ANY_ATOMIC_VALUE: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean =
      item.getUnderlyingValue.isInstanceOf[AtomicValue]

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType.isInstanceOf[AtomicType]

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      BuiltInAtomicType.ANY_ATOMIC
  }

  val ERROR: ItemType = new ItemType {
    def matches(item: XdmItem): Boolean = false

    def subsumes(other: ItemType): Boolean =
      other.getUnderlyingItemType eq ErrorType

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      ErrorType
  }

  object BuiltInAtomicItemType {

    def makeVariant(`type`: BuiltInAtomicItemType,
                    conversionRules: ConversionRules): BuiltInAtomicItemType =
      new BuiltInAtomicItemType(`type`.underlyingType, conversionRules)

  }

  class BuiltInAtomicItemType(
                               private var underlyingType: BuiltInAtomicType,
                               @BeanProperty var conversonRules: ConversionRules)
    extends ItemType {

    def matches(item: XdmItem): Boolean = {
      val value: Item = item.getUnderlyingValue
      if (!(value.isInstanceOf[AtomicValue])) {
        return false
      }
      val `type`: AtomicType = value.asInstanceOf[AtomicValue].getItemType
      subsumesUnderlyingType(`type`)
    }

    def subsumes(other: ItemType): Boolean = {
      val otherType: net.sf.saxon.model.ItemType = other.getUnderlyingItemType
      if (!otherType.isPlainType) {
        return false
      }
      val `type`: AtomicType = otherType.asInstanceOf[AtomicType]
      subsumesUnderlyingType(`type`)
    }

    private def subsumesUnderlyingType(`type`: AtomicType): Boolean = {
      var builtIn: BuiltInAtomicType =
        if (`type`.isInstanceOf[BuiltInAtomicType])
          `type`.asInstanceOf[BuiltInAtomicType]
        else `type`.getBuiltInBaseType.asInstanceOf[BuiltInAtomicType]
      while (true) {
        if (builtIn.isSameType(underlyingType)) {
          return true
        }
        val base: SchemaType = builtIn.getBaseType
        if (!(base.isInstanceOf[BuiltInAtomicType])) {
          return false
        }
        builtIn = base.asInstanceOf[BuiltInAtomicType]
      }
      false
    }

    def getUnderlyingItemType(): net.sf.saxon.model.ItemType = underlyingType

  }

  val STRING: ItemType =
    atomic(BuiltInAtomicType.STRING, defaultConversionRules)

  val BOOLEAN: ItemType =
    atomic(BuiltInAtomicType.BOOLEAN, defaultConversionRules)

  val DURATION: ItemType =
    atomic(BuiltInAtomicType.DURATION, defaultConversionRules)

  val DATE_TIME: ItemType =
    atomic(BuiltInAtomicType.DATE_TIME, defaultConversionRules)

  val DATE: ItemType = atomic(BuiltInAtomicType.DATE, defaultConversionRules)

  val TIME: ItemType = atomic(BuiltInAtomicType.TIME, defaultConversionRules)

  val G_YEAR_MONTH: ItemType =
    atomic(BuiltInAtomicType.G_YEAR_MONTH, defaultConversionRules)

  val G_MONTH: ItemType =
    atomic(BuiltInAtomicType.G_MONTH, defaultConversionRules)

  val G_MONTH_DAY: ItemType =
    atomic(BuiltInAtomicType.G_MONTH_DAY, defaultConversionRules)

  val G_YEAR: ItemType =
    atomic(BuiltInAtomicType.G_YEAR, defaultConversionRules)

  val G_DAY: ItemType = atomic(BuiltInAtomicType.G_DAY, defaultConversionRules)

  val HEX_BINARY: ItemType =
    atomic(BuiltInAtomicType.HEX_BINARY, defaultConversionRules)

  val BASE64_BINARY: ItemType =
    atomic(BuiltInAtomicType.BASE64_BINARY, defaultConversionRules)

  val ANY_URI: ItemType =
    atomic(BuiltInAtomicType.ANY_URI, defaultConversionRules)

  val QNAME: ItemType = atomic(BuiltInAtomicType.QNAME, defaultConversionRules)

  val NOTATION: ItemType =
    atomic(BuiltInAtomicType.NOTATION, defaultConversionRules)

  val UNTYPED_ATOMIC: ItemType =
    atomic(BuiltInAtomicType.UNTYPED_ATOMIC, defaultConversionRules)

  val DECIMAL: ItemType =
    atomic(BuiltInAtomicType.DECIMAL, defaultConversionRules)

  val FLOAT: ItemType = atomic(BuiltInAtomicType.FLOAT, defaultConversionRules)

  val DOUBLE: ItemType =
    atomic(BuiltInAtomicType.DOUBLE, defaultConversionRules)

  val INTEGER: ItemType =
    atomic(BuiltInAtomicType.INTEGER, defaultConversionRules)

  val NON_POSITIVE_INTEGER: ItemType =
    atomic(BuiltInAtomicType.NON_POSITIVE_INTEGER, defaultConversionRules)

  val NEGATIVE_INTEGER: ItemType =
    atomic(BuiltInAtomicType.NEGATIVE_INTEGER, defaultConversionRules)

  val LONG: ItemType = atomic(BuiltInAtomicType.LONG, defaultConversionRules)

  val INT: ItemType = atomic(BuiltInAtomicType.INT, defaultConversionRules)

  val SHORT: ItemType = atomic(BuiltInAtomicType.SHORT, defaultConversionRules)

  val BYTE: ItemType = atomic(BuiltInAtomicType.BYTE, defaultConversionRules)

  val NON_NEGATIVE_INTEGER: ItemType =
    atomic(BuiltInAtomicType.NON_NEGATIVE_INTEGER, defaultConversionRules)

  val POSITIVE_INTEGER: ItemType =
    atomic(BuiltInAtomicType.POSITIVE_INTEGER, defaultConversionRules)

  val UNSIGNED_LONG: ItemType =
    atomic(BuiltInAtomicType.UNSIGNED_LONG, defaultConversionRules)

  val UNSIGNED_INT: ItemType =
    atomic(BuiltInAtomicType.UNSIGNED_INT, defaultConversionRules)

  val UNSIGNED_SHORT: ItemType =
    atomic(BuiltInAtomicType.UNSIGNED_SHORT, defaultConversionRules)

  val UNSIGNED_BYTE: ItemType =
    atomic(BuiltInAtomicType.UNSIGNED_BYTE, defaultConversionRules)

  val YEAR_MONTH_DURATION: ItemType =
    atomic(BuiltInAtomicType.YEAR_MONTH_DURATION, defaultConversionRules)

  val DAY_TIME_DURATION: ItemType =
    atomic(BuiltInAtomicType.DAY_TIME_DURATION, defaultConversionRules)

  val NORMALIZED_STRING: ItemType =
    atomic(BuiltInAtomicType.NORMALIZED_STRING, defaultConversionRules)

  val TOKEN: ItemType = atomic(BuiltInAtomicType.TOKEN, defaultConversionRules)

  val LANGUAGE: ItemType =
    atomic(BuiltInAtomicType.LANGUAGE, defaultConversionRules)

  val NAME: ItemType = atomic(BuiltInAtomicType.NAME, defaultConversionRules)

  val NMTOKEN: ItemType =
    atomic(BuiltInAtomicType.NMTOKEN, defaultConversionRules)

  val NCNAME: ItemType =
    atomic(BuiltInAtomicType.NCNAME, defaultConversionRules)

  val ID: ItemType = atomic(BuiltInAtomicType.ID, defaultConversionRules)

  val IDREF: ItemType = atomic(BuiltInAtomicType.IDREF, defaultConversionRules)

  val ENTITY: ItemType =
    atomic(BuiltInAtomicType.ENTITY, defaultConversionRules)

  val DATE_TIME_STAMP: ItemType =
    atomic(BuiltInAtomicType.DATE_TIME_STAMP, defaultConversionRules)

  private def atomic(underlyingType: BuiltInAtomicType,
                     conversionRules: ConversionRules): ItemType =
    new BuiltInAtomicItemType(underlyingType, conversionRules)

  val NUMERIC: ItemType = new ItemType {
    override def getConversionRules(): ConversionRules = defaultConversionRules

    override def matches(item: XdmItem): Boolean =
      item.getUnderlyingValue.isInstanceOf[NumericValue]

    override def subsumes(other: ItemType): Boolean =
      DECIMAL.subsumes(other) || DOUBLE.subsumes(other) || FLOAT.subsumes(
        other)

    override def getUnderlyingItemType(): net.sf.saxon.model.ItemType =
      NumericType.getInstance
  }

}

abstract class ItemType {

  def getConversionRules: ConversionRules = defaultConversionRules

  def matches(item: XdmItem): Boolean

  def subsumes(other: ItemType): Boolean

  def getUnderlyingItemType: net.sf.saxon.model.ItemType

  def getTypeName: QName = {
    val `type`: net.sf.saxon.model.ItemType = getUnderlyingItemType
    if (`type`.isInstanceOf[SchemaType]) {
      val name: StructuredQName =
        `type`.asInstanceOf[SchemaType].getStructuredQName
      if (name == null) null else new QName(name)
    } else {
      null
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: ItemType =>
      getUnderlyingItemType == other.getUnderlyingItemType
    case _ => false

  }

  override def hashCode: Int = getUnderlyingItemType.hashCode

  override def toString: String = {
    val `type`: net.sf.saxon.model.ItemType = getUnderlyingItemType
    if (`type`.isInstanceOf[SchemaType]) {
      var marker: String = ""
      var st: SchemaType = `type`.asInstanceOf[SchemaType]
      var name: StructuredQName = null
      while (true) {
        name = st.getStructuredQName
        if (name != null) {
          marker + name.getEQName
        } else {
          marker = "<"
          st = st.getBaseType
          if (st == null) {
            "Q{" + NamespaceConstant.SCHEMA + "}anyType"
          }
        }
      }
      ""
    } else {
      `type`.toString
    }
  }

}
