package net.sf.saxon.s9api

import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.model._
import net.sf.saxon.value._
import java.math.BigDecimal
import java.math.BigInteger
import java.net.URI
import java.time._

import net.sf.saxon.s9api

object XdmAtomicValue {

  def makeAtomicValue(value: AnyRef): XdmAtomicValue =
    if (value.isInstanceOf[AtomicValue]) {
      new XdmAtomicValue(value.asInstanceOf[AtomicValue], true)
    } else if (value.isInstanceOf[java.lang.Boolean]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Boolean])
    } else if (value.isInstanceOf[java.lang.Integer]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Integer])
    } else if (value.isInstanceOf[java.lang.Long]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Long])
    } else if (value.isInstanceOf[java.lang.Short]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Short])
    } else if (value.isInstanceOf[java.lang.Character]) {
      new XdmAtomicValue(value.asInstanceOf[String])
    } else if (value.isInstanceOf[java.lang.Byte]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Byte])
    } else if (value.isInstanceOf[String]) {
      new XdmAtomicValue(value.asInstanceOf[String])
    } else if (value.isInstanceOf[java.lang.Double]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Double])
    } else if (value.isInstanceOf[java.lang.Float]) {
      new XdmAtomicValue(value.asInstanceOf[java.lang.Float])
    } else if (value.isInstanceOf[BigDecimal]) {
      new XdmAtomicValue(value.asInstanceOf[BigDecimal])
    } else if (value.isInstanceOf[BigInteger]) {
      new XdmAtomicValue(
        IntegerValue.makeIntegerValue(value.asInstanceOf[BigInteger]),
        true)
    } else if (value.isInstanceOf[URI]) {
      new XdmAtomicValue(value.asInstanceOf[URI])
    } else if (value.isInstanceOf[QName]) {
      new XdmAtomicValue(value.asInstanceOf[QName])
    } else if (value.isInstanceOf[ZonedDateTime]) {
      new XdmAtomicValue(value.asInstanceOf[ZonedDateTime])
    } else if (value.isInstanceOf[LocalDateTime]) {
      new XdmAtomicValue(value.asInstanceOf[LocalDateTime])
    } else if (value.isInstanceOf[LocalDate]) {
      new XdmAtomicValue(value.asInstanceOf[LocalDate])
    } else if (value.isInstanceOf[XdmAtomicValue]) {
      value.asInstanceOf[XdmAtomicValue]
    } else {
      throw new IllegalArgumentException(value.toString)
    }

}

class XdmAtomicValue extends XdmItem {

  def this(value: AtomicValue, flag: Boolean) = {
    this()
    this.setValue(value)
  }

  def this(value: Boolean) = this(BooleanValue.get(value), true)

  def this(value: Long) =
    this(Int64Value.makeDerived(value, BuiltInAtomicType.LONG), true)

  def this(value: Int) =
    this(Int64Value.makeDerived(value, BuiltInAtomicType.INT), true)

  def this(value: Short) =
    this(Int64Value.makeDerived(value, BuiltInAtomicType.SHORT), true)

  def this(value: Byte) =
    this(Int64Value.makeDerived(value, BuiltInAtomicType.BYTE), true)

  def this(value: BigDecimal) = this(new BigDecimalValue(value), true)

  def this(value: Double) = this(new DoubleValue(value), true)

  def this(value: Float) = this(new FloatValue(value), true)

  def this(value: String) = this(new StringValue(value), true)

  def this(value: URI) = this(new AnyURIValue(value.toString), true)

  def this(value: QName) =
    this(new QNameValue(value.getStructuredQName, BuiltInAtomicType.QNAME),
      true)

  def this(value: Instant) = this(DateTimeValue.fromJavaInstant(value), true)

  def this(value: ZonedDateTime) =
    this(DateTimeValue.fromZonedDateTime(value), true)

  def this(value: OffsetDateTime) =
    this(DateTimeValue.fromOffsetDateTime(value), true)

  def this(value: LocalDateTime) =
    this(DateTimeValue.fromLocalDateTime(value), true)

  def this(value: LocalDate) = this(new DateValue(value), true)

  def this(lexicalForm: String, `type`: s9api.ItemType) = {
    this()
    val it: net.sf.saxon.model.ItemType = `type`.getUnderlyingItemType
    if (!it.isPlainType) {
      throw new SaxonApiException("Requested type is not atomic")
    }
    if (it.asInstanceOf[AtomicType].isAbstract) {
      throw new SaxonApiException("Requested type is an abstract type")
    }
    if (it.asInstanceOf[AtomicType].isNamespaceSensitive) {
      throw new SaxonApiException("Requested type is namespace-sensitive")
    }
    val converter: StringConverter =
      it.asInstanceOf[AtomicType].getStringConverter(`type`.getConversionRules)
    this.setValue(converter.convertString(lexicalForm).asAtomic())
  }

  override def getUnderlyingValue: AtomicValue =
    super.getUnderlyingValue.asInstanceOf[AtomicValue]

  override def toString: String = getStringValue

  def getPrimitiveTypeName: QName = {
    val value: AtomicValue = getUnderlyingValue
    val `type`: BuiltInAtomicType = value.getPrimitiveType
    new QName(`type`.getStructuredQName)
  }

  def getTypeName: QName = {
    val value: AtomicValue = getUnderlyingValue
    val `type`: AtomicType = value.getItemType
    new QName(`type`.getStructuredQName)
  }

  def getValue: Any = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[StringValue]) {
      av.getStringValue
    } else if (av.isInstanceOf[IntegerValue]) {
      av.asInstanceOf[IntegerValue].asBigInteger()
    } else if (av.isInstanceOf[DoubleValue]) {
      av.asInstanceOf[DoubleValue].getDoubleValue
    } else if (av.isInstanceOf[FloatValue]) {
      av.asInstanceOf[FloatValue].getFloatValue
    } else if (av.isInstanceOf[BooleanValue]) {
      av.asInstanceOf[BooleanValue].getBooleanValue
    } else if (av.isInstanceOf[BigDecimalValue]) {
      av.asInstanceOf[BigDecimalValue].getDecimalValue
    } else if (av.isInstanceOf[DateTimeValue]) {
      if (av.asInstanceOf[DateTimeValue].hasTimezone) {
        av.asInstanceOf[DateTimeValue].toZonedDateTime
      } else {
        av.asInstanceOf[DateTimeValue].toLocalDateTime
      }
    } else if (av.isInstanceOf[DateValue]) {
      av.asInstanceOf[DateValue].toLocalDate
    } else if (av.isInstanceOf[QNameValue]) {
      val q: QNameValue = av.asInstanceOf[QNameValue]
      new QName(q.getPrefix, q.getNamespaceURI, q.getLocalName)
    } else {
      av.getStringValue
    }
  }

  def getBooleanValue: Boolean = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[BooleanValue]) {
      av.asInstanceOf[BooleanValue].getBooleanValue
    } else if (av.isInstanceOf[NumericValue]) {
      !av.isNaN && av.asInstanceOf[NumericValue].signum() != 0
    } else if (av.isInstanceOf[StringValue]) {
      val s: String = av.getStringValue.trim()
      "1" == s || "true" == s
    } else {
      throw new SaxonApiException("Cannot cast item to a boolean")
    }
  }

  def getLongValue: Long = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[BooleanValue]) {
      if (av.asInstanceOf[BooleanValue].getBooleanValue) 0L else 1L
    } else if (av.isInstanceOf[NumericValue]) {
      av.asInstanceOf[NumericValue].longValue()
    } else if (av.isInstanceOf[StringValue]) {
      val converter: StringToDouble = StringToDouble.getInstance
      converter.stringToNumber(av.getStringValueCS).toLong
    } else {
      throw new SaxonApiException("Cannot cast item to an integer")
    }
  }

  def getDoubleValue: Double = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[BooleanValue]) {
      if (av.asInstanceOf[BooleanValue].getBooleanValue) 0.0 else 1.0
    } else if (av.isInstanceOf[NumericValue]) {
      av.asInstanceOf[NumericValue].getDoubleValue
    } else if (av.isInstanceOf[StringValue]) {
      val converter: StringToDouble = StringToDouble11.getInstance
      converter.stringToNumber(av.getStringValueCS)
    } else {
      throw new SaxonApiException("Cannot cast item to a double")
    }
  }

  def getDecimalValue: BigDecimal = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[BooleanValue]) {
      if (av.asInstanceOf[BooleanValue].getBooleanValue) BigDecimal.ZERO
      else BigDecimal.ONE
    } else if (av.isInstanceOf[NumericValue]) {
      av.asInstanceOf[NumericValue].getDecimalValue
    } else if (av.isInstanceOf[StringValue]) {
      new BigDecimal(av.getStringValueCS.toString)
    } else {
      throw new SaxonApiException("Cannot cast item to a decimal")
    }
  }

  def getQNameValue: QName = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[QualifiedNameValue]) {
      new QName(av.asInstanceOf[QualifiedNameValue].getStructuredQName)
    } else {
      null
    }
  }

  def getInstant: Instant = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[DateTimeValue] && av
      .asInstanceOf[DateTimeValue]
      .hasTimezone) {
      av.asInstanceOf[DateTimeValue].toJavaInstant
    } else {
      null
    }
  }

  def getZonedDateTime: ZonedDateTime = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[DateTimeValue] && av
      .asInstanceOf[DateTimeValue]
      .hasTimezone) {
      av.asInstanceOf[DateTimeValue].toZonedDateTime
    } else {
      null
    }
  }

  def getOffsetDateTime: OffsetDateTime = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[DateTimeValue] && av
      .asInstanceOf[DateTimeValue]
      .hasTimezone) {
      av.asInstanceOf[DateTimeValue].toOffsetDateTime
    } else {
      null
    }
  }

  def getLocalDateTime: LocalDateTime = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[DateTimeValue]) {
      av.asInstanceOf[DateTimeValue].toLocalDateTime
    } else {
      null
    }
  }

  def getLocalDate: LocalDate = {
    val av: AtomicValue = getUnderlyingValue
    if (av.isInstanceOf[DateValue]) {
      av.asInstanceOf[DateValue].toLocalDate
    } else {
      null
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: XdmAtomicValue => {
      val a: AtomicMatchKey = getUnderlyingValue.asMapKey()
      val b: AtomicMatchKey = other.getUnderlyingValue.asMapKey()
      a == b
    }
    case _ => false

  }

  override def hashCode: Int = getUnderlyingValue.asMapKey().hashCode

}
