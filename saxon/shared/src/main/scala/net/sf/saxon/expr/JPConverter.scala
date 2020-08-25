package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.lib.ExternalObjectModel
import net.sf.saxon.lib.ParseOptions
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.model.{AnyFunctionType, AnyItemType, BuiltInAtomicType, ErrorType, ItemType, JavaExternalObjectType}
import net.sf.saxon.om._
import net.sf.saxon.pattern.AnyNodeTest
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.s9api._
import net.sf.saxon.trans.SaxonErrorCode
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value._
import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import java.lang.reflect.ParameterizedType
import java.math.BigDecimal
import java.math.BigInteger
import java.net.URI
import java.net.URL
import java.time._
import java.util._

import net.sf.saxon.s9apir.XdmFunctionItem

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

object JPConverter {

  var converterMap: HashMap[Class[_], JPConverter] = new HashMap()

  converterMap.put(classOf[XdmValue],
    new FromXdmValue(AnyItemType.getInstance,
      StaticProperty.ALLOWS_ZERO_OR_MORE))

  converterMap.put(
    classOf[XdmItem],
    new FromXdmValue(AnyItemType.getInstance, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmAtomicValue],
    new FromXdmValue(BuiltInAtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmNode],
    new FromXdmValue(AnyNodeTest.getInstance, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmFunctionItem],
    new FromXdmValue(AnyFunctionType.getInstance, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmMap],
    new FromXdmValue(MapType.ANY_MAP_TYPE, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmArray],
    new FromXdmValue(ArrayItemType.ANY_ARRAY_TYPE, StaticProperty.ALLOWS_ONE))

  converterMap.put(
    classOf[XdmEmptySequence],
    new FromXdmValue(ErrorType.getInstance, StaticProperty.ALLOWS_ZERO))

  converterMap.put(classOf[SequenceIterator], FromSequenceIterator.INSTANCE)

  converterMap.put(classOf[Sequence], FromSequence.INSTANCE)

  converterMap.put(classOf[OneOrMore[_ <: Item]], FromSequence.INSTANCE)

  converterMap.put(classOf[One[_ <: Item]], FromSequence.INSTANCE)

  converterMap.put(classOf[ZeroOrOne[_ <: Item]], FromSequence.INSTANCE)

  converterMap.put(classOf[ZeroOrMore[_ <: Item]], FromSequence.INSTANCE)

  converterMap.put(classOf[String], FromString.INSTANCE)

  converterMap.put(classOf[Boolean], FromBoolean.INSTANCE)

  converterMap.put(classOf[Boolean], FromBoolean.INSTANCE)

  converterMap.put(classOf[Double], FromDouble.INSTANCE)

  converterMap.put(classOf[Double], FromDouble.INSTANCE)

  converterMap.put(classOf[Float], FromFloat.INSTANCE)

  converterMap.put(classOf[Float], FromFloat.INSTANCE)

  converterMap.put(classOf[BigDecimal], FromBigDecimal.INSTANCE)

  converterMap.put(classOf[BigInteger], FromBigInteger.INSTANCE)

  converterMap.put(classOf[Long], FromLong.INSTANCE)

  converterMap.put(classOf[Long], FromLong.INSTANCE)

  converterMap.put(classOf[Integer], FromInt.INSTANCE)

  converterMap.put(classOf[Int], FromInt.INSTANCE)

  converterMap.put(classOf[Short], FromShort.INSTANCE)

  converterMap.put(classOf[Short], FromShort.INSTANCE)

  converterMap.put(classOf[Byte], FromByte.INSTANCE)

  converterMap.put(classOf[Byte], FromByte.INSTANCE)

  converterMap.put(classOf[Character], FromCharacter.INSTANCE)

  converterMap.put(classOf[Char], FromCharacter.INSTANCE)

  converterMap.put(classOf[URI], FromURI.INSTANCE)

  converterMap.put(classOf[URL], FromURI.INSTANCE)

  converterMap.put(classOf[Date], FromDate.INSTANCE)

  converterMap.put(classOf[Instant], FromInstant.INSTANCE)

  converterMap.put(classOf[LocalDateTime], FromLocalDateTime.INSTANCE)

  converterMap.put(classOf[ZonedDateTime], FromZonedDateTime.INSTANCE)

  converterMap.put(classOf[OffsetDateTime], FromOffsetDateTime.INSTANCE)

  converterMap.put(classOf[LocalDate], FromLocalDate.INSTANCE)

  converterMap.put(classOf[Array[Long]], FromLongArray.INSTANCE)

  converterMap.put(classOf[Array[Int]], FromIntArray.INSTANCE)

  converterMap.put(classOf[Array[Short]], FromShortArray.INSTANCE)

  converterMap.put(classOf[Array[Byte]], FromByteArray.INSTANCE)

  converterMap.put(classOf[Array[Char]], FromCharArray.INSTANCE)

  converterMap.put(classOf[Array[Double]], FromDoubleArray.INSTANCE)

  converterMap.put(classOf[Array[Float]], FromFloatArray.INSTANCE)

  converterMap.put(classOf[Array[Boolean]], FromBooleanArray.INSTANCE)

  converterMap.put(classOf[Collection[_]], FromCollection.INSTANCE)

  var itemTypeMap: Map[Class[_], ItemType] = new HashMap()

  itemTypeMap.put(classOf[BooleanValue], BuiltInAtomicType.BOOLEAN)

  itemTypeMap.put(classOf[StringValue], BuiltInAtomicType.STRING)

  itemTypeMap.put(classOf[DoubleValue], BuiltInAtomicType.DOUBLE)

  itemTypeMap.put(classOf[FloatValue], BuiltInAtomicType.FLOAT)

  itemTypeMap.put(classOf[BigDecimalValue], BuiltInAtomicType.DECIMAL)

  itemTypeMap.put(classOf[IntegerValue], BuiltInAtomicType.INTEGER)

  itemTypeMap.put(classOf[DurationValue], BuiltInAtomicType.DURATION)

  itemTypeMap.put(classOf[DayTimeDurationValue],
    BuiltInAtomicType.DAY_TIME_DURATION)

  itemTypeMap.put(classOf[YearMonthDurationValue],
    BuiltInAtomicType.YEAR_MONTH_DURATION)

  itemTypeMap.put(classOf[DateTimeValue], BuiltInAtomicType.DATE_TIME)

  itemTypeMap.put(classOf[DateValue], BuiltInAtomicType.DATE)

  itemTypeMap.put(classOf[TimeValue], BuiltInAtomicType.TIME)

  itemTypeMap.put(classOf[GYearValue], BuiltInAtomicType.G_YEAR)

  itemTypeMap.put(classOf[GYearMonthValue], BuiltInAtomicType.G_YEAR_MONTH)

  itemTypeMap.put(classOf[GMonthValue], BuiltInAtomicType.G_MONTH)

  itemTypeMap.put(classOf[GMonthDayValue], BuiltInAtomicType.G_MONTH_DAY)

  itemTypeMap.put(classOf[GDayValue], BuiltInAtomicType.G_DAY)

  itemTypeMap.put(classOf[AnyURIValue], BuiltInAtomicType.ANY_URI)

  itemTypeMap.put(classOf[QNameValue], BuiltInAtomicType.QNAME)

  itemTypeMap.put(classOf[NotationValue], BuiltInAtomicType.NOTATION)

  itemTypeMap.put(classOf[HexBinaryValue], BuiltInAtomicType.HEX_BINARY)

  itemTypeMap.put(classOf[Base64BinaryValue], BuiltInAtomicType.BASE64_BINARY)

  itemTypeMap.put(classOf[NodeInfo], AnyNodeTest.getInstance)

  itemTypeMap.put(classOf[TreeInfo], NodeKindTest.DOCUMENT)

  itemTypeMap.put(classOf[MapItem], MapType.EMPTY_MAP_TYPE)

  itemTypeMap.put(classOf[ArrayItem], ArrayItemType.ANY_ARRAY_TYPE)

  itemTypeMap.put(classOf[Function], AnyFunctionType.getInstance)

  itemTypeMap.put(classOf[AtomicValue], BuiltInAtomicType.ANY_ATOMIC)

  itemTypeMap.put(classOf[UntypedAtomicValue],
    BuiltInAtomicType.UNTYPED_ATOMIC)

  var cardinalityMap: Map[Class[_], Integer] = new HashMap()

  cardinalityMap.put(classOf[Sequence], StaticProperty.ALLOWS_ZERO_OR_MORE)

  cardinalityMap.put(classOf[ZeroOrMore[_ <: Item]], StaticProperty.ALLOWS_ZERO_OR_MORE)

  cardinalityMap.put(classOf[OneOrMore[_ <: Item]], StaticProperty.ALLOWS_ONE_OR_MORE)

  cardinalityMap.put(classOf[One[_ <: Item]], StaticProperty.EXACTLY_ONE)

  cardinalityMap.put(classOf[ZeroOrOne[_ <: Item]], StaticProperty.ALLOWS_ZERO_OR_ONE)

  cardinalityMap.put(classOf[XdmValue], StaticProperty.ALLOWS_ZERO_OR_MORE)

  cardinalityMap.put(classOf[XdmItem], StaticProperty.ALLOWS_ZERO_OR_MORE)

  cardinalityMap.put(classOf[XdmEmptySequence], StaticProperty.ALLOWS_ZERO)

  def allocate(javaClass: Class[_],
               genericType: java.lang.reflect.Type,
               config: Configuration): JPConverter = {
    if (classOf[javax.xml.namespace.QName].isAssignableFrom(javaClass)) {
      FromQName.INSTANCE
    }
    if (classOf[Sequence].isAssignableFrom(javaClass)) {
      if (genericType.isInstanceOf[ParameterizedType]) {
        val params: Array[java.lang.reflect.Type] =
          genericType.asInstanceOf[ParameterizedType].getActualTypeArguments
        if (params.length == 1 && params(0).isInstanceOf[Class[_]] &&
          classOf[Item].isAssignableFrom(params(0).asInstanceOf[Class[_]])) {
          val itemType: ItemType =
            itemTypeMap.get(params(0).asInstanceOf[Class[_]])
          val cardinality: java.lang.Integer = cardinalityMap.get(javaClass)
          if (itemType != null && cardinality != null) {
            new FromSequence(itemType, cardinality)
          }
        }
      } else {
        val itemType: ItemType = itemTypeMap.get(javaClass)
        if (itemType != null) {
          new FromSequence(itemType, StaticProperty.ALLOWS_ZERO_OR_ONE)
        }
      }
    }
    val c: JPConverter = converterMap.get(javaClass)
    if (c != null) {
      return c
    }
    if (javaClass == classOf[AnyRef]) {
      FromObject.INSTANCE
    }
    if (classOf[NodeInfo].isAssignableFrom(javaClass)) {
      new FromSequence(AnyNodeTest.getInstance,
        StaticProperty.ALLOWS_ZERO_OR_ONE)
    }
    if (classOf[Source].isAssignableFrom(javaClass) && !classOf[DOMSource]
      .isAssignableFrom(javaClass)) {
      FromSource.INSTANCE
    }
    for ((key, value) <- converterMap.asScala if key.isAssignableFrom(javaClass)) {
      value
    }
    val externalObjectModels: List[ExternalObjectModel] =
      config.getExternalObjectModels
    for (model <- externalObjectModels.asScala) {
      val converter: JPConverter = model.getJPConverter(javaClass, config)
      if (converter != null) {
        converter
      }
    }
    if (javaClass.isArray) {
      val itemClass: Class[_] = javaClass.getComponentType
      new FromObjectArray(allocate(itemClass, null, config))
    }
    if (javaClass == Void.TYPE) {
      VoidConverter.INSTANCE
    }
    new ExternalObjectWrapper(config.getJavaExternalObjectType(javaClass))
  }

  object FromObject {

    val INSTANCE: FromObject = new FromObject()

  }

  class FromObject extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val theClass: Class[_] = `object`.getClass
      var instanceConverter: JPConverter =
        allocate(theClass, null, context.getConfiguration)
      if (instanceConverter.isInstanceOf[FromObject]) {
        instanceConverter = new ExternalObjectWrapper(
          context.getConfiguration.getJavaExternalObjectType(theClass))
      }
      instanceConverter.convert(`object`, context)
    }

    def getItemType(): ItemType = AnyItemType.getInstance

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromSequenceIterator {

    val INSTANCE: FromSequenceIterator = new FromSequenceIterator()

  }

  class FromSequenceIterator extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence =
      `object`.asInstanceOf[SequenceIterator].materialize()

    def getItemType(): ItemType = AnyItemType.getInstance

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  class FromXdmValue(var resultType: ItemType,
                     @BeanProperty var cardinalityInt: Int = 0)
    extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence =
      `object`.asInstanceOf[XdmValue].getUnderlyingValue

    def getItemType(): ItemType = resultType

  }

  object FromSequence {

    val INSTANCE: FromSequence = new FromSequence(
      AnyItemType.getInstance,
      StaticProperty.ALLOWS_ZERO_OR_MORE)

  }

  class FromSequence(var resultType: ItemType,
                     @BeanProperty var cardinalityInt: Int)
    extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence =
      if (`object`.isInstanceOf[Closure])
        `object`.asInstanceOf[Closure].iterate().materialize()
      else `object`.asInstanceOf[Sequence]

    def getItemType(): ItemType = resultType

  }

  object FromString {

    val INSTANCE: FromString = new FromString()

  }

  class FromString extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): StringValue =
      new StringValue(`object`.asInstanceOf[String])

    def getItemType(): ItemType = BuiltInAtomicType.STRING

  }

  object FromBoolean {

    val INSTANCE: FromBoolean = new FromBoolean()

  }

  class FromBoolean extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): BooleanValue =
      BooleanValue.get(`object`.asInstanceOf[java.lang.Boolean])

    def getItemType(): ItemType = BuiltInAtomicType.BOOLEAN

  }

  object FromDouble {

    val INSTANCE: FromDouble = new FromDouble()

  }

  class FromDouble extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DoubleValue =
      new DoubleValue(`object`.asInstanceOf[java.lang.Double])

    def getItemType(): ItemType = BuiltInAtomicType.DOUBLE

  }

  object FromFloat {

    val INSTANCE: FromFloat = new FromFloat()

  }

  class FromFloat extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): FloatValue =
      new FloatValue(`object`.asInstanceOf[java.lang.Float])

    def getItemType(): ItemType = BuiltInAtomicType.FLOAT

  }

  object FromBigDecimal {

    val INSTANCE: FromBigDecimal = new FromBigDecimal()

  }

  class FromBigDecimal extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): BigDecimalValue =
      new BigDecimalValue(`object`.asInstanceOf[BigDecimal])

    def getItemType(): ItemType = BuiltInAtomicType.DECIMAL

  }

  object FromBigInteger {

    val INSTANCE: FromBigInteger = new FromBigInteger()

  }

  class FromBigInteger extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): IntegerValue =
      IntegerValue.makeIntegerValue(`object`.asInstanceOf[BigInteger])

    def getItemType(): ItemType = BuiltInAtomicType.INTEGER

  }

  object FromLong {

    val INSTANCE: FromLong = new FromLong()

  }

  class FromLong extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(`object`.asInstanceOf[java.lang.Long])

    def getItemType(): ItemType = BuiltInAtomicType.INTEGER

  }

  object FromInt {

    val INSTANCE: FromInt = new FromInt()

  }

  class FromInt extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Int64Value = new Int64Value(`object`.asInstanceOf[Long])

    def getItemType(): ItemType = BuiltInAtomicType.INTEGER

  }

  object FromShort {

    val INSTANCE: FromShort = new FromShort()

  }

  class FromShort extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(`object`.asInstanceOf[java.lang.Short].intValue())

    def getItemType(): ItemType = BuiltInAtomicType.INTEGER

  }

  object FromByte {

    val INSTANCE: FromByte = new FromByte()

  }

  class FromByte extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(`object`.asInstanceOf[java.lang.Byte].intValue())

    def getItemType(): ItemType = BuiltInAtomicType.INTEGER

  }

  object FromCharacter {

    val INSTANCE: FromCharacter = new FromCharacter()

  }

  class FromCharacter extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): StringValue =
      new StringValue(`object`.toString)

    def getItemType(): ItemType = BuiltInAtomicType.STRING

  }

  object FromQName {

    val INSTANCE: FromQName = new FromQName()

  }

  class FromQName extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): QNameValue = {
      val qn: javax.xml.namespace.QName =
        `object`.asInstanceOf[javax.xml.namespace.QName]
      new QNameValue(qn.getPrefix, qn.getNamespaceURI, qn.getLocalPart)
    }

    def getItemType(): ItemType = BuiltInAtomicType.QNAME

  }

  object FromURI {

    val INSTANCE: FromURI = new FromURI()

  }

  class FromURI extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): AnyURIValue =
      new AnyURIValue(`object`.toString)

    def getItemType(): ItemType = BuiltInAtomicType.ANY_URI

  }

  object FromDate {

    val INSTANCE: FromDate = new FromDate()

  }

  class FromDate extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromJavaDate(`object`.asInstanceOf[Date])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  object FromInstant {

    val INSTANCE: FromInstant = new FromInstant()

  }

  class FromInstant extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromJavaInstant(`object`.asInstanceOf[Instant])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  object FromZonedDateTime {

    val INSTANCE: FromZonedDateTime = new FromZonedDateTime()

  }

  class FromZonedDateTime extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromZonedDateTime(`object`.asInstanceOf[ZonedDateTime])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  object FromOffsetDateTime {

    val INSTANCE: FromOffsetDateTime = new FromOffsetDateTime()

  }

  class FromOffsetDateTime extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromOffsetDateTime(`object`.asInstanceOf[OffsetDateTime])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  object FromLocalDateTime {

    val INSTANCE: FromLocalDateTime = new FromLocalDateTime()

  }

  class FromLocalDateTime extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromLocalDateTime(`object`.asInstanceOf[LocalDateTime])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  object FromLocalDate {

    val INSTANCE: FromLocalDate = new FromLocalDate()

  }

  class FromLocalDate extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): DateValue =
      new DateValue(`object`.asInstanceOf[LocalDate])

    def getItemType(): ItemType = BuiltInAtomicType.DATE_TIME

  }

  class ExternalObjectWrapper(var resultType: JavaExternalObjectType)
    extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): ExternalObject[Any] =
      if (`object` == null) {
        null
      } else if (resultType.getJavaClass.isInstance(`object`)) {
        new ObjectValue(`object`)
      } else {
        throw new XPathException(
          "Java external object of type " + `object`.getClass.getName +
            " is not an instance of the required type " +
            resultType.getJavaClass.getName,
          "XPTY0004")
      }

    def getItemType(): JavaExternalObjectType = resultType

  }

  object VoidConverter {

    val INSTANCE: VoidConverter = new VoidConverter()

  }

  class VoidConverter extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): EmptySequence[_ <: Item] = EmptySequence.getInstance

    def getItemType(): ItemType = AnyItemType.getInstance

  }

  object FromCollection {

    val INSTANCE: FromCollection = new FromCollection()

  }

  class FromCollection extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val list: List[Item] =
        new ArrayList[Item](`object`.asInstanceOf[Collection[_]].size)
      val a: Int = 0
      for (obj <- `object`.asInstanceOf[Collection[_]].asScala) {
        val itemConverter: JPConverter =
          allocate(obj.getClass, null, context.getConfiguration)
        val item: Item =
          SequenceTool.asItem(itemConverter.convert(obj.asInstanceOf[AnyRef], context))
        if (item != null) {
          list.add(item)
        }
      }
      new SequenceExtent(list)
    }

    def getItemType(): ItemType = AnyItemType.getInstance()

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromSource {

    val INSTANCE: FromSource = new FromSource()

  }

  class FromSource extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): NodeInfo = {
      val options: ParseOptions = new ParseOptions()
      val controller: Controller = context.getController
      if (controller != null) {
        options.setSchemaValidationMode(controller.getSchemaValidationMode)
      }
      context.getConfiguration
        .buildDocumentTree(`object`.asInstanceOf[Source], options)
        .getRootNode
    }

    def getItemType(): ItemType = AnyNodeTest.getInstance

  }

  object FromLongArray {

    val INSTANCE: FromLongArray = new FromLongArray()

  }

  class FromLongArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Long]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          `object`.asInstanceOf[Array[Long]](i),
          BuiltInAtomicType.LONG)
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.LONG

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromIntArray {

    val INSTANCE: FromIntArray = new FromIntArray()

  }

  class FromIntArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Int]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(`object`.asInstanceOf[Array[Int]](i),
          BuiltInAtomicType.INT)
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.INT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromShortArray {

    val INSTANCE: FromShortArray = new FromShortArray()

  }

  class FromShortArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Short]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          `object`.asInstanceOf[Array[Short]](i),
          BuiltInAtomicType.SHORT)
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.SHORT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromByteArray {

    val INSTANCE: FromByteArray = new FromByteArray()

  }

  class FromByteArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Byte]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          255 & `object`.asInstanceOf[Array[Byte]](i).toInt,
          BuiltInAtomicType.UNSIGNED_BYTE)
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.UNSIGNED_BYTE

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromCharArray {

    val INSTANCE: FromCharArray = new FromCharArray()

  }

  class FromCharArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): StringValue =
      StringValue.makeStringValue(
        new String(`object`.asInstanceOf[Array[Char]]))

    def getItemType(): ItemType = BuiltInAtomicType.STRING

  }

  object FromDoubleArray {

    val INSTANCE: FromDoubleArray = new FromDoubleArray()

  }

  class FromDoubleArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Double]].length)
      for (i <- 0 until array.length) {
        array(i) = new DoubleValue(`object`.asInstanceOf[Array[Double]](i))
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.DOUBLE

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromFloatArray {

    val INSTANCE: FromFloatArray = new FromFloatArray()

  }

  class FromFloatArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Float]].length)
      for (i <- 0 until array.length) {
        array(i) = new DoubleValue(`object`.asInstanceOf[Array[Float]](i))
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.FLOAT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  object FromBooleanArray {

    val INSTANCE: FromBooleanArray = new FromBooleanArray()

  }

  class FromBooleanArray extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](`object`.asInstanceOf[Array[Boolean]].length)
      for (i <- 0 until array.length) {
        array(i) = BooleanValue.get(`object`.asInstanceOf[Array[Boolean]](i))
      }
      new SequenceExtent(array)
    }

    def getItemType(): ItemType = BuiltInAtomicType.BOOLEAN

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

  class FromObjectArray(var itemConverter: JPConverter)
    extends JPConverter {

    def convert(`object`: AnyRef, context: XPathContext): Sequence = {
      val arrayObject: Array[Any] = `object`.asInstanceOf[Array[Any]]
      val newArray: List[Item] = new ArrayList[Item](arrayObject.length)
      val a: Int = 0
      for (member <- arrayObject) {
        if (member != null) {
          val newItem: Item =
            SequenceTool.asItem(itemConverter.convert(member.asInstanceOf[AnyRef], context))
          if (newItem != null) {
            newArray.add(newItem)
          }
        } else {
          throw new XPathException(
            "Returned array contains null values: cannot convert to items",
            SaxonErrorCode.SXJE0051)
        }
      }
      new SequenceExtent(newArray)
    }

    def getItemType(): ItemType = itemConverter.getItemType

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  }

}

abstract class JPConverter {

  def convert(`object`: AnyRef, context: XPathContext): Sequence

  def getItemType(): ItemType

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

}
