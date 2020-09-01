package net.sf.saxon.expr

import java.lang.reflect.ParameterizedType
import java.math.{BigDecimal, BigInteger}
import java.net.{URI, URL}
import java.time._
import java.{lang => jl, util => ju}

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import net.sf.saxon.lib.ParseOptions
import net.sf.saxon.ma.arrays.{ArrayItem, ArrayItemType}
import net.sf.saxon.ma.map.{MapItem, MapType}
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.pattern.{AnyNodeTest, NodeKindTest}
import net.sf.saxon.s9api.{ItemType => _, _}
import net.sf.saxon.s9apir.XdmFunctionItem
import net.sf.saxon.trans.{SaxonErrorCode, XPathException}
import net.sf.saxon.utils.{Configuration, Controller}
import net.sf.saxon.value._

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

object JPConverter {

  var converterMap =
    Map[Class[_], JPConverter](
      classOf[XdmValue]              -> new FromXdmValue(AnyItemType,                  StaticProperty.ALLOWS_ZERO_OR_MORE),
      classOf[XdmItem]               -> new FromXdmValue(AnyItemType,                  StaticProperty.ALLOWS_ONE),
      classOf[XdmAtomicValue]        -> new FromXdmValue(BuiltInAtomicType.ANY_ATOMIC, StaticProperty.ALLOWS_ONE),
      classOf[XdmNode]               -> new FromXdmValue(AnyNodeTest.getInstance,      StaticProperty.ALLOWS_ONE),
      classOf[XdmFunctionItem]       -> new FromXdmValue(AnyFunctionType.getInstance,  StaticProperty.ALLOWS_ONE),
      classOf[XdmMap]                -> new FromXdmValue(MapType.ANY_MAP_TYPE,         StaticProperty.ALLOWS_ONE),
      classOf[XdmArray]              -> new FromXdmValue(ArrayItemType.ANY_ARRAY_TYPE, StaticProperty.ALLOWS_ONE),
      classOf[XdmEmptySequence]      -> new FromXdmValue(ErrorType,        StaticProperty.ALLOWS_ZERO),
      classOf[SequenceIterator]      -> FromSequenceIterator,
      classOf[Sequence]              -> FromSequenceOfAny,
      classOf[OneOrMore[_ <: Item]]  -> FromSequenceOfAny,
      classOf[One[_ <: Item]]        -> FromSequenceOfAny,
      classOf[ZeroOrOne[_ <: Item]]  -> FromSequenceOfAny,
      classOf[ZeroOrMore[_ <: Item]] -> FromSequenceOfAny,
      classOf[String]                -> FromString,
      classOf[jl.Boolean]            -> FromBoolean,
      classOf[Boolean]               -> FromBoolean,
      classOf[jl.Double]             -> FromDouble,
      classOf[Double]                -> FromDouble,
      classOf[jl.Float]              -> FromFloat,
      classOf[Float]                 -> FromFloat,
      classOf[BigDecimal]            -> FromBigDecimal,
      classOf[BigInteger]            -> FromBigInteger,
      classOf[jl.Long]               -> FromLong,
      classOf[Long]                  -> FromLong,
      classOf[jl.Integer]            -> FromInt,
      classOf[Int]                   -> FromInt,
      classOf[jl.Short]              -> FromShort,
      classOf[Short]                 -> FromShort,
      classOf[jl.Byte]               -> FromByte,
      classOf[Byte]                  -> FromByte,
      classOf[jl.Character]          -> FromCharacter,
      classOf[Char]                  -> FromCharacter,
      classOf[URI]                   -> FromURI,
      classOf[URL]                   -> FromURI,
      classOf[ju.Date]               -> FromDate,
      classOf[Instant]               -> FromInstant,
      classOf[LocalDateTime]         -> FromLocalDateTime,
      classOf[ZonedDateTime]         -> FromZonedDateTime,
      classOf[OffsetDateTime]        -> FromOffsetDateTime,
      classOf[LocalDate]             -> FromLocalDate,
      classOf[Array[Long]]           -> FromLongArray,
      classOf[Array[Int]]            -> FromIntArray,
      classOf[Array[Short]]          -> FromShortArray,
      classOf[Array[Byte]]           -> FromByteArray,
      classOf[Array[Char]]           -> FromCharArray,
      classOf[Array[Double]]         -> FromDoubleArray,
      classOf[Array[Float]]          -> FromFloatArray,
      classOf[Array[Boolean]]        -> FromBooleanArray,
      classOf[ju.Collection[_]]      -> FromCollection,
    )

  var itemTypeMap = Map[Class[_], ItemType](
    classOf[BooleanValue]            -> BuiltInAtomicType.BOOLEAN,
    classOf[StringValue]             -> BuiltInAtomicType.STRING,
    classOf[DoubleValue]             -> BuiltInAtomicType.DOUBLE,
    classOf[FloatValue]              -> BuiltInAtomicType.FLOAT,
    classOf[BigDecimalValue]         -> BuiltInAtomicType.DECIMAL,
    classOf[IntegerValue]            -> BuiltInAtomicType.INTEGER,
    classOf[DurationValue]           -> BuiltInAtomicType.DURATION,
    classOf[DayTimeDurationValue]    -> BuiltInAtomicType.DAY_TIME_DURATION,
    classOf[YearMonthDurationValue]  -> BuiltInAtomicType.YEAR_MONTH_DURATION,
    classOf[DateTimeValue]           -> BuiltInAtomicType.DATE_TIME,
    classOf[DateValue]               -> BuiltInAtomicType.DATE,
    classOf[TimeValue]               -> BuiltInAtomicType.TIME,
    classOf[GYearValue]              -> BuiltInAtomicType.G_YEAR,
    classOf[GYearMonthValue]         -> BuiltInAtomicType.G_YEAR_MONTH,
    classOf[GMonthValue]             -> BuiltInAtomicType.G_MONTH,
    classOf[GMonthDayValue]          -> BuiltInAtomicType.G_MONTH_DAY,
    classOf[GDayValue]               -> BuiltInAtomicType.G_DAY,
    classOf[AnyURIValue]             -> BuiltInAtomicType.ANY_URI,
    classOf[QNameValue]              -> BuiltInAtomicType.QNAME,
    classOf[NotationValue]           -> BuiltInAtomicType.NOTATION,
    classOf[HexBinaryValue]          -> BuiltInAtomicType.HEX_BINARY,
    classOf[Base64BinaryValue]       -> BuiltInAtomicType.BASE64_BINARY,
    classOf[NodeInfo]                -> AnyNodeTest.getInstance,
    classOf[TreeInfo]                -> NodeKindTest.DOCUMENT,
    classOf[MapItem]                 -> MapType.EMPTY_MAP_TYPE,
    classOf[ArrayItem]               -> ArrayItemType.ANY_ARRAY_TYPE,
    classOf[Function]                -> AnyFunctionType.getInstance,
    classOf[AtomicValue]             -> BuiltInAtomicType.ANY_ATOMIC,
    classOf[UntypedAtomicValue]      -> BuiltInAtomicType.UNTYPED_ATOMIC,
  )

  var cardinalityMap = Map[Class[_], Int](
    classOf[Sequence]                -> StaticProperty.ALLOWS_ZERO_OR_MORE,
    classOf[ZeroOrMore[_ <: Item]]   -> StaticProperty.ALLOWS_ZERO_OR_MORE,
    classOf[OneOrMore[_ <: Item]]    -> StaticProperty.ALLOWS_ONE_OR_MORE,
    classOf[One[_ <: Item]]          -> StaticProperty.EXACTLY_ONE,
    classOf[ZeroOrOne[_ <: Item]]    -> StaticProperty.ALLOWS_ZERO_OR_ONE,
    classOf[XdmValue]                -> StaticProperty.ALLOWS_ZERO_OR_MORE,
    classOf[XdmItem]                 -> StaticProperty.ALLOWS_ZERO_OR_MORE,
    classOf[XdmEmptySequence]        -> StaticProperty.ALLOWS_ZERO,
  )

  def allocate(javaClass: Class[_],
               genericType: java.lang.reflect.Type,
               config: Configuration): JPConverter = {

    if (classOf[javax.xml.namespace.QName].isAssignableFrom(javaClass))
      return FromQName

    if (classOf[Sequence].isAssignableFrom(javaClass)) {
      genericType match {
        case parameterizedType: ParameterizedType =>
          val params  = parameterizedType.getActualTypeArguments
          if (params.length == 1 && params(0).isInstanceOf[Class[_]] &&
            classOf[Item].isAssignableFrom(params(0).asInstanceOf[Class[_]])) {
            val itemType = itemTypeMap.get(params(0).asInstanceOf[Class[_]])
            val cardinality = cardinalityMap.get(javaClass)
            if (itemType.isDefined && cardinality.isDefined)
              return new FromSequence(itemType.get, cardinality.get)
          }
        case _ =>
          itemTypeMap.get(javaClass) foreach { itemType =>
            return new FromSequence(itemType, StaticProperty.ALLOWS_ZERO_OR_ONE)
          }
      }
    }

    converterMap.get(javaClass) foreach { c =>
      return c // NonLocalReturns
    }

    if (javaClass == classOf[AnyRef])
      return FromObject

    if (classOf[NodeInfo].isAssignableFrom(javaClass))
      return new FromSequence(AnyNodeTest.getInstance, StaticProperty.ALLOWS_ZERO_OR_ONE)

    if (classOf[Source].isAssignableFrom(javaClass) && !classOf[DOMSource].isAssignableFrom(javaClass))
      return FromSource

    for ((key, value) <- converterMap if key.isAssignableFrom(javaClass))
      return value

    for (model <- config.getExternalObjectModels.asScala) {
      val converter = model.getJPConverter(javaClass, config)
      if (converter ne null)
        return converter
    }

    if (javaClass.isArray)
      return new FromObjectArray(allocate(javaClass.getComponentType, null, config))

    if (javaClass == Void.TYPE)
      return VoidConverter

    new ExternalObjectWrapper(config.getJavaExternalObjectType(javaClass))
  }

  object FromObject extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val theClass = o.getClass
      var instanceConverter = allocate(theClass, null, context.getConfiguration)
      if (instanceConverter eq FromObject)
        instanceConverter = new ExternalObjectWrapper(context.getConfiguration.getJavaExternalObjectType(theClass))

      instanceConverter.convert(o, context)
    }

    def getItemType: ItemType = AnyItemType

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromSequenceIterator extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence =
      o.asInstanceOf[SequenceIterator].materialize()

    def getItemType: ItemType = AnyItemType

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  class FromXdmValue(var resultType: ItemType,
                     @BeanProperty var cardinalityInt: Int = 0)
    extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence =
      o.asInstanceOf[XdmValue].getUnderlyingValue

    def getItemType: ItemType = resultType
  }

  object FromSequenceOfAny extends FromSequence(AnyItemType, StaticProperty.ALLOWS_ZERO_OR_MORE)

  class FromSequence(var resultType: ItemType, @BeanProperty var cardinalityInt: Int) extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence =
      o match {
        case closure: Closure => closure.iterate().materialize()
        case _                => o.asInstanceOf[Sequence]
      }

    def getItemType: ItemType = resultType
  }

  object FromString extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): StringValue =
      new StringValue(o.asInstanceOf[String])

    def getItemType: ItemType = BuiltInAtomicType.STRING
  }

  object FromBoolean extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): BooleanValue =
      BooleanValue.get(o.asInstanceOf[java.lang.Boolean])

    def getItemType: ItemType = BuiltInAtomicType.BOOLEAN
  }

  object  FromDouble extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DoubleValue =
      new DoubleValue(o.asInstanceOf[java.lang.Double])

    def getItemType: ItemType = BuiltInAtomicType.DOUBLE
  }

  object FromFloat extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): FloatValue =
      new FloatValue(o.asInstanceOf[java.lang.Float])

    def getItemType: ItemType = BuiltInAtomicType.FLOAT
  }

  object FromBigDecimal extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): BigDecimalValue =
      new BigDecimalValue(o.asInstanceOf[BigDecimal])

    def getItemType: ItemType = BuiltInAtomicType.DECIMAL
  }

  object FromBigInteger extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): IntegerValue =
      IntegerValue.makeIntegerValue(o.asInstanceOf[BigInteger])

    def getItemType: ItemType = BuiltInAtomicType.INTEGER
  }

  object FromLong extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(o.asInstanceOf[java.lang.Long])

    def getItemType: ItemType = BuiltInAtomicType.INTEGER
  }

  object FromInt extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Int64Value = new Int64Value(o.asInstanceOf[Long])

    def getItemType: ItemType = BuiltInAtomicType.INTEGER
  }

  object FromShort extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(o.asInstanceOf[java.lang.Short].intValue())

    def getItemType: ItemType = BuiltInAtomicType.INTEGER
  }

  object FromByte extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Int64Value =
      new Int64Value(o.asInstanceOf[java.lang.Byte].intValue())

    def getItemType: ItemType = BuiltInAtomicType.INTEGER
  }

  object FromCharacter extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): StringValue =
      new StringValue(o.toString)

    def getItemType: ItemType = BuiltInAtomicType.STRING
  }


  object FromQName extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): QNameValue = {
      val qn: javax.xml.namespace.QName =
        o.asInstanceOf[javax.xml.namespace.QName]
      new QNameValue(qn.getPrefix, qn.getNamespaceURI, qn.getLocalPart)
    }

    def getItemType: ItemType = BuiltInAtomicType.QNAME
  }

  object FromURI extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): AnyURIValue =
      new AnyURIValue(o.toString)

    def getItemType: ItemType = BuiltInAtomicType.ANY_URI
  }

  object FromDate extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromJavaDate(o.asInstanceOf[ju.Date])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  object FromInstant extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromJavaInstant(o.asInstanceOf[Instant])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  object FromZonedDateTime extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromZonedDateTime(o.asInstanceOf[ZonedDateTime])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  object FromOffsetDateTime extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromOffsetDateTime(o.asInstanceOf[OffsetDateTime])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  object FromLocalDateTime extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateTimeValue =
      DateTimeValue.fromLocalDateTime(o.asInstanceOf[LocalDateTime])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  object FromLocalDate extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): DateValue =
      new DateValue(o.asInstanceOf[LocalDate])

    def getItemType: ItemType = BuiltInAtomicType.DATE_TIME
  }

  class ExternalObjectWrapper(var resultType: JavaExternalObjectType)
    extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): ExternalObject[Any] =
      if (o == null) {
        null
      } else if (resultType.getJavaClass.isInstance(o)) {
        new ObjectValue(o)
      } else {
        throw new XPathException(
          "Java external object of type " + o.getClass.getName +
            " is not an instance of the required type " +
            resultType.getJavaClass.getName,
          "XPTY0004")
      }

    def getItemType: JavaExternalObjectType = resultType

  }

  object VoidConverter extends JPConverter {
    def convert(o: AnyRef, context: XPathContext): EmptySequence[_ <: Item] = EmptySequence.getInstance
    def getItemType: ItemType = AnyItemType
  }

  object FromCollection extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val list = new ju.ArrayList[Item](o.asInstanceOf[ju.Collection[_]].size)
      for (obj <- o.asInstanceOf[ju.Collection[_]].asScala) {
        val itemConverter = allocate(obj.getClass, null, context.getConfiguration)
        val item = SequenceTool.asItem(itemConverter.convert(obj.asInstanceOf[AnyRef], context))
        if (item != null)
          list.add(item)
      }
      new SequenceExtent(list)
    }

    def getItemType: ItemType = AnyItemType

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromSource extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): NodeInfo = {
      val options: ParseOptions = new ParseOptions()
      val controller: Controller = context.getController
      if (controller != null) {
        options.setSchemaValidationMode(controller.getSchemaValidationMode)
      }
      context.getConfiguration
        .buildDocumentTree(o.asInstanceOf[Source], options)
        .getRootNode
    }

    def getItemType: ItemType = AnyNodeTest.getInstance
  }

  object FromLongArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Long]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          o.asInstanceOf[Array[Long]](i),
          BuiltInAtomicType.LONG)
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.LONG

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromIntArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Int]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(o.asInstanceOf[Array[Int]](i),
          BuiltInAtomicType.INT)
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.INT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromShortArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Short]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          o.asInstanceOf[Array[Short]](i),
          BuiltInAtomicType.SHORT)
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.SHORT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromByteArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Byte]].length)
      for (i <- 0 until array.length) {
        array(i) = Int64Value.makeDerived(
          255 & o.asInstanceOf[Array[Byte]](i).toInt,
          BuiltInAtomicType.UNSIGNED_BYTE)
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.UNSIGNED_BYTE

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromCharArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): StringValue =
      StringValue.makeStringValue(
        new String(o.asInstanceOf[Array[Char]]))

    def getItemType: ItemType = BuiltInAtomicType.STRING
  }

  object FromDoubleArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Double]].length)
      for (i <- 0 until array.length) {
        array(i) = new DoubleValue(o.asInstanceOf[Array[Double]](i))
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.DOUBLE

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromFloatArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array: Array[Item] =
        Array.ofDim[Item](o.asInstanceOf[Array[Float]].length)
      for (i <- 0 until array.length) {
        array(i) = new DoubleValue(o.asInstanceOf[Array[Float]](i))
      }
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.FLOAT

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  object FromBooleanArray extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val array = Array.ofDim[Item](o.asInstanceOf[Array[Boolean]].length)
      for (i <- array.indices)
        array(i) = BooleanValue.get(o.asInstanceOf[Array[Boolean]](i))
      new SequenceExtent(array)
    }

    def getItemType: ItemType = BuiltInAtomicType.BOOLEAN

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  class FromObjectArray(var itemConverter: JPConverter)
    extends JPConverter {

    def convert(o: AnyRef, context: XPathContext): Sequence = {
      val arrayObject: Array[Any] = o.asInstanceOf[Array[Any]]
      val newArray = new ju.ArrayList[Item](arrayObject.length)
      for (member <- arrayObject) {
        if (member != null) {
          val newItem = SequenceTool.asItem(itemConverter.convert(member.asInstanceOf[AnyRef], context))
          if (newItem != null)
            newArray.add(newItem)
        } else {
          throw new XPathException(
            "Returned array contains null values: cannot convert to items",
            SaxonErrorCode.SXJE0051
          )
        }
      }
      new SequenceExtent(newArray)
    }

    def getItemType: ItemType = itemConverter.getItemType

    override def getCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE
  }
}

abstract class JPConverter {
  def convert(o: AnyRef, context: XPathContext): Sequence
  def getItemType: ItemType
  def getCardinality: Int = StaticProperty.EXACTLY_ONE
}
