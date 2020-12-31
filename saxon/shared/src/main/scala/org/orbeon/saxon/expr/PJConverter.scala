package org.orbeon.saxon.expr

import java.lang.reflect.{Array, Constructor, ParameterizedType}
import java.math.{BigDecimal, BigInteger}
import java.net.{URI, URL}
import java.time._
import java.util.{ArrayList, Collection, HashMap, List}

import org.orbeon.saxon.lib.ExternalObjectModel
import org.orbeon.saxon.ma.map.{MapItem, MapType}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.trans.{SaxonErrorCode, XPathException}
import org.orbeon.saxon.tree.wrapper.VirtualNode
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object PJConverter {

  private val jpmap: HashMap[Class[_], SequenceType] = new HashMap

  jpmap.put(classOf[Boolean], SequenceType.SINGLE_BOOLEAN)
  jpmap.put(classOf[Boolean], SequenceType.OPTIONAL_BOOLEAN)
  jpmap.put(classOf[String], SequenceType.OPTIONAL_STRING)
  jpmap.put(classOf[CharSequence], SequenceType.OPTIONAL_STRING)
  jpmap.put(classOf[Long], SequenceType.SINGLE_INTEGER)
  jpmap.put(classOf[Long], SequenceType.OPTIONAL_INTEGER)
  jpmap.put(classOf[Int], SequenceType.SINGLE_INTEGER)
  jpmap.put(classOf[Integer], SequenceType.OPTIONAL_INTEGER)
  jpmap.put(classOf[Short], SequenceType.SINGLE_SHORT)
  jpmap.put(classOf[Short], SequenceType.OPTIONAL_SHORT)
  jpmap.put(classOf[Byte], SequenceType.SINGLE_BYTE)
  jpmap.put(classOf[Byte], SequenceType.OPTIONAL_BYTE)
  jpmap.put(classOf[Float], SequenceType.SINGLE_FLOAT)
  jpmap.put(classOf[Float], SequenceType.OPTIONAL_FLOAT)
  jpmap.put(classOf[Double], SequenceType.SINGLE_DOUBLE)
  jpmap.put(classOf[Double], SequenceType.OPTIONAL_DOUBLE)
  jpmap.put(classOf[URI], SequenceType.OPTIONAL_ANY_URI)
  jpmap.put(classOf[URL], SequenceType.OPTIONAL_ANY_URI)
  jpmap.put(classOf[BigInteger], SequenceType.OPTIONAL_INTEGER)
  jpmap.put(classOf[BigDecimal], SequenceType.OPTIONAL_DECIMAL)
  jpmap.put(classOf[StringValue], SequenceType.OPTIONAL_STRING)
  jpmap.put(classOf[BooleanValue], SequenceType.OPTIONAL_BOOLEAN)
  jpmap.put(classOf[DoubleValue], SequenceType.OPTIONAL_DOUBLE)
  jpmap.put(classOf[FloatValue], SequenceType.OPTIONAL_FLOAT)
  jpmap.put(classOf[DecimalValue], SequenceType.OPTIONAL_DECIMAL)
  jpmap.put(classOf[IntegerValue], SequenceType.OPTIONAL_INTEGER)
  jpmap.put(classOf[AnyURIValue], SequenceType.OPTIONAL_ANY_URI)
  jpmap.put(classOf[QNameValue], SequenceType.OPTIONAL_QNAME)
  jpmap.put(classOf[NotationValue], SequenceType.OPTIONAL_NOTATION)
  jpmap.put(classOf[DateValue], SequenceType.OPTIONAL_DATE)
  jpmap.put(classOf[DateTimeValue], SequenceType.OPTIONAL_DATE_TIME)
  jpmap.put(classOf[TimeValue], SequenceType.OPTIONAL_TIME)
  jpmap.put(classOf[DurationValue], SequenceType.OPTIONAL_DURATION)
  jpmap.put(classOf[DayTimeDurationValue], SequenceType.OPTIONAL_DAY_TIME_DURATION)
  jpmap.put(classOf[YearMonthDurationValue], SequenceType.OPTIONAL_YEAR_MONTH_DURATION)
  jpmap.put(classOf[GYearValue], SequenceType.OPTIONAL_G_YEAR)
  jpmap.put(classOf[GYearMonthValue], SequenceType.OPTIONAL_G_YEAR_MONTH)
  jpmap.put(classOf[GMonthValue], SequenceType.OPTIONAL_G_MONTH)
  jpmap.put(classOf[GMonthDayValue], SequenceType.OPTIONAL_G_MONTH_DAY)
  jpmap.put(classOf[GDayValue], SequenceType.OPTIONAL_G_DAY)
  jpmap.put(classOf[Base64BinaryValue], SequenceType.OPTIONAL_BASE64_BINARY)
  jpmap.put(classOf[HexBinaryValue], SequenceType.OPTIONAL_HEX_BINARY)
  jpmap.put(classOf[Function], SequenceType.OPTIONAL_FUNCTION_ITEM)
  jpmap.put(classOf[MapItem], MapType.OPTIONAL_MAP_ITEM)
  jpmap.put(classOf[NodeInfo], SequenceType.OPTIONAL_NODE)
  jpmap.put(classOf[TreeInfo], SequenceType.OPTIONAL_DOCUMENT_NODE)

  def getEquivalentSequenceType(javaClass: Class[_]): SequenceType = {
    if (javaClass.isArray) {
      val memberClass: Class[_] = javaClass.getComponentType
      if (memberClass == classOf[Byte]) {
        return SequenceType.makeSequenceType(BuiltInAtomicType.UNSIGNED_BYTE,
          StaticProperty.ALLOWS_ZERO_OR_MORE)
      } else {
        val memberType: SequenceType = getEquivalentSequenceType(memberClass)
        if (memberType != null) {
         return SequenceType.makeSequenceType(memberType.getPrimaryType,
            StaticProperty.ALLOWS_ZERO_OR_MORE)
        }
      }
    }
    jpmap.get(javaClass)
  }

  def getParameterizedSequenceType(
                                    javaType: java.lang.reflect.Type): SequenceType = {
    if (javaType.isInstanceOf[ParameterizedType]) {
      val aType: ParameterizedType = javaType.asInstanceOf[ParameterizedType]
      val parameterArgTypes: scala.Array[java.lang.reflect.Type] =
        aType.getActualTypeArguments
      if (parameterArgTypes.length == 1 && parameterArgTypes(0)
        .isInstanceOf[Class[_]] &&
        classOf[Item].isAssignableFrom(
          parameterArgTypes(0).asInstanceOf[Class[_]])) {
        val memberType: SequenceType = getEquivalentSequenceType(
          parameterArgTypes(0).asInstanceOf[Class[_]])
        val itemType: ItemType =
          if (memberType == null) null else memberType.getPrimaryType
        val collectionType: java.lang.reflect.Type = aType.getRawType
        var cardinality: Int = -1
        if (collectionType == classOf[ZeroOrOne[_ <: Item]]) {
          cardinality = StaticProperty.ALLOWS_ZERO_OR_ONE
        } else if (collectionType == classOf[One[_ <: Item]]) {
          cardinality = StaticProperty.ALLOWS_ONE
        } else if (collectionType == classOf[OneOrMore[_ <: Item]]) {
          cardinality = StaticProperty.ALLOWS_ONE_OR_MORE
        } else if (collectionType == classOf[ZeroOrMore[_ <: Item]]) {
          cardinality = StaticProperty.ALLOWS_ZERO_OR_MORE
        }
        if (itemType != null && cardinality != -1) {
          return SequenceType.makeSequenceType(itemType, cardinality)
        }
      }
    }
    null
  }

  def allocate(config: Configuration,
               itemType: ItemType,
               cardinality: Int,
               targetClass: Class[_]): PJConverter = {
    val th = config.getTypeHierarchy
    if (targetClass == classOf[SequenceIterator]) {
      return ToSequenceIterator
    }
    if (targetClass == classOf[Sequence] || targetClass == classOf[Item]) {
      return Identity
    }
    if (targetClass == classOf[One[_ <: Item]]) {
      return ToOne
    }
    if (targetClass == classOf[ZeroOrOne[_ <: Item]]) {
      return ToZeroOrOne
    }
    if (targetClass == classOf[OneOrMore[_ <: Item]]) {
      return ToOneOrMore
    }
    if (targetClass == classOf[ZeroOrMore[_ <: Item]]) {
      return ToZeroOrMore
    }
    if (targetClass == classOf[GroundedValue] | targetClass == classOf[
      SequenceExtent]) {
      return ToSequenceExtent
    }
    if (!itemType.isPlainType) {
      val externalObjectModels: List[ExternalObjectModel] =
        config.getExternalObjectModels
      for (model <- externalObjectModels.asScala) {
        val converter: PJConverter = model.getPJConverter(targetClass)
        if (converter != null) {
          return converter
        }
      }
      if (classOf[NodeInfo].isAssignableFrom(targetClass)) {
        return Identity
      }
    }
    if (classOf[Collection[_]].isAssignableFrom(targetClass)) {
      return ToCollection
    }
    if (targetClass.isArray) {
      val itemConverter = allocate(config,
        itemType,
        StaticProperty.EXACTLY_ONE,
        targetClass.getComponentType)
      return new ToArray(itemConverter)
    }
    if (!Cardinality.allowsMany(cardinality)) {
      if (itemType.isPlainType) {
        if (itemType == ErrorType) {
          StringValueToString
        } else if (th.isSubType(itemType, BuiltInAtomicType.STRING)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[String] || targetClass == classOf[CharSequence]) {
            StringValueToString
          } else if (targetClass.isAssignableFrom(classOf[StringValue])) {
            Identity
          } else if (targetClass == classOf[Char] || targetClass == classOf[Character]) {
            StringValueToChar
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (itemType == BuiltInAtomicType.UNTYPED_ATOMIC) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[String] || targetClass == classOf[CharSequence]) {
            StringValueToString
          } else if (targetClass.isAssignableFrom(classOf[UntypedAtomicValue])) {
            Identity
          } else {
            val constructor: Constructor[_] = targetClass.getConstructor(classOf[String])
            new PJConverter() {
              def convert(value: Sequence,
                          targetClass: Class[_],
                          context: XPathContext): Any =
                constructor.newInstance(value.head.getStringValue)
            }
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.BOOLEAN)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[Boolean] || targetClass == classOf[Boolean]) {
            BooleanValueToBoolean
          } else if (targetClass.isAssignableFrom(classOf[BooleanValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.INTEGER)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[BigInteger]) {
            IntegerValueToBigInteger
          } else if (targetClass == classOf[Long] || targetClass == classOf[Long]) {
            IntegerValueToLong
          } else if (targetClass == classOf[Int] || targetClass == classOf[Integer]) {
            IntegerValueToInt
          } else if (targetClass == classOf[Short] || targetClass == classOf[Short]) {
            IntegerValueToShort} else if (targetClass == classOf[Byte] || targetClass == classOf[Byte]) {
            IntegerValueToByte
          } else if (targetClass == classOf[Char] || targetClass == classOf[Character]) {
            IntegerValueToChar
          } else if (targetClass == classOf[Double] || targetClass == classOf[
            Double]) {
            NumericValueToDouble
          } else if (targetClass == classOf[Float] || targetClass == classOf[
            Float]) {
            NumericValueToFloat
          } else if (targetClass == classOf[BigDecimal]) {
            NumericValueToBigDecimal
          } else if (targetClass.isAssignableFrom(classOf[IntegerValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.DECIMAL)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[BigDecimal]) {
            NumericValueToBigDecimal
          } else if (targetClass == classOf[Double] || targetClass == classOf[Double]) {
            NumericValueToDouble
          } else if (targetClass == classOf[Float] || targetClass == classOf[Float]) {
            NumericValueToFloat
          } else if (targetClass.isAssignableFrom(classOf[BigDecimalValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.FLOAT)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[Float] ||
            targetClass == classOf[Float]) {
            NumericValueToFloat
          } else if (targetClass == classOf[Double] || targetClass == classOf[Double]) {
            NumericValueToDouble
          } else if (targetClass.isAssignableFrom(classOf[FloatValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.DOUBLE)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[Double] || targetClass == classOf[Double]) {
            NumericValueToDouble
          } else if (targetClass.isAssignableFrom(classOf[DoubleValue])) {
            Identity
          } else {
            Atomic
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.ANY_URI)) {
          if (targetClass == classOf[AnyRef] || classOf[URI].isAssignableFrom(targetClass)) {
            AnyURIValueToURI
          } else if (classOf[URL].isAssignableFrom(targetClass)) {
            AnyURIValueToURL
          } else if (targetClass == classOf[String] || targetClass == classOf[CharSequence]) {
            StringValueToString
          } else if (targetClass.isAssignableFrom(classOf[AnyURIValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.QNAME)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[javax.xml.namespace.QName]) {
            QualifiedNameValueToQName
          } else if (targetClass.isAssignableFrom(classOf[QNameValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.NOTATION)) {
          if (targetClass == classOf[AnyRef] || targetClass == classOf[javax.xml.namespace.QName]) {
            QualifiedNameValueToQName
          } else if (targetClass.isAssignableFrom(classOf[NotationValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.DURATION)) {
          if (targetClass.isAssignableFrom(classOf[DurationValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.DATE_TIME)) {
          if (targetClass.isAssignableFrom(classOf[DateTimeValue])) {
            Identity
          } else if (targetClass == classOf[java.util.Date]) {
            CalendarValueToDate
          } else if (targetClass == classOf[java.util.Calendar]) {
            CalendarValueToCalendar
          } else if (targetClass == classOf[Instant]) {
            CalendarValueToInstant
          } else if (targetClass == classOf[ZonedDateTime]) {
            CalendarValueToZonedDateTime
          } else if (targetClass == classOf[LocalDateTime]) {
            CalendarValueToLocalDateTime
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.DATE)) {
          if (targetClass.isAssignableFrom(classOf[DateValue])) {
            Identity
          } else if (targetClass == classOf[java.util.Date]) {
            CalendarValueToDate
          } else if (targetClass == classOf[java.util.Calendar]) {
            CalendarValueToCalendar
          } else if (targetClass == classOf[Instant]) {
            CalendarValueToInstant
          } else if (targetClass == classOf[ZonedDateTime]) {
            CalendarValueToZonedDateTime
          } else if (targetClass == classOf[OffsetDateTime]) {
            CalendarValueToOffsetDateTime
          } else if (targetClass == classOf[LocalDateTime]) {
            CalendarValueToLocalDateTime
          } else if (targetClass == classOf[LocalDate]) {
            DateValueToLocalDate
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.TIME)) {
          if (targetClass.isAssignableFrom(classOf[TimeValue])) {
            Identity
          } else if (targetClass == classOf[java.util.Date]) {
            CalendarValueToDate
          } else if (targetClass == classOf[java.util.Calendar]) {
            CalendarValueToCalendar
          } else if (targetClass == classOf[Instant]) {
            CalendarValueToInstant
          } else if (targetClass == classOf[ZonedDateTime]) {
            CalendarValueToZonedDateTime
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.G_YEAR)) {
          if (targetClass.isAssignableFrom(classOf[GYearValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.G_YEAR_MONTH)) {
          if (targetClass.isAssignableFrom(classOf[GYearMonthValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.G_MONTH)) {
          if (targetClass.isAssignableFrom(classOf[GMonthValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.G_MONTH_DAY)) {
          if (targetClass.isAssignableFrom(classOf[GMonthDayValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.G_DAY)) {
          if (targetClass.isAssignableFrom(classOf[GDayValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.BASE64_BINARY)) {
          if (targetClass.isAssignableFrom(classOf[Base64BinaryValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else if (th.isSubType(itemType, BuiltInAtomicType.HEX_BINARY)) {
          if (targetClass.isAssignableFrom(classOf[HexBinaryValue])) {
            Identity
          } else {
            throw cannotConvert(itemType, targetClass, config)
          }
        } else {
          Atomic
        }
      } else if (itemType.isInstanceOf[JavaExternalObjectType]) {
        UnwrapExternalObject
      } else if (itemType eq ErrorType) {
        ToNull
      } else if (itemType.isInstanceOf[NodeTest]) {
        if (classOf[NodeInfo].isAssignableFrom(targetClass)) {
          Identity
        } else {
          General
        }
      } else {
        General
      }
    } else {
      General
    }
  }

  private def cannotConvert(source: ItemType,
                            target: Class[_],
                            config: Configuration): XPathException =
    new XPathException("Cannot convert from " + source + " to " + target.getName)

  def allocateNodeListCreator(config: Configuration,
                              node: AnyRef): PJConverter = {
    val externalObjectModels = config.getExternalObjectModels
    for (model <- externalObjectModels.asScala) {
      val converter = model.getNodeListCreator(node)
      if (converter != null) {
        return converter
      }
    }
    ToCollection
  }

  object ToSequenceIterator extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = value.iterate()

  }

  object ToNull extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = null

  }

  object ToSequenceExtent extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = value.iterate().materialize

  }

  object ToCollection extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      var list: Collection[Any] = null
      if (targetClass.isAssignableFrom(classOf[ArrayList[_]])) {
        list = new ArrayList(100)
      } else {
        try
          list = targetClass.newInstance().asInstanceOf[Collection[Any]]
        catch {
          case e =>
            val de: XPathException = new XPathException(
              "Cannot instantiate collection class " + targetClass)
            de.setXPathContext(context)
            throw de
          case _: IllegalAccessException =>
            val de: XPathException = new XPathException(
              "Cannot access collection class " + targetClass)
            de.setXPathContext(context)
            throw de
        }
      }
      val config: Configuration = context.getConfiguration
      val iter: SequenceIterator = value.iterate()
      var it: Item = null
      while ({
        it = iter.next()
        it
      } != null) if (it.isInstanceOf[AtomicValue]) {
        val pj = allocate(
          config,
          it.asInstanceOf[AtomicValue].getItemType,
          StaticProperty.EXACTLY_ONE,
          classOf[AnyRef])
        list.add(pj.convert(it, classOf[AnyRef], context))
      } else if (it.isInstanceOf[VirtualNode]) {
        list.add(it.asInstanceOf[VirtualNode].getRealNode)
      } else {
        list.add(it)
      }
      list
    }

  }

  class ToArray(private var itemConverter: PJConverter) extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      if (value.isInstanceOf[ExternalObject[_]] &&
        targetClass.isAssignableFrom(
          value.asInstanceOf[ExternalObject[_]].getObject.getClass)) {
        value.asInstanceOf[ExternalObject[_]].getObject // Erick need to check
      }
      val componentClass: Class[_] = targetClass.getComponentType
      val list = new ArrayList[Any](20)
      val iter = value.iterate()
      var item: Item = null
      while ({
        item = iter.next()
        item
      } != null) {
        val obj: Any = itemConverter.convert(item, componentClass, context)
        if (obj != null) {
          list.add(obj)
        }
      }
      val array: AnyRef = Array.newInstance(componentClass, list.size)
      for (i <- 0 until list.size) {
        Array.set(array, i, list.get(i))
      }
      array
    }

  }

  object ToOne extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): One[_ <: Item] = new One(value.head)

  }

  object ToZeroOrOne extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): ZeroOrOne[_ <: Item] = new ZeroOrOne(value.head)

  }

  object ToOneOrMore extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): OneOrMore[Item] =
      OneOrMore.makeOneOrMore(value)

  }

  object ToZeroOrMore extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): ZeroOrMore[Item] =
      new ZeroOrMore(value.iterate())

  }

  object Identity extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      var seqVal: Sequence = value
      if (seqVal.isInstanceOf[Closure]) {
        seqVal = seqVal.asInstanceOf[Closure].reduce()
      }
      if (seqVal.isInstanceOf[ZeroOrOne[_ <: Item]]) {
        seqVal = seqVal.asInstanceOf[ZeroOrOne[_ <: Item]].head
      }
      if (seqVal.isInstanceOf[VirtualNode]) {
        val obj: AnyRef = seqVal.asInstanceOf[VirtualNode].getRealNode
        if (targetClass.isAssignableFrom(obj.getClass)) {
          return obj
        }
      }
      if (targetClass.isAssignableFrom(seqVal.getClass)) {
        seqVal
      } else {
        var gv: GroundedValue = seqVal.materialize
        if (targetClass.isAssignableFrom(gv.getClass)) {
          return gv
        }
        gv = gv.reduce()
        if (targetClass.isAssignableFrom(gv.getClass)) {
          return gv
        }
        if (gv.getLength == 0) {
          null
        } else {
          throw new XPathException(
            "Cannot convert value " + seqVal.getClass + " of type " +
              SequenceTool.getItemType(
                seqVal,
                context.getConfiguration.getTypeHierarchy) +
              " to class " +
              targetClass.getName)
        }
      }
    }

  }

  object UnwrapExternalObject extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      var head: Item = value.head
      if (head == null) {
        return null
      }
      if (! head.isInstanceOf[ExternalObject[_]]) {
        if (classOf[Sequence].isAssignableFrom(targetClass)) {
          head = new ObjectValue(value)
        } else {
          throw new XPathException(
            "Expected external object of class " + targetClass.getName +
              ", got " +
              head.getClass)
        }
      }
      val obj: Any = head.asInstanceOf[ExternalObject[_]].getObject
      if (!targetClass.isAssignableFrom(obj.getClass)) {
        throw new XPathException(
          "External object has wrong class (is " + obj.getClass.getName +
            ", expected " +
            targetClass.getName +
            ")")
      }
      obj
    }

  }

  object StringValueToString extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): String = {
      val first: Item = value.head
      if (first == null) null else first.getStringValue
    }

  }

  object StringValueToChar extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val first: Item = value.head
      if (first == null) {
        return null
      }
      val str: String = first.getStringValue
      if (str.length == 1) {
        str.charAt(0)
      } else {
        val de: XPathException = new XPathException(
          "Cannot convert xs:string to Java char unless length is 1")
        de.setXPathContext(context)
        de.setErrorCode(SaxonErrorCode.SXJE0005)
        throw de
      }
    }

  }

  object BooleanValueToBoolean extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val bv: BooleanValue = value.head.asInstanceOf[BooleanValue]
      assert(bv != null)
      bv.getBooleanValue
    }

  }

  object IntegerValueToBigInteger extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val `val`: IntegerValue = value.head.asInstanceOf[IntegerValue]
      if (`val` == null) null else `val`.asBigInteger()
    }

  }

  object IntegerValueToLong extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val iv: IntegerValue = value.head.asInstanceOf[IntegerValue]
      assert(iv != null)
      iv.longValue
    }

  }

  object IntegerValueToInt extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val iv: IntegerValue = value.head.asInstanceOf[IntegerValue]
      assert(iv != null)
      iv.longValue.toInt
    }

  }

  object IntegerValueToShort extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val iv: IntegerValue = value.head.asInstanceOf[IntegerValue]
      assert(iv != null)
      iv.longValue.toShort
    }

  }

  object IntegerValueToByte extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val iv: IntegerValue = value.head.asInstanceOf[IntegerValue]
      assert(iv != null)
      iv.longValue.toByte
    }

  }

  object IntegerValueToChar extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val iv: IntegerValue = value.head.asInstanceOf[IntegerValue]
      assert(iv != null)
      iv.longValue.toChar
    }

  }

  object NumericValueToBigDecimal extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val nv: NumericValue = value.head.asInstanceOf[NumericValue]
      if (nv == null) null else nv.getDecimalValue
    }

  }

  object NumericValueToDouble extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val nv: NumericValue = value.head.asInstanceOf[NumericValue]
      assert(nv != null)
      nv.getDoubleValue
    }

  }

  object NumericValueToFloat extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val nv: NumericValue = value.head.asInstanceOf[NumericValue]
      assert(nv != null)
      nv.getFloatValue
    }

  }

  object AnyURIValueToURI extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val av: AnyURIValue = value.head.asInstanceOf[AnyURIValue]
      if (av == null) null
      else new URI(value.asInstanceOf[AnyURIValue].getStringValue)
    }

  }

  object AnyURIValueToURL extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val av: AnyURIValue = value.head.asInstanceOf[AnyURIValue]
      if (av == null) null
      else new URL(value.asInstanceOf[AnyURIValue].getStringValue)
    }

  }

  object QualifiedNameValueToQName extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val qv: QualifiedNameValue =
        value.head.asInstanceOf[QualifiedNameValue]
      if (qv == null) null else qv.toJaxpQName
    }

  }

  object CalendarValueToInstant extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val cv: CalendarValue = value.head.asInstanceOf[CalendarValue]
      if (cv == null) null else cv.toDateTime.toJavaInstant
    }

  }

  object CalendarValueToZonedDateTime extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val cv: CalendarValue = value.head.asInstanceOf[CalendarValue]
      if (cv == null) null else cv.toDateTime.toZonedDateTime
    }

  }

  object CalendarValueToOffsetDateTime extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val cv: CalendarValue = value.head.asInstanceOf[CalendarValue]
      if (cv == null) null else cv.toDateTime.toOffsetDateTime
    }

  }

  object CalendarValueToLocalDateTime extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val cv: CalendarValue = value.head.asInstanceOf[CalendarValue]
      if (cv == null) null else cv.toDateTime.toLocalDateTime
    }

  }

  object CalendarValueToDate extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      // ORBEON: GregorianCalendar
      ???
//      val cv = value.head.asInstanceOf[CalendarValue]
//      if (cv == null) null else cv.getCalendar.getTime
    }
  }

  object DateValueToLocalDate extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      val cv: DateValue = value.head.asInstanceOf[DateValue]
      if (cv == null) null else cv.toLocalDate
    }
  }

  object CalendarValueToCalendar extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): AnyRef = {
      // ORBEON: GregorianCalendar
      ???
//      val cv = value.head.asInstanceOf[CalendarValue]
//      if (cv == null) null else cv.getCalendar
    }
  }

  object Atomic extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val item: AtomicValue = value.head.asInstanceOf[AtomicValue]
      if (item == null) {
        return null
      }
      val config: Configuration = context.getConfiguration
      val converter: PJConverter = allocate(config,
        item.getItemType,
        StaticProperty.EXACTLY_ONE,
        targetClass)
      converter.convert(item, targetClass, context)
    }
  }

  object General extends PJConverter {

    def convert(value: Sequence,
                targetClass: Class[_],
                context: XPathContext): Any = {
      val config: Configuration = context.getConfiguration
      val gv: GroundedValue = value.materialize
      var converter: PJConverter = allocate(
        config,
        SequenceTool.getItemType(gv, config.getTypeHierarchy),
        SequenceTool.getCardinality(gv),
        targetClass)
      if (converter.isInstanceOf[General.type]) {
        converter = Identity
      }
      converter.convert(gv, targetClass, context)
    }
  }
}

abstract class PJConverter {
  def convert(value: Sequence, targetClass: Class[_], context: XPathContext): Any
}
