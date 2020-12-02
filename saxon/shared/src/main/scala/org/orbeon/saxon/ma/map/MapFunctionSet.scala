package org.orbeon.saxon.ma.map

import java.util.{ArrayList, List, Map}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import org.orbeon.saxon.functions.SystemFunction._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet
import org.orbeon.saxon.functions.{InsertBefore, OptionsParameter, SystemFunction}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.ma.arrays.{ArrayItem, ArrayItemType, SimpleArrayItem}
import org.orbeon.saxon.ma.map.MapFunctionSet._
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.value._

//import scala.collection.compat._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet._

import scala.jdk.CollectionConverters._

object MapFunctionSet {

  val getInstance: MapFunctionSet = new MapFunctionSet()

  class MapContains extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
      val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
      val key: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
      BooleanValue.get(map.get(key) != null)
    }

  }

  class MapGet extends SystemFunction {

    var pendingWarning: String = null

    override def supplyTypeInformation(visitor: ExpressionVisitor,
                                       contextItemType: ContextItemStaticInfo,
                                       arguments: Array[Expression]): Unit = {
      val it = arguments(0).getItemType
      it match {
        case tupleType: TupleType =>
          arguments(1) match {
            case literal: Literal =>
              val key = literal
                .getValue
                .getStringValue
              if (tupleType.getFieldType(key) == null) {
                val xe = new XPathException(
                  "Field " + key + " is not defined for tuple type " + it,
                  "SXTT0001")
                xe.setIsTypeError(true)
                throw xe
              }
            case _ =>
          }
          val th = visitor.getConfiguration.getTypeHierarchy
          val relation = th.relationship(arguments(1).getItemType, BuiltInAtomicType.STRING)
          if (relation == Affinity.DISJOINT) {
            val xe = new XPathException(
              "Key for tuple type must be a string (actual type is " +
                arguments(1).getItemType,
              "XPTY0004")
            xe.setIsTypeError(true)
            throw xe
          }
        case _ =>
      }
    }

    override def getResultItemType(args: Array[Expression]): ItemType = {
      val itemType = args(0).getItemType
      itemType match {
        case tupleItemType: TupleItemType if args(1).isInstanceOf[StringLiteral] =>
          val key = args(1).asInstanceOf[StringLiteral].getStringValue
          val valueType: SequenceType = tupleItemType.getFieldType(key)
          if (valueType == null) {
            warning("Field " + key + " is not defined in tuple type")
            AnyItemType
          } else {
            valueType.getPrimaryType
          }
        case mapType: MapType =>
          mapType.getValueType.getPrimaryType
        case _ =>
          super.getResultItemType(args)
      }
    }

    override def getCardinality(args: Array[Expression]): Int = {
      val itemType = args(0).getItemType
      itemType match {
        case tupleItemType: TupleItemType if args(1).isInstanceOf[StringLiteral] =>
          val key = args(1).asInstanceOf[StringLiteral].getStringValue
          val valueType: SequenceType = tupleItemType.getFieldType(key)
          if (valueType == null) {
            warning("Field " + key + " is not defined in tuple type")
            StaticProperty.ALLOWS_MANY
          } else {
            valueType.getCardinality
          }
        case mapType1: MapType =>
          Cardinality.union(
            mapType1.getValueType.getCardinality,
            StaticProperty.ALLOWS_ZERO)
        case _ =>
          super.getCardinality(args)
      }
    }

    override def makeOptimizedFunctionCall(
                                            visitor: ExpressionVisitor,
                                            contextInfo: ContextItemStaticInfo,
                                            arguments: Expression*): Expression = {
      if (pendingWarning != null && pendingWarning.!=("DONE")) {
        visitor.issueWarning(pendingWarning, arguments(0).getLocation)
        pendingWarning = "DONE"
      }
      null
    }

    private def warning(message: String): Unit =
      if ("DONE" != pendingWarning)
        pendingWarning = message

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
      assert(map != null)
      val key: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
      val value: Sequence = map.get(key)
      if (value == null) {
        EmptySequence.getInstance
      } else {
        value
      }
    }
  }

  class MapFind extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): ArrayItem = {
      val result = new ArrayList[GroundedValue]()
      val key = arguments(1).head.asInstanceOf[AtomicValue]
      processSequence(arguments(0), key, result)
      new SimpleArrayItem(result)
    }

    private def processSequence(in: Sequence,
                                key: AtomicValue,
                                result: List[GroundedValue]): Unit = {
      in.iterate().forEachOrFail {
        case arrayItem: ArrayItem =>
          for (sequence <- arrayItem.members)
            processSequence(sequence, key, result)
        case mapItem: MapItem =>
          val value = mapItem.get(key)
          if (value != null)
            result.add(value)
          for (entry <- mapItem.keyValuePairs.asScala)
            processSequence(entry.value, key, result)
        case _ =>
      }
    }
  }

  class MapEntry extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val key: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
      assert(key != null)
      val value: GroundedValue =
        arguments(1).iterate().materialize
      new SingleEntryMap(key, value)
    }

    override def getResultItemType(args: Array[Expression]): ItemType = {
      val ku: PlainType = args(0).getItemType.getAtomizedItemType
      var ka: AtomicType = null
      ka =
        ku match {
          case atomicType: AtomicType => atomicType
          case _ => ku.getPrimitiveItemType
        }
      new MapType(ka,
        SequenceType.makeSequenceType(args(1).getItemType,
          args(1).getCardinality))
    }

    override def getStreamerName: String = "MapEntry"

  }

  class MapForEach extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val map = arguments(0).head.asInstanceOf[MapItem]
      val fn = arguments(1).head.asInstanceOf[Function]
      val results = new ArrayList[GroundedValue]()
      for (pair <- map.keyValuePairs.asScala) {
        val seq = dynamicCall(fn, context, Array(pair.key, pair.value))
        results.add(seq.materialize)
      }
      new Chain(results)
    }

  }

  class MapKeys extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
      val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
      assert(map != null)
      SequenceTool.toLazySequence(map.keys)
    }

  }

  object MapMerge {

    val finalKey: String = "Q{" + NamespaceConstant.SAXON + "}final"

    val keyTypeKey: String = "Q{" + NamespaceConstant.SAXON + "}key-type"

    val onDuplicatesKey: String = "Q{" + NamespaceConstant.SAXON + "}on-duplicates"

    val errorCodeKey: String = "Q{" + NamespaceConstant.SAXON + "}duplicates-error-code"

  }

  class MapMerge extends SystemFunction {

    private var duplicates: String = "use-first"

    private var duplicatesErrorCode: String = "FOJS0003"

    private var onDuplicates: Function = null

    private var allStringKeys: Boolean = false

    private var treatAsFinal: Boolean = false

    override def makeOptimizedFunctionCall(
                                            visitor: ExpressionVisitor,
                                            contextInfo: ContextItemStaticInfo,
                                            arguments: Expression*): Expression = {
      if (arguments.length == 2 && arguments(1).isInstanceOf[Literal]) {
        val options: MapItem = arguments(1)
          .asInstanceOf[Literal]
          .getValue
          .head
          .asInstanceOf[MapItem]
        val values: Map[String, Sequence] =
          getDetails.optionDetails.processSuppliedOptions(
            options,
            visitor.getStaticContext.makeEarlyEvaluationContext())
        var duplicates: String =
          values.get("duplicates").asInstanceOf[StringValue].getStringValue
        val duplicatesErrorCode: String =
          values.get(MapMerge.errorCodeKey).asInstanceOf[StringValue].getStringValue
        val onDuplicates: Function =
          values.get(MapMerge.onDuplicatesKey).asInstanceOf[Function]
        if (onDuplicates != null) {
          duplicates = "use-callback"
        }
        val isFinal: Boolean =
          values.get(MapMerge.finalKey).asInstanceOf[BooleanValue].getBooleanValue
        val keyType: String =
          values.get(MapMerge.keyTypeKey).asInstanceOf[StringValue].getStringValue
        val mm2: MapMerge = MapFunctionSet.getInstance
          .makeFunction("merge", 1)
          .asInstanceOf[MapMerge]
        mm2.duplicates = duplicates
        mm2.duplicatesErrorCode = duplicatesErrorCode
        mm2.onDuplicates = onDuplicates
        mm2.allStringKeys = keyType.==("string")
        mm2.treatAsFinal = isFinal
        mm2.makeFunctionCall(arguments(0))
      }
      super.makeOptimizedFunctionCall(visitor, contextInfo, arguments: _*)
    }

    override def getResultItemType(args: Array[Expression]): ItemType = {
      val it: ItemType = args(0).getItemType
      if (it == ErrorType) {
        MapType.EMPTY_MAP_TYPE
      } else it match {
        case mapType: MapType =>
          var maybeCombined: Boolean = true
          if (args.length == 1) {
            maybeCombined = false
          } else args(1) match {
            case literal: Literal =>
              val options = literal.getValue.head.asInstanceOf[MapItem]
              val dupes = options.get(new StringValue("duplicates"))
              try
                if (dupes != null && "combine" != dupes.getStringValue)
                  maybeCombined = false
              catch {
                case _: XPathException =>
              }
            case _ =>
          }
          if (maybeCombined) {
            new MapType(
              mapType.getKeyType,
              SequenceType.makeSequenceType(
                mapType.getValueType.getPrimaryType,
                StaticProperty.ALLOWS_ZERO_OR_MORE)
            )
          } else {
            it
          }
        case _ =>
          super.getResultItemType(args)
      }
    }

    import MapMerge._

    def call(context: XPathContext, arguments: Array[Sequence]): MapItem = {
      var duplicates: String = this.duplicates
      var duplicatesErrorCode: String = this.duplicatesErrorCode
      var allStringKeys: Boolean = this.allStringKeys
      var treatAsFinal: Boolean = this.treatAsFinal
      var onDuplicates: Function = this.onDuplicates
      if (arguments.length > 1) {
        val options: MapItem = arguments(1).head.asInstanceOf[MapItem]
        val values: Map[String, Sequence] =
          getDetails.optionDetails.processSuppliedOptions(options, context)
        duplicates =
          values.get("duplicates").asInstanceOf[StringValue].getStringValue
        duplicatesErrorCode =
          values.get(errorCodeKey).asInstanceOf[StringValue].getStringValue
        treatAsFinal =
          values.get(finalKey).asInstanceOf[BooleanValue].getBooleanValue
        allStringKeys = "string" == values
          .get(keyTypeKey)
          .asInstanceOf[StringValue]
          .getStringValue
        onDuplicates = values.get(onDuplicatesKey).asInstanceOf[Function]
        if (onDuplicates != null) {
          duplicates = "use-callback"
        }
      }
      if (treatAsFinal && allStringKeys) {
        val iter: SequenceIterator = arguments(0).iterate()
        val baseMap: DictionaryMap = new DictionaryMap()
        var next: MapItem = null
        duplicates match {
          case "unspecified" | "use-any" | "use-last" =>
            while (({
              next = iter.next().asInstanceOf[MapItem]
              next
            }) != null) for (pair <- next
              .keyValuePairs.asScala) {
              if (!(pair.key.isInstanceOf[StringValue])) {
                throw new XPathException(
                  "The keys in this map must all be strings (found " + pair.key.getItemType +
                    ")")
              }
              baseMap.initialPut(pair.key.getStringValue, pair.value)
            }
            null
          case _ =>
            while (({
              next = iter.next().asInstanceOf[MapItem]
              next
            }) != null) for (pair <- next
              .keyValuePairs.asScala) {
              if (!(pair.key.isInstanceOf[StringValue])) {
                throw new XPathException(
                  "The keys in this map must all be strings (found " + pair.key.getItemType +
                    ")")
              }
              val existing: Sequence = baseMap.get(pair.key)
              if (existing != null) {
                duplicates match {
                  case "use-first" | "unspecified" | "use-any" =>
                  case "use-last" =>
                    baseMap.initialPut(pair.key.getStringValue, pair.value)
                  case "combine" =>
                    var combinedIter: InsertBefore.InsertIterator =
                      new InsertBefore.InsertIterator(pair.value.iterate(),
                        existing.iterate(),
                        1)
                    var combinedValue: GroundedValue =
                      combinedIter.materialize
                    baseMap.initialPut(pair.key.getStringValue, combinedValue)
                  case "use-callback" =>
                    var args: Array[Sequence] = Array(existing, pair.value)
                    var combined: Sequence = onDuplicates.call(context, args)
                    baseMap.initialPut(pair.key.getStringValue,
                      combined.materialize)
                  case _ =>
                    throw new XPathException(
                      "Duplicate key in constructed map: " + Err.wrap(
                        pair.key.getStringValueCS),
                      duplicatesErrorCode)

                }
              } else {
                baseMap.initialPut(pair.key.getStringValue, pair.value)
              }
            }
            baseMap

        }
      } else {
        val iter: SequenceIterator = arguments(0).iterate()
        var baseMap: MapItem = iter.next().asInstanceOf[MapItem]
        if (baseMap == null) {
          new HashTrieMap()
        } else {
          if (!(baseMap.isInstanceOf[HashTrieMap])) {
            baseMap = HashTrieMap.copy(baseMap)
          }
          var next: MapItem = null
          while (({
            next = iter.next().asInstanceOf[MapItem]
            next
          }) != null) for (pair <- next
            .keyValuePairs.asScala) {
            val existing: Sequence = baseMap.get(pair.key)
            if (existing != null) {
              duplicates match {
                case "use-first" | "unspecified" | "use-any" =>
                case "use-last" =>
                  baseMap = baseMap.addEntry(pair.key, pair.value)
                case "combine" =>
                  var combinedIter: InsertBefore.InsertIterator =
                    new InsertBefore.InsertIterator(pair.value.iterate(),
                      existing.iterate(),
                      1)
                  var combinedValue: GroundedValue = combinedIter.materialize
                  baseMap = baseMap.addEntry(pair.key, combinedValue)
                case "use-callback" =>
                  assert(onDuplicates != null)
                  var args: Array[Sequence] =
                    if (onDuplicates.getArity == 2) Array(existing, pair.value)
                    else Array(existing, pair.value, pair.key)
                  var combined: Sequence = onDuplicates.call(context, args)
                  baseMap = baseMap.addEntry(pair.key, combined.materialize)
                case _ =>
                  throw new XPathException(
                    "Duplicate key in constructed map: " + Err.wrap(
                      pair.key.getStringValueCS),
                    duplicatesErrorCode)

              }
            } else {
              baseMap = baseMap.addEntry(pair.key, pair.value)
            }
          }
          baseMap
        }
      }
    }

    override def getStreamerName: String = "NewMap"

    override def exportAdditionalArguments(call: SystemFunctionCall,
                                           out: ExpressionPresenter): Unit = {
      if (call.getArity == 1) {
        val options: HashTrieMap = new HashTrieMap()
        options.initialPut(new StringValue("duplicates"),
          new StringValue(duplicates))
        options.initialPut(new StringValue("duplicates-error-code"),
          new StringValue(duplicatesErrorCode))
        Literal.exportValue(options, out)
      }
    }

  }

  class MapPut extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): MapItem = {
      var baseMap: MapItem = arguments(0).head.asInstanceOf[MapItem]
      if (!(baseMap.isInstanceOf[HashTrieMap])) {
        baseMap = HashTrieMap.copy(baseMap)
      }
      val key: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
      val value: GroundedValue =
        arguments(2).materialize
      baseMap.addEntry(key, value)
    }

  }

  class MapRemove extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): MapItem = {
      var map: MapItem = arguments(0).head.asInstanceOf[MapItem]
      val iter: SequenceIterator = arguments(1).iterate()
      var key: AtomicValue = null
      while (({
        key = iter.next().asInstanceOf[AtomicValue]
        key
      }) != null) map =
        map.remove(key)
      map
    }

  }

  class MapSize extends SystemFunction {

    def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue = {
      val map: MapItem = arguments(0).head.asInstanceOf[MapItem]
      new Int64Value(map.size)
    }

  }

}

class MapFunctionSet extends BuiltInFunctionSet {
  locally {
    register("merge", 1, () => new MapMerge, MapType.ANY_MAP_TYPE, ONE, 0).arg(
      0,
      MapType.ANY_MAP_TYPE,
      STAR | INS,
      null)
    val ON_DUPLICATES_CALLBACK_TYPE: SpecificFunctionType =
      new SpecificFunctionType(
        Array(SequenceType.ANY_SEQUENCE, SequenceType.ANY_SEQUENCE),
        SequenceType.ANY_SEQUENCE)
    val oneOnDuplicatesFunction: SequenceType = SequenceType.makeSequenceType(
      ON_DUPLICATES_CALLBACK_TYPE,
      StaticProperty.EXACTLY_ONE)
    val mergeOptionDetails: OptionsParameter = new OptionsParameter()
    mergeOptionDetails.addAllowedOption("duplicates",
      SequenceType.SINGLE_STRING,
      new StringValue("use-first"))
    mergeOptionDetails.setAllowedValues("duplicates",
      "FOJS0005",
      "use-first",
      "use-last",
      "combine",
      "reject",
      "unspecified",
      "use-any",
      "use-callback")
    mergeOptionDetails.addAllowedOption(MapMerge.errorCodeKey,
      SequenceType.SINGLE_STRING,
      new StringValue("FOJS0003"))
    mergeOptionDetails.addAllowedOption(MapMerge.keyTypeKey,
      SequenceType.SINGLE_STRING,
      new StringValue("anyAtomicType"))
    mergeOptionDetails.addAllowedOption(MapMerge.finalKey,
      SequenceType.SINGLE_BOOLEAN,
      BooleanValue.FALSE)
    mergeOptionDetails.addAllowedOption(MapMerge.onDuplicatesKey,
      oneOnDuplicatesFunction,
      null)
    register("merge", 2, () => new MapMerge, MapType.ANY_MAP_TYPE, ONE, 0)
      .arg(0, MapType.ANY_MAP_TYPE, STAR, null)
      .arg(1, MapType.ANY_MAP_TYPE, ONE, null)
      .optionDetails(mergeOptionDetails)
    register("entry", 2, () => new MapEntry, MapType.ANY_MAP_TYPE, ONE, 0)
      .arg(0, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
      .arg(1, AnyItemType, STAR | NAV, null)
    register("find", 2, () => new MapFind, ArrayItemType.ANY_ARRAY_TYPE, ONE, 0)
      .arg(0, AnyItemType, STAR | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
    register("get", 2, () => new MapGet, AnyItemType, STAR, 0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
    register("put", 3, () => new MapPut, MapType.ANY_MAP_TYPE, ONE, 0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
      .arg(2, AnyItemType, STAR | NAV, null)
    register("contains",
      2,
      () => new MapContains,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
    register("remove", 2, () => new MapRemove, MapType.ANY_MAP_TYPE, ONE, 0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, STAR | ABS, null)
    register("keys",
      1,
      () => new MapKeys,
      BuiltInAtomicType.ANY_ATOMIC,
      STAR,
      0).arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
    register("size", 1, () => new MapSize, BuiltInAtomicType.INTEGER, ONE, 0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
    val actionType: ItemType = new SpecificFunctionType(
      Array(SequenceType.SINGLE_ATOMIC, SequenceType.ANY_SEQUENCE),
      SequenceType.ANY_SEQUENCE)
    register("for-each",
      2,
      () => new MapForEach,
      AnyItemType,
      STAR,
      0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, actionType, ONE | INS, null)
    register("untyped-contains",
      2,
      () => new MapUntypedContains,
      BuiltInAtomicType.BOOLEAN,
      ONE,
      0)
      .arg(0, MapType.ANY_MAP_TYPE, ONE | INS, null)
      .arg(1, BuiltInAtomicType.ANY_ATOMIC, ONE | ABS, null)
  }

  override def getNamespace: String = NamespaceConstant.MAP_FUNCTIONS
  override def getConventionalPrefix: String = "map"
}
