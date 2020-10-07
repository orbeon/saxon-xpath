package org.orbeon.saxon.model

import java.util
import java.util._
import java.util.concurrent.ConcurrentHashMap

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.functions.hof.FunctionSequenceCoercer
import org.orbeon.saxon.lib.FunctionAnnotationHandler
import org.orbeon.saxon.ma.map.MapType
import org.orbeon.saxon.model.Affinity._
import org.orbeon.saxon.model.TypeHierarchy._
import org.orbeon.saxon.om.{Sequence, SequenceIterator, SequenceTool}
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.{IntHashSet, IntSet, IntUniversalSet}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object TypeHierarchy {

  private def stabilize(in: ItemType): ItemType =
    in match {
      case test: SameNameTest =>
        test.getEquivalentNameTest
      case _ =>
        in
    }

  private def requireTrueItemType(t: ItemType): Unit = {
    Objects.requireNonNull(t)
    if (! t.isTrueItemType)
      throw new AssertionError(s"$t is a non-pure union type")
  }

  private def nameTestRelationship(t1: QNameTest, t2: QNameTest): Affinity =
    if (t1 == t2) {
      SAME_TYPE
    } else t2 match {
      case test: NameTest =>
        if (t1.matches(test.getMatchingNodeName)) SUBSUMES else DISJOINT
      case _ => t1 match {
        case test: NameTest =>
          if (t2.matches(test.getMatchingNodeName)) SUBSUMED_BY else DISJOINT
        case _ => t2 match {
          case test: SameNameTest =>
            if (t1.matches(test.getMatchingNodeName)) SUBSUMES else DISJOINT
          case _ => t1 match {
            case test: SameNameTest =>
              if (t2.matches(test.getMatchingNodeName)) SUBSUMED_BY else DISJOINT
            case _: NamespaceTest if t2.isInstanceOf[NamespaceTest] =>
              DISJOINT
            case _: LocalNameTest if t2.isInstanceOf[LocalNameTest] =>
              DISJOINT
            case _ =>
              OVERLAPS
          }
        }
      }
    }

  private def combineRelationships(rel1: Affinity, rel2: Affinity): Affinity =
    if (rel1 == SAME_TYPE && rel2 == SAME_TYPE) {
      SAME_TYPE
    } else if ((rel1 == SAME_TYPE || rel1 == SUBSUMES) && (rel2 == SAME_TYPE || rel2 == SUBSUMES)) {
      SUBSUMES
    } else if ((rel1 == SAME_TYPE || rel1 == SUBSUMED_BY) && (rel2 == SAME_TYPE || rel2 == SUBSUMED_BY)) {
      SUBSUMED_BY
    } else if (rel1 == DISJOINT || rel2 == DISJOINT) {
      DISJOINT
    } else {
      OVERLAPS
    }

  private def toSet[X](in: Iterable[X]): Set[X] = {
    val s = new HashSet[X]()
    for (x <- in)
      s.add(x)
    s
  }

  def inverseRelationship(relation: Affinity): Affinity = relation match {
    case SAME_TYPE => SAME_TYPE
    case SUBSUMES => SUBSUMED_BY
    case SUBSUMED_BY => SUBSUMES
    case OVERLAPS => OVERLAPS
    case DISJOINT => DISJOINT
    case _ => throw new IllegalArgumentException()

  }

  private class ItemTypePair(var s: ItemType, var t: ItemType) {

    override def hashCode: Int = s.hashCode ^ t.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case pair: ItemTypePair => s == pair.s && t == pair.t
      case _                  => false
    }
  }
}

class TypeHierarchy(var config: Configuration) {

  private val map: Map[ItemTypePair, Affinity] = new ConcurrentHashMap()

  def applyFunctionConversionRules(value: Sequence,
                                   requiredType: SequenceType,
                                   role: RoleDiagnostic,
                                   locator: Location): Sequence = {
    var suppliedItemType = SequenceTool.getItemType(value, this)
    var iterator         = value.iterate()
    val requiredItemType = requiredType.getPrimaryType

    // step 1: apply atomization if necessary
    if (requiredItemType.isPlainType) {
      if (! suppliedItemType.isPlainType) {
        try
          iterator = Atomizer.getAtomizingIterator(iterator, oneToOne = false)
        catch {
          case e: XPathException =>
            val vf: ValidationFailure = new ValidationFailure(
              "Failed to atomize the " + role.getMessage + ": " + e.getMessage)
            vf.setErrorCode("XPTY0117")
            throw vf.makeException()
        }
        suppliedItemType = suppliedItemType.getAtomizedItemType
      }

      // step 2: convert untyped atomic values to target item type
      if (relationship(suppliedItemType, BuiltInAtomicType.UNTYPED_ATOMIC) != DISJOINT &&
        ! isSubType(BuiltInAtomicType.UNTYPED_ATOMIC, requiredItemType)) {
        val nsSensitive = requiredItemType.asInstanceOf[SimpleType].isNamespaceSensitive
        var converter: ItemMappingFunction = null
        if (nsSensitive) {
          converter = item =>
            if (item.isInstanceOf[UntypedAtomicValue]) {
              val vf = new ValidationFailure(
                "Failed to convert the " + role.getMessage + ": " + "Implicit conversion of untypedAtomic value to " +
                  requiredItemType +
                  " is not allowed")
              vf.setErrorCode("XPTY0117")
              throw vf.makeException()
            } else {
              return item
            }
        } else if (requiredItemType.asInstanceOf[SimpleType].isUnionType) {
          val rules = config.getConversionRules
          converter = item =>
            if (item.isInstanceOf[UntypedAtomicValue]) {
              try requiredItemType
                .asInstanceOf[SimpleType]
                .getTypedValue(item.getStringValueCS, null, rules)
                .head
              catch {
                case ve: ValidationException => {
                  ve.setErrorCode("XPTY0004")
                  throw ve
                }
              }
            } else {
              return item
            }
        } else {
          converter = item =>
            item match {
              case atomicValue: UntypedAtomicValue =>
                Converter.convert(atomicValue,
                  requiredItemType.asInstanceOf[AtomicType],
                  config.getConversionRules)
              case _ =>
                return item
            }
        }
        iterator = new ItemMappingIterator(iterator, converter, true)
      }

      // step 3: apply numeric promotion
      if (requiredItemType == BuiltInAtomicType.DOUBLE) {
        val promoter: ItemMappingFunction = item =>
          item match {
            case numericValue: NumericValue =>
              return Converter
                .convert(numericValue,
                  BuiltInAtomicType.DOUBLE,
                  config.getConversionRules)
                .asAtomic()
                .asInstanceOf[DoubleValue]
            case _ =>
              throw new XPathException(
                "Failed to convert the " + role.getMessage + ": " + "Cannot promote non-numeric value to xs:double",
                "XPTY0004")
          }
        iterator = new ItemMappingIterator(iterator, promoter, true)
      } else if (requiredItemType == BuiltInAtomicType.FLOAT) {
        val promoter: ItemMappingFunction = item =>
          if (item.isInstanceOf[DoubleValue]) {
            throw new XPathException(
              "Failed to convert the " + role.getMessage + ": " + "Cannot promote xs:double value to xs:float",
              "XPTY0004")
          } else if (item.isInstanceOf[NumericValue]) {
            return Converter
              .convert(item.asInstanceOf[NumericValue],
                BuiltInAtomicType.FLOAT,
                config.getConversionRules)
              .asAtomic()
              .asInstanceOf[FloatValue]
          } else {
            throw new XPathException(
              "Failed to convert the " + role.getMessage + ": " + "Cannot promote non-numeric value to xs:float",
              "XPTY0004")
          }
        iterator = new ItemMappingIterator(iterator, promoter, true)
      }

      // step 4: apply URI-to-string promotion
      if (requiredItemType == BuiltInAtomicType.STRING &&
        relationship(suppliedItemType, BuiltInAtomicType.ANY_URI) != DISJOINT) {
        val promoter: ItemMappingFunction = item =>
          return {
            if (item.isInstanceOf[AnyURIValue])
              new StringValue(item.getStringValueCS)
            else
              item
          }
        iterator = new ItemMappingIterator(iterator, promoter, true)
      }
    }

    // step 5: apply function coercion
    iterator = applyFunctionCoercion(iterator, suppliedItemType, requiredItemType, locator)

    // Add a check that the values conform to the required type
    val relation = relationship(suppliedItemType, requiredItemType)
    if (! (relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      val itemChecker = new ItemTypeCheckingFunction(requiredItemType, role, locator, config)
      iterator = new ItemMappingIterator(iterator, itemChecker, true)
    }

    if (requiredType.getCardinality != StaticProperty.ALLOWS_ZERO_OR_MORE)
      iterator = new CardinalityCheckingIterator(iterator, requiredType.getCardinality, role, locator)
    SequenceTool.toMemoSequence(iterator)
  }

  def applyFunctionCoercion(iterator: SequenceIterator,
                            suppliedItemType: ItemType,
                            requiredItemType: ItemType,
                            locator: Location): SequenceIterator =
    requiredItemType match {
      case itemType: FunctionItemType if !(relationship(requiredItemType, suppliedItemType) == Affinity.SUBSUMES) && !itemType.isArrayType && !itemType.isMapType =>
        if (requiredItemType == AnyFunctionType) {
          iterator
        } else {
          val coercer: FunctionSequenceCoercer.Coercer =
            new FunctionSequenceCoercer.Coercer(
              requiredItemType.asInstanceOf[SpecificFunctionType],
              config,
              locator)
          new ItemMappingIterator(iterator, coercer, true)
        }
      case _ =>
        iterator
    }

  def getConfiguration: Configuration = config

  def isSubType(subtype: ItemType, supertype: ItemType): Boolean = {
    val relation: Affinity = relationship(subtype, supertype)
    relation == SAME_TYPE || relation == SUBSUMED_BY
  }

  def relationship(t1: ItemType, t2: ItemType): Affinity = {
    var itmTyp1 = t1
    var itmTyp2 = t2
    Objects.requireNonNull(itmTyp1)
    Objects.requireNonNull(itmTyp2)
    itmTyp1 = stabilize(itmTyp1)
    itmTyp2 = stabilize(itmTyp2)

    if (itmTyp1 == itmTyp2) {
      SAME_TYPE
    } else if (itmTyp2 eq AnyItemType) {
      SUBSUMED_BY
    } else if (itmTyp1 eq AnyItemType) {
      SUBSUMES
    } else itmTyp1 match {
      case _: BuiltInAtomicType if itmTyp2
        .isInstanceOf[BuiltInAtomicType] =>
        if (itmTyp1.getBasicAlphaCode.startsWith(itmTyp2.getBasicAlphaCode)) {
          SUBSUMED_BY
        } else if (itmTyp2.getBasicAlphaCode.startsWith(itmTyp1.getBasicAlphaCode)) {
          SUBSUMES
        } else {
          DISJOINT
        }
      case ErrorType =>
        SUBSUMED_BY
      case _ =>
        if (itmTyp2 eq ErrorType) {
          SUBSUMES
        } else {
          val pair: ItemTypePair = new ItemTypePair(itmTyp1, itmTyp2)
          var result: Affinity = map.get(pair)
          if (result == null) {
            result = computeRelationship(itmTyp1, itmTyp2)
            map.put(pair, result)
          }
          result
        }
    }
  }

  private def computeRelationship(t1: ItemType, t2: ItemType): Affinity = {
    requireTrueItemType(t1)
    requireTrueItemType(t2)
    try {
      if (t1 == t2) {
        SAME_TYPE
      } else if (t1 eq AnyItemType) {
        if (t2 eq AnyItemType) {
          SAME_TYPE
        } else {
          SUBSUMES
        }
      } else if (t2 eq AnyItemType) {
        SUBSUMED_BY
      } else if (t1.isPlainType) {
        if (t2.isInstanceOf[NodeTest] || t2.isInstanceOf[FunctionItemType] || t2.isInstanceOf[JavaExternalObjectType]) {
          DISJOINT
        } else if (t1 == BuiltInAtomicType.ANY_ATOMIC && t2.isPlainType) {
          SUBSUMES
        } else if (t2 == BuiltInAtomicType.ANY_ATOMIC) {
          SUBSUMED_BY
        } else if (t1.isInstanceOf[AtomicType] && t2.isInstanceOf[AtomicType]) {
          if (t1.asInstanceOf[AtomicType].getFingerprint == t2.asInstanceOf[AtomicType].getFingerprint) {
            return SAME_TYPE
          }
          var t: AtomicType = t2.asInstanceOf[AtomicType]
          breakable {
            while (true) {
              if (t1.asInstanceOf[AtomicType].getFingerprint == t.getFingerprint) {
                return SUBSUMES
              }
              val st: SchemaType = t.getBaseType
              if (st.isInstanceOf[AtomicType]) {
                t = st.asInstanceOf[AtomicType]
              } else {
                break()
              }
            }
          }
          t = t1.asInstanceOf[AtomicType]
          breakable {
            while (true) {
              if (t.getFingerprint == t2.asInstanceOf[AtomicType].getFingerprint) {
                return SUBSUMED_BY
              }
              val st: SchemaType = t.getBaseType
              if (st.isInstanceOf[AtomicType]) {
                t = st.asInstanceOf[AtomicType]
              } else {
                break()
              }
            }
          }
          DISJOINT
        } else if (!t1.isAtomicType && t2.isPlainType) {
          val s1: Set[_ <: PlainType] = toSet(
            t1.asInstanceOf[PlainType].getPlainMemberTypes)
          val s2: Set[_ <: PlainType] = toSet(
            t2.asInstanceOf[PlainType].getPlainMemberTypes)
          if (!unionOverlaps(s1, s2)) {
            return DISJOINT
          }
          val gt: Boolean = s1.containsAll(s2)
          val lt: Boolean = s2.containsAll(s1)
          if (gt && lt) {
            SAME_TYPE
          } else if (gt) {
            SUBSUMES
          } else if (lt) {
            SUBSUMED_BY
          } else if (unionSubsumes(s1, s2)) {
            SUBSUMES
          } else if (unionSubsumes(s2, s1)) {
            SUBSUMED_BY
          } else {
            OVERLAPS
          }
        } else if (t1.isInstanceOf[AtomicType]) {
          val r: Affinity = relationship(t2, t1)
          inverseRelationship(r)
        } else {
          throw new IllegalStateException()
        }
      } else if (t1.isInstanceOf[NodeTest]) {
        if (t2.isPlainType || t2.isInstanceOf[FunctionItemType]) {
          DISJOINT
        } else {
          if (t1.isInstanceOf[AnyNodeTest]) {
            if (t2.isInstanceOf[AnyNodeTest]) {
              SAME_TYPE
            } else {
              SUBSUMES
            }
          } else if (t2.isInstanceOf[AnyNodeTest]) {
            SUBSUMED_BY
          } else if (t2 eq ErrorType) {
            DISJOINT
          } else {
            var nodeKindRelationship: Affinity = null
            val m1 = t1.getUType
            val m2 = t2.getUType
            if (!m1.overlaps(m2)) {
              return DISJOINT
            } else
              nodeKindRelationship =
                if (m1 == m2) SAME_TYPE
                else if (m2.subsumes(m1)) SUBSUMED_BY
                else if (m1.subsumes(m2)) SUBSUMES
                else OVERLAPS
            var nodeNameRelationship: Affinity = null
            val on1: Optional[IntSet] =
              t1.asInstanceOf[NodeTest].getRequiredNodeNames
            val on2: Optional[IntSet] =
              t2.asInstanceOf[NodeTest].getRequiredNodeNames
            t1 match {
              case test: QNameTest if t2.isInstanceOf[QNameTest] =>
                nodeNameRelationship = nameTestRelationship(test, t2.asInstanceOf[QNameTest])
              case _ => if (on1.isPresent && on1.get.isInstanceOf[IntUniversalSet]) {
                nodeNameRelationship =
                  if (on2.isPresent && on2.get.isInstanceOf[IntUniversalSet])
                    SAME_TYPE
                  else
                    SUBSUMES
              } else if (on2.isPresent && on2.get.isInstanceOf[IntUniversalSet]) {
                nodeNameRelationship = SUBSUMED_BY
              } else if (!(on1.isPresent && on2.isPresent)) {
                nodeNameRelationship = if (t1 == t2) SAME_TYPE else OVERLAPS
              } else {
                val n1 = on1.get
                val n2 = on2.get
                nodeNameRelationship =
                  if (n1.containsAll(n2))
                    if (n1.size == n2.size)
                      SAME_TYPE
                    else
                      SUBSUMES
                  else if (n2.containsAll(n1))
                    SUBSUMED_BY
                  else if (IntHashSet.containsSome(n1, n2))
                    OVERLAPS
                  else
                    DISJOINT
              }
            }
            val contentRelationship: Affinity =
              computeContentRelationship(t1, t2, on1, on2)
            if (nodeKindRelationship == SAME_TYPE && nodeNameRelationship == SAME_TYPE &&
              contentRelationship == SAME_TYPE) {
              SAME_TYPE
            } else if ((nodeKindRelationship == SAME_TYPE || nodeKindRelationship == SUBSUMES) &&
              (nodeNameRelationship == SAME_TYPE || nodeNameRelationship == SUBSUMES) &&
              (contentRelationship == SAME_TYPE || contentRelationship == SUBSUMES)) {
              SUBSUMES
            } else if ((nodeKindRelationship == SAME_TYPE || nodeKindRelationship == SUBSUMED_BY) &&
              (nodeNameRelationship == SAME_TYPE || nodeNameRelationship == SUBSUMED_BY) &&
              (contentRelationship == SAME_TYPE || contentRelationship == SUBSUMED_BY)) {
              SUBSUMED_BY
            } else if (nodeNameRelationship == DISJOINT || contentRelationship == DISJOINT) {
              DISJOINT
            } else {
              OVERLAPS
            }
          }
        }
      } else t2 match {
        case _: FunctionItemType if t1.isInstanceOf[AnyExternalObjectType] =>
          if (!(t2.isInstanceOf[AnyExternalObjectType])) {
            return DISJOINT
          }
          t1 match {
            case objectType: JavaExternalObjectType =>
              if (t2 == AnyExternalObjectType.THE_INSTANCE) {
                SUBSUMED_BY
              } else t2 match {
                case objectType1: JavaExternalObjectType =>
                  objectType.getRelationship(objectType1)
                case _ =>
                  DISJOINT
              }
            case _ => if (t2.isInstanceOf[JavaExternalObjectType]) {
              SUBSUMES
            } else {
              DISJOINT
            }
          }
        case _ =>
          t1 match {
            case mapType: MapType if t2.isInstanceOf[MapType] =>
              if (t1 == MapType.EMPTY_MAP_TYPE) {
                return SUBSUMED_BY
              } else if (t2 == MapType.EMPTY_MAP_TYPE) {
                return SUBSUMES
              } else if (t1 == MapType.ANY_MAP_TYPE) {
                return SUBSUMES
              } else if (t2 == MapType.ANY_MAP_TYPE) {
                return SUBSUMED_BY
              }

              val k1: AtomicType = mapType.getKeyType
              val k2: AtomicType = t2.asInstanceOf[MapType].getKeyType
              val v1: SequenceType = mapType.getValueType
              val v2: SequenceType = t2.asInstanceOf[MapType].getValueType
              val keyRel: Affinity = relationship(k1, k2)
              val valueRel: Affinity = sequenceTypeRelationship(v1, v2)
              val rel: Affinity = combineRelationships(keyRel, valueRel)
              if (rel == SAME_TYPE || rel == SUBSUMES || rel == SUBSUMED_BY) {
                return rel
              }
            case _ =>
          }
          t2 match {
            case itemType: FunctionItemType =>

              val signatureRelationship =
                t1.asInstanceOf[FunctionItemType].relationship(itemType, this)

              if (signatureRelationship == DISJOINT) {
                DISJOINT
              } else {
                var assertionRelationship = SAME_TYPE
                val first =
                  t1.asInstanceOf[FunctionItemType].getAnnotationAssertions
                val second =
                  itemType.getAnnotationAssertions
                val namespaces = new util.HashSet[String]()
                for (a <- first.asScala)
                  namespaces.add(a.getAnnotationQName.getURI)
                for (a <- second.asScala)
                  namespaces.add(a.getAnnotationQName.getURI)
                for (ns <- namespaces.asScala) {
                  val handler: FunctionAnnotationHandler =
                    config.getFunctionAnnotationHandler(ns)
                  if (handler != null) {
                    var localRel: Affinity = SAME_TYPE
                    val firstFiltered: AnnotationList = first.filterByNamespace(ns)
                    val secondFiltered: AnnotationList =
                      second.filterByNamespace(ns)
                    if (firstFiltered.isEmpty) {
                      if (secondFiltered.isEmpty) {} else {
                        localRel = SUBSUMES
                      }
                    } else {
                      localRel =
                        if (secondFiltered.isEmpty)
                          SUBSUMED_BY
                        else
                          handler.relationship(firstFiltered, secondFiltered)
                    }
                    assertionRelationship =
                      combineRelationships(assertionRelationship, localRel)
                  }
                }
                combineRelationships(signatureRelationship, assertionRelationship)
              }
            case _ =>
              DISJOINT
          }
      }
    } catch {
      case _: MissingComponentException =>
        OVERLAPS
    }
  }

  private def unionSubsumes(s1: Set[_ <: PlainType],
                            s2: Set[_ <: PlainType]): Boolean = {
    for (t2 <- s2.asScala) {
      var t2isSubsumed: Boolean = false
      breakable {
        for (t1 <- s1.asScala) {
          val rel = relationship(t1, t2)
          if (rel == SUBSUMES || rel == SAME_TYPE) {
            t2isSubsumed = true
            break()
          }
        }
      }
      if (! t2isSubsumed)
        return false
    }
    true
  }

  private def unionOverlaps(s1: Set[_ <: PlainType],
                            s2: Set[_ <: PlainType]): Boolean = {
    for (t2 <- s2.asScala; t1 <- s1.asScala) {
      val rel: Affinity = relationship(t1, t2)
      if (rel != DISJOINT)
        return true
    }
    false
  }

  def computeContentRelationship(t1: ItemType,
                                 t2: ItemType,
                                 n1: Optional[IntSet],
                                 n2: Optional[IntSet]): Affinity = {
    var contentRelationship: Affinity = null
    t1 match {
      case test: DocumentNodeTest =>
        contentRelationship =
          t2 match {
            case test1: DocumentNodeTest => relationship(test.getElementTest, test1.getElementTest)
            case _ => SUBSUMED_BY
          }
      case _ =>
        if (t2.isInstanceOf[DocumentNodeTest]) {
          contentRelationship = SUBSUMES
        } else {
          val s1 = t1.asInstanceOf[NodeTest].getContentType
          val s2 = t2.asInstanceOf[NodeTest].getContentType
          contentRelationship = schemaTypeRelationship(s1, s2)
        }
    }
    val nillable1 = t1.asInstanceOf[NodeTest].isNillable
    val nillable2 = t2.asInstanceOf[NodeTest].isNillable
    if (nillable1 != nillable2)
      contentRelationship match {
        case SUBSUMES =>
          if (nillable2)
            contentRelationship = OVERLAPS
        case SUBSUMED_BY =>
          if (nillable1)
            contentRelationship = OVERLAPS
        case SAME_TYPE =>
          contentRelationship = if (nillable1) SUBSUMES else SUBSUMED_BY
        case _ =>
      }
    contentRelationship
  }

  def sequenceTypeRelationship(s1: SequenceType, s2: SequenceType): Affinity = {
    val c1 = s1.getCardinality
    val c2 = s2.getCardinality
    val cardRel =
      if (c1 == c2) {
        SAME_TYPE
      } else if (Cardinality.subsumes(c1, c2)) {
        SUBSUMES
      } else if (Cardinality.subsumes(c2, c1)) {
        SUBSUMED_BY
      } else if (c1 == StaticProperty.EMPTY && !Cardinality.allowsZero(c2)) {
        return DISJOINT
      } else if (c2 == StaticProperty.EMPTY && !Cardinality.allowsZero(c1)) {
        return DISJOINT
      } else {
        OVERLAPS
      }

    val itemRel = relationship(s1.getPrimaryType, s2.getPrimaryType)
    if (itemRel == DISJOINT)
      DISJOINT
    else if (cardRel == SAME_TYPE || cardRel == itemRel)
      itemRel
    else if (itemRel == SAME_TYPE)
      cardRel
    else
      OVERLAPS
  }

  def schemaTypeRelationship(s1: SchemaType, s2: SchemaType): Affinity = {
    if (s1.isSameType(s2)) {
      SAME_TYPE
    } else if (s1.isInstanceOf[AnyType.AnyType]) {
      SUBSUMES
    } else if (s2.isInstanceOf[AnyType.AnyType]) {
      SUBSUMED_BY
    } else if (s1.isInstanceOf[Untyped.Untyped] && (s2 == BuiltInAtomicType.ANY_ATOMIC || s2 == BuiltInAtomicType.UNTYPED_ATOMIC)) {
      OVERLAPS
    } else if (s2.isInstanceOf[Untyped.Untyped] && (s1 == BuiltInAtomicType.ANY_ATOMIC || s1 == BuiltInAtomicType.UNTYPED_ATOMIC)) {
      OVERLAPS
    } else s1 match {
      case plainType: PlainType if s2.asInstanceOf[PlainType].isPlainType && s2.isInstanceOf[PlainType] && plainType.isPlainType =>
        relationship(s1.asInstanceOf[ItemType], s2.asInstanceOf[ItemType])
      case _ =>
        var t1 = s1
        while ({
          t1 = t1.getBaseType
          t1
        } != null)
          if (t1.isSameType(s2))
            return SUBSUMED_BY
        var t2 = s2
        while ({
          t2 = t2.getBaseType
          t2
        } != null)
          if (t2.isSameType(s1))
            return SUBSUMES

        DISJOINT
    }
  }

  def getGenericFunctionItemType: ItemType = AnyItemType
}
