package net.sf.saxon.model

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser.RoleDiagnostic
import net.sf.saxon.functions.hof.FunctionSequenceCoercer
import net.sf.saxon.lib.ConversionRules
import net.sf.saxon.lib.FunctionAnnotationHandler
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.om.SequenceTool
import net.sf.saxon.pattern._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value._
import net.sf.saxon.z.IntHashSet
import net.sf.saxon.z.IntSet
import net.sf.saxon.z.IntUniversalSet
import java.util._
import java.util.concurrent.ConcurrentHashMap

import net.sf.saxon.model.Affinity._
import TypeHierarchy._
import net.sf.saxon.query.AnnotationList

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object TypeHierarchy {

  private def stabilize(in: ItemType): ItemType =
    if (in.isInstanceOf[SameNameTest]) {
      in.asInstanceOf[SameNameTest].getEquivalentNameTest
    } else {
      in
    }

  private def requireTrueItemType(t: ItemType): Unit = {
    Objects.requireNonNull(t)
    if (!t.isTrueItemType) {
      throw new AssertionError(s"$t is a non-pure union type")
    }
  }

  private def nameTestRelationship(t1: QNameTest, t2: QNameTest): Affinity =
    if (t1 == t2) {
      SAME_TYPE
    } else if (t2.isInstanceOf[NameTest]) {
      if (t1.matches(t2.asInstanceOf[NameTest].getMatchingNodeName)) SUBSUMES else DISJOINT
    } else if (t1.isInstanceOf[NameTest]) {
      if (t2.matches(t1.asInstanceOf[NameTest].getMatchingNodeName)) SUBSUMED_BY else DISJOINT
    } else if (t2.isInstanceOf[SameNameTest]) {
      if (t1.matches(t2.asInstanceOf[SameNameTest].getMatchingNodeName)) SUBSUMES else DISJOINT
    } else if (t1.isInstanceOf[SameNameTest]) {
      if (t2.matches(t1.asInstanceOf[SameNameTest].getMatchingNodeName)) SUBSUMED_BY else DISJOINT
    } else if (t1.isInstanceOf[NamespaceTest] && t2.isInstanceOf[NamespaceTest]) {
      DISJOINT
    } else if (t1.isInstanceOf[LocalNameTest] && t2.isInstanceOf[LocalNameTest]) {
      DISJOINT
    } else {
      OVERLAPS
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
    val s: Set[X] = new HashSet[X]()
    for (x <- in) {
      s.add(x)
    }
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

    override def hashCode(): Int = s.hashCode ^ t.hashCode

    override def equals(obj: Any): Boolean = obj match {
      case obj: ItemTypePair => {
        val pair: ItemTypePair = obj
        s == pair.s && t == pair.t
      }
      case _ => false

    }

  }

}

class TypeHierarchy(var config: Configuration) {

  private val map: Map[ItemTypePair, Affinity] = new ConcurrentHashMap()

  def applyFunctionConversionRules(value: Sequence,
                                   requiredType: SequenceType,
                                   role: RoleDiagnostic,
                                   locator: Location): Sequence = {
    var suppliedItemType: ItemType = SequenceTool.getItemType(value, this)
    var iterator: SequenceIterator = value.iterate()
    val requiredItemType: ItemType = requiredType.getPrimaryType
    if (requiredItemType.isPlainType) {
      if (!suppliedItemType.isPlainType) {
        try iterator = Atomizer.getAtomizingIterator(iterator, oneToOne = false)
        catch {
          case e: XPathException => {
            val vf: ValidationFailure = new ValidationFailure(
              "Failed to atomize the " + role.getMessage + ": " + e.getMessage)
            vf.setErrorCode("XPTY0117")
            throw vf.makeException()
          }

        }
        suppliedItemType = suppliedItemType.getAtomizedItemType
      }
      if (relationship(suppliedItemType, BuiltInAtomicType.UNTYPED_ATOMIC) !=
        DISJOINT &&
        !isSubType(BuiltInAtomicType.UNTYPED_ATOMIC, requiredItemType)) {
        val nsSensitive: Boolean =
          requiredItemType.asInstanceOf[SimpleType].isNamespaceSensitive
        var converter: ItemMappingFunction = null
        if (nsSensitive) {
          converter = (item) =>
            if (item.isInstanceOf[UntypedAtomicValue]) {
              var vf: ValidationFailure = new ValidationFailure(
                "Failed to convert the " + role.getMessage + ": " + "Implicit conversion of untypedAtomic value to " +
                  requiredItemType +
                  " is not allowed")
              vf.setErrorCode("XPTY0117")
              throw vf.makeException()
            } else {
              item
            }
        } else if (requiredItemType.asInstanceOf[SimpleType].isUnionType) {
          val rules: ConversionRules = config.getConversionRules
          converter = (item) =>
            if (item.isInstanceOf[UntypedAtomicValue]) {
              try requiredItemType
                .asInstanceOf[SimpleType]
                .getTypedValue(item.getStringValueCS, null, rules)
                .head()
              catch {
                case ve: ValidationException => {
                  ve.setErrorCode("XPTY0004")
                  throw ve
                }

              }
            } else {
              item
            }
        } else {
          converter = (item) =>
            if (item.isInstanceOf[UntypedAtomicValue]) {
              Converter.convert(item.asInstanceOf[UntypedAtomicValue],
                requiredItemType.asInstanceOf[AtomicType],
                config.getConversionRules)
            } else {
              item
            }
        }
        iterator = new ItemMappingIterator(iterator, converter, true)
      }
      if (requiredItemType == BuiltInAtomicType.DOUBLE) {
        val promoter: ItemMappingFunction = (item) =>
          if (item.isInstanceOf[NumericValue]) {
            Converter
              .convert(item.asInstanceOf[NumericValue],
                BuiltInAtomicType.DOUBLE,
                config.getConversionRules)
              .asAtomic()
              .asInstanceOf[DoubleValue]
          } else {
            throw new XPathException(
              "Failed to convert the " + role.getMessage + ": " + "Cannot promote non-numeric value to xs:double",
              "XPTY0004")
          }
        iterator = new ItemMappingIterator(iterator, promoter, true)
      } else if (requiredItemType == BuiltInAtomicType.FLOAT) {
        val promoter: ItemMappingFunction = (item) =>
          if (item.isInstanceOf[DoubleValue]) {
            throw new XPathException(
              "Failed to convert the " + role.getMessage + ": " + "Cannot promote xs:double value to xs:float",
              "XPTY0004")
          } else if (item.isInstanceOf[NumericValue]) {
            Converter
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
      if (requiredItemType == BuiltInAtomicType.STRING &&
        relationship(suppliedItemType, BuiltInAtomicType.ANY_URI) !=
          DISJOINT) {
        val promoter: ItemMappingFunction = (item) =>
          if (item.isInstanceOf[AnyURIValue]) {
            new StringValue(item.getStringValueCS)
          } else {
            item
          }
        iterator = new ItemMappingIterator(iterator, promoter, true)
      }
    }
    iterator = applyFunctionCoercion(iterator,
      suppliedItemType,
      requiredItemType,
      locator)
    val relation: Affinity = relationship(suppliedItemType, requiredItemType)
    if (!(relation == SAME_TYPE || relation == SUBSUMED_BY)) {
      val itemChecker: ItemTypeCheckingFunction =
        new ItemTypeCheckingFunction(requiredItemType, role, locator, config)
      iterator = new ItemMappingIterator(iterator, itemChecker, true)
    }
    if (requiredType.getCardinality != StaticProperty.ALLOWS_ZERO_OR_MORE) {
      iterator = new CardinalityCheckingIterator(iterator,
        requiredType.getCardinality,
        role,
        locator)
    }
    SequenceTool.toMemoSequence(iterator)
  }

  def applyFunctionCoercion(iterator: SequenceIterator,
                            suppliedItemType: ItemType,
                            requiredItemType: ItemType,
                            locator: Location): SequenceIterator =
    if (requiredItemType.isInstanceOf[FunctionItemType] &&
      !requiredItemType.asInstanceOf[FunctionItemType].isMapType &&
      !requiredItemType.asInstanceOf[FunctionItemType].isArrayType &&
      !(relationship(requiredItemType, suppliedItemType) == Affinity.SUBSUMES)) {
      if (requiredItemType == AnyFunctionType.getInstance) {
        iterator
      } else {
        val coercer: FunctionSequenceCoercer.Coercer =
          new FunctionSequenceCoercer.Coercer(
            requiredItemType.asInstanceOf[SpecificFunctionType],
            config,
            locator)
        new ItemMappingIterator(iterator, coercer, true)
      }
    } else {
      iterator
    }

  def getConfiguration(): Configuration = config

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
    } else if (itmTyp2.isInstanceOf[AnyItemType]) {
      SUBSUMED_BY
    } else if (itmTyp1.isInstanceOf[AnyItemType]) {
      SUBSUMES
    } else if (itmTyp1.isInstanceOf[BuiltInAtomicType] && itmTyp2
      .isInstanceOf[BuiltInAtomicType]) {
      if (itmTyp1.getBasicAlphaCode.startsWith(itmTyp2.getBasicAlphaCode)) {
        SUBSUMED_BY
      } else if (itmTyp2.getBasicAlphaCode.startsWith(itmTyp1.getBasicAlphaCode)) {
        SUBSUMES
      } else {
        DISJOINT
      }
    } else if (itmTyp1.isInstanceOf[ErrorType]) {
      SUBSUMED_BY
    } else if (itmTyp2.isInstanceOf[ErrorType]) {
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

  private def computeRelationship(t1: ItemType, t2: ItemType): Affinity = {
    requireTrueItemType(t1)
    requireTrueItemType(t2)
    try {
      if (t1 == t2) {
        SAME_TYPE
      } else if (t1.isInstanceOf[AnyItemType]) {
        if (t2.isInstanceOf[AnyItemType]) {
          SAME_TYPE
        } else {
          SUBSUMES
        }
      } else if (t2.isInstanceOf[AnyItemType]) {
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
          } else if (t2.isInstanceOf[ErrorType]) {
            DISJOINT
          } else {
            var nodeKindRelationship: Affinity = null
            val m1: UType = t1.getUType
            val m2: UType = t2.getUType
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
            if (t1.isInstanceOf[QNameTest] && t2.isInstanceOf[QNameTest]) {
              nodeNameRelationship = nameTestRelationship(
                t1.asInstanceOf[QNameTest],
                t2.asInstanceOf[QNameTest])
            } else if (on1.isPresent && on1.get
              .isInstanceOf[IntUniversalSet]) {
              nodeNameRelationship =
                if (on2.isPresent && on2.get.isInstanceOf[IntUniversalSet])
                  SAME_TYPE
                else SUBSUMES
            } else if (on2.isPresent && on2.get
              .isInstanceOf[IntUniversalSet]) {
              nodeNameRelationship = SUBSUMED_BY
            } else if (!(on1.isPresent && on2.isPresent)) {
              nodeNameRelationship = if (t1 == t2) SAME_TYPE else OVERLAPS
            } else {
              val n1: IntSet = on1.get
              val n2: IntSet = on2.get
              nodeNameRelationship =
                if (n1.containsAll(n2))
                  if (n1.size == n2.size) SAME_TYPE else SUBSUMES
                else if (n2.containsAll(n1)) SUBSUMED_BY
                else if (IntHashSet.containsSome(n1, n2)) OVERLAPS
                else DISJOINT
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
      } else if (t1.isInstanceOf[AnyExternalObjectType]) {
        if (!(t2.isInstanceOf[AnyExternalObjectType])) {
          return DISJOINT
        }
        if (t1.isInstanceOf[JavaExternalObjectType]) {
          if (t2 == AnyExternalObjectType.THE_INSTANCE) {
            SUBSUMED_BY
          } else if (t2.isInstanceOf[JavaExternalObjectType]) {
            t1.asInstanceOf[JavaExternalObjectType]
              .getRelationship(t2.asInstanceOf[JavaExternalObjectType])
          } else {
            DISJOINT
          }
        } else if (t2.isInstanceOf[JavaExternalObjectType]) {
          SUBSUMES
        } else {
          DISJOINT
        }
      } else {
        if (t1.isInstanceOf[MapType] && t2.isInstanceOf[MapType]) {
          if (t1 == MapType.EMPTY_MAP_TYPE) {
            return SUBSUMED_BY
          } else if (t2 == MapType.EMPTY_MAP_TYPE) {
            return SUBSUMES
          } else if (t1 == MapType.ANY_MAP_TYPE) {
            return SUBSUMES
          } else if (t2 == MapType.ANY_MAP_TYPE) {
            return SUBSUMED_BY
          }

          val k1: AtomicType = t1.asInstanceOf[MapType].getKeyType
          val k2: AtomicType = t2.asInstanceOf[MapType].getKeyType
          val v1: SequenceType = t1.asInstanceOf[MapType].getValueType
          val v2: SequenceType = t2.asInstanceOf[MapType].getValueType
          val keyRel: Affinity = relationship(k1, k2)
          val valueRel: Affinity = sequenceTypeRelationship(v1, v2)
          val rel: Affinity = combineRelationships(keyRel, valueRel)
          if (rel == SAME_TYPE || rel == SUBSUMES || rel == SUBSUMED_BY) {
            return rel
          }
        }
        if (t2.isInstanceOf[FunctionItemType]) {

          val signatureRelationship =
            t1.asInstanceOf[FunctionItemType].relationship(t2.asInstanceOf[FunctionItemType], this)

          if (signatureRelationship == DISJOINT) {
            DISJOINT
          } else {
            var assertionRelationship = SAME_TYPE
            val first =
              t1.asInstanceOf[FunctionItemType].getAnnotationAssertions
            val second =
              t2.asInstanceOf[FunctionItemType].getAnnotationAssertions
            val namespaces: Set[String] = new HashSet[String]()
            for (a <- first.asScala) {
              namespaces.add(a.getAnnotationQName.getURI)
            }
            for (a <- second.asScala) {
              namespaces.add(a.getAnnotationQName.getURI)
            }
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
                    if (secondFiltered.isEmpty) SUBSUMED_BY
                    else handler.relationship(firstFiltered, secondFiltered)
                }
                assertionRelationship =
                  combineRelationships(assertionRelationship, localRel)
              }
            }
            combineRelationships(signatureRelationship, assertionRelationship)
          }
        } else {
          DISJOINT
        }
      }
    } catch {
      case _: MissingComponentException => OVERLAPS
    }
  }

  private def unionSubsumes(s1: Set[_ <: PlainType],
                            s2: Set[_ <: PlainType]): Boolean = {
    for (t2 <- s2.asScala) {
      var t2isSubsumed: Boolean = false
      breakable {
        for (t1 <- s1.asScala) {
          val rel: Affinity = relationship(t1, t2)
          if (rel == SUBSUMES || rel == SAME_TYPE) {
            t2isSubsumed = true
            break()
          }
        }
      }
      if (!t2isSubsumed) {
        false
      }
    }
    true
  }

  private def unionOverlaps(s1: Set[_ <: PlainType],
                            s2: Set[_ <: PlainType]): Boolean = {
    for (t2 <- s2.asScala; t1 <- s1.asScala) {
      val rel: Affinity = relationship(t1, t2)
      if (rel != DISJOINT) {
        true
      }
    }
    false
  }

  def computeContentRelationship(t1: ItemType,
                                 t2: ItemType,
                                 n1: Optional[IntSet],
                                 n2: Optional[IntSet]): Affinity = {
    var contentRelationship: Affinity = null
    if (t1.isInstanceOf[DocumentNodeTest]) {
      contentRelationship =
        if (t2.isInstanceOf[DocumentNodeTest])
          relationship(t1.asInstanceOf[DocumentNodeTest].getElementTest,
            t2.asInstanceOf[DocumentNodeTest].getElementTest)
        else SUBSUMED_BY
    } else if (t2.isInstanceOf[DocumentNodeTest]) {
      contentRelationship = SUBSUMES
    } else {
      val s1: SchemaType = t1.asInstanceOf[NodeTest].getContentType
      val s2: SchemaType = t2.asInstanceOf[NodeTest].getContentType
      contentRelationship = schemaTypeRelationship(s1, s2)
    }
    val nillable1: Boolean = t1.asInstanceOf[NodeTest].isNillable
    val nillable2: Boolean = t2.asInstanceOf[NodeTest].isNillable
    if (nillable1 != nillable2) {
      contentRelationship match {
        case SUBSUMES =>
          if (nillable2) {
            contentRelationship = OVERLAPS
          }
        case SUBSUMED_BY =>
          if (nillable1) {
            contentRelationship = OVERLAPS
          }
        case SAME_TYPE =>
          contentRelationship = if (nillable1) SUBSUMES else SUBSUMED_BY
        case _ =>

      }
    }
    contentRelationship
  }

  def sequenceTypeRelationship(s1: SequenceType, s2: SequenceType): Affinity = {
    val c1: Int = s1.getCardinality
    val c2: Int = s2.getCardinality
    var cardRel =
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
    } else if (s1.isInstanceOf[PlainType] && s1.asInstanceOf[PlainType].isPlainType && s2.isInstanceOf[PlainType] && s2.asInstanceOf[PlainType].isPlainType) {
      relationship(s1.asInstanceOf[ItemType], s2.asInstanceOf[ItemType])
    } else {
      var t1 = s1
      while (({
        t1 = t1.getBaseType
        t1
      }) != null)
        if (t1.isSameType(s2)) {
          return SUBSUMED_BY
        }
      var t2 = s2
      while (({
        t2 = t2.getBaseType
        t2
      }) != null)
        if (t2.isSameType(s1)) {
          return SUBSUMES
        }

      DISJOINT
    }
  }

  def getGenericFunctionItemType(): ItemType = AnyItemType.getInstance

}
