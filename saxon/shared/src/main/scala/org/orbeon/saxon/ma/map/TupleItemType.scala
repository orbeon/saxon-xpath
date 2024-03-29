package org.orbeon.saxon.ma.map

import java.util._

import org.orbeon.saxon.expr.{Expression, StaticProperty}
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Genre, GroundedValue, Item, Sequence}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value._

import scala.beans.BooleanBeanProperty

//import scala.collection.compat._
import org.orbeon.saxon.model.Affinity._

import scala.jdk.CollectionConverters._

class TupleItemType(names: List[String],
                    types: List[SequenceType],
                    @BooleanBeanProperty var extensible: Boolean)
  extends AnyFunctionType
    with TupleType {

  private var fields: Map[String, SequenceType] = new HashMap()

  for (i <- 0 until names.size) {
    fields.put(names.get(i), types.get(i))
  }

  override def getGenre: Genre.Genre = Genre.MAP

  override def isMapType: Boolean = true

  override def isArrayType: Boolean = false

  override def getFieldNames: java.lang.Iterable[String] = fields.keySet

  def getFieldType(field: String): SequenceType = fields.get(field)

  override def matches(item: Item, th: TypeHierarchy): Boolean = {
    if (! item.isInstanceOf[MapItem])
      return false
    val map: MapItem = item.asInstanceOf[MapItem]
    for ((key, value) <- fields.asScala) {
      var `val`: Sequence = map.get(new StringValue(key))
      if (`val` == null) {
        `val` = EmptySequence.getInstance
      }
      if (!value.matches(`val`, th)) {
        return false
      }
    }
    if (!extensible) {
      val keyIter: AtomicIterator[_ <: AtomicValue] = map.keys
      var key: AtomicValue = null
      while (({
        key = keyIter.next()
        key
      }) != null) if (!(key.isInstanceOf[
        StringValue]) || !fields
        .containsKey(
          key.getStringValue)) {
       return false
      }
    }
    true
  }

  def getArity: Int = 1

  override def getArgumentTypes: Array[SequenceType] =
    Array(SequenceType.SINGLE_ATOMIC)

  override def getResultType: SequenceType =
    if (extensible) {
      SequenceType.ANY_SEQUENCE
    } else {
      var resultType: ItemType = null
      var allowsMany: Boolean = false
      for ((key, value) <- fields.asScala) {
        resultType =
          if (resultType == null) value.getPrimaryType
          else Type.getCommonSuperType(resultType, value.getPrimaryType)
        allowsMany = allowsMany || Cardinality.allowsMany(value.getCardinality)
      }
      SequenceType.makeSequenceType(resultType,
        if (allowsMany)
          StaticProperty.ALLOWS_ZERO_OR_MORE
        else StaticProperty.ALLOWS_ZERO_OR_ONE)
    }

  override def getDefaultPriority: Double = {
    var prio: Double = 1
    for (st <- fields.values.asScala) {
      prio *= st.getPrimaryType.getNormalizedDefaultPriority
    }
    if (extensible) 0.5 + prio / 2 else prio
  }

  override def toString: String = makeString((seq: SequenceType) => seq.toString)

  override def toExportString: String =
    makeString((seq: SequenceType) => seq.toExportString)

  override def getBasicAlphaCode: String = "FM"

  private def makeString(show: SequenceType => String): String = {
    val sb = new FastStringBuffer(100)
    sb.append("tuple(")
    var first: Boolean = true
    for ((key, value) <- fields.asScala) {
      if (first) {
        first = false
      } else {
        sb.append(", ")
      }
      sb.append(key)
      sb.append(": ")
      sb.append(show.apply(value))
    }
    if (isExtensible) {
      sb.append(", *")
    }
    sb.append(")")
    sb.toString
  }

  override def equals(other: Any): Boolean =
    other match {
      case o: TupleItemType if this eq o => true
      case o: TupleItemType => extensible == o.extensible && fields == o.fields
      case _ => false
    }

  override def hashCode: Int = fields.hashCode

  override def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity.Affinity =
    if (other == AnyFunctionType) {
      Affinity.SUBSUMED_BY
    } else if (other.isInstanceOf[TupleItemType]) {
      tupleTypeRelationship(other.asInstanceOf[TupleItemType], th)
    } else if (other == MapType.ANY_MAP_TYPE) {
      Affinity.SUBSUMED_BY
    } else if (other.isArrayType) {
      Affinity.DISJOINT
    } else if (other.isInstanceOf[MapType]) {
      tupleToMapRelationship(other.asInstanceOf[MapType], th)
    } else {
      var rel: Affinity.Affinity = null
      rel = new SpecificFunctionType(getArgumentTypes, getResultType)
        .relationship(other, th)
      rel
    }

  private def tupleToMapRelationship(other: MapType,
                                     th: TypeHierarchy): Affinity = {
    val tupleKeyType: AtomicType =
      if (isExtensible) BuiltInAtomicType.ANY_ATOMIC
      else BuiltInAtomicType.STRING
    val keyRel: Affinity = th.relationship(tupleKeyType, other.getKeyType)
    if (keyRel == Affinity.DISJOINT) {
     return Affinity.DISJOINT
    }
    if (other.getValueType.getPrimaryType == AnyItemType &&
      other.getValueType.getCardinality == StaticProperty.ALLOWS_ZERO_OR_MORE) {
      if (keyRel == Affinity.SUBSUMED_BY || keyRel == Affinity.SAME_TYPE) {
        Affinity.SUBSUMED_BY
      } else {
        Affinity.OVERLAPS
      }
    } else if (isExtensible) {
      Affinity.OVERLAPS
    } else {
      for (entry <- fields.values.asScala) {
        val rel: Affinity =
          th.sequenceTypeRelationship(entry, other.getValueType)
        if (!(rel == Affinity.SUBSUMED_BY || rel == Affinity.SAME_TYPE)) {
          return Affinity.OVERLAPS
        }
      }
      Affinity.SUBSUMED_BY
    }
  }

  private def tupleTypeRelationship(other: TupleItemType,
                                    th: TypeHierarchy): Affinity.Affinity = {
    val keys: Set[String] = new HashSet[String](fields.keySet)
    keys.addAll(other.fields.keySet)
    var foundSubsuming: Boolean = false
    var foundSubsumed: Boolean = false
    var foundOverlap: Boolean = false
    if (isExtensible) {
      if (!other.isExtensible) {
        foundSubsuming = true
      }
    } else if (other.isExtensible) {
      foundSubsumed = true
    }
    for (key <- keys.asScala) {
      val t1: SequenceType = fields.get(key)
      val t2: SequenceType = other.fields.get(key)
      if (t1 == null) {
        if (isExtensible) {
          foundSubsuming = true
        } else if (Cardinality.allowsZero(t2.getCardinality)) {
          foundOverlap = true
        } else {
         return Affinity.DISJOINT
        }
      } else if (t2 == null) {
        if (other.isExtensible) {
          foundSubsumed = true
        } else if (Cardinality.allowsZero(t1.getCardinality)) {
          foundOverlap = true
        } else {
          return Affinity.DISJOINT
        }
      } else {
        val a: Affinity = th.sequenceTypeRelationship(t1, t2)
        a match {
          case SAME_TYPE =>
          case SUBSUMED_BY => foundSubsumed = true
          case SUBSUMES => foundSubsuming = true
          case OVERLAPS => foundOverlap = true
          case DISJOINT => return Affinity.DISJOINT

        }
      }
    }
    if (foundOverlap || (foundSubsumed && foundSubsuming)) {
      Affinity.OVERLAPS
    } else if (foundSubsuming) {
      Affinity.SUBSUMES
    } else if (foundSubsumed) {
      Affinity.SUBSUMED_BY
    } else {
      Affinity.SAME_TYPE
    }
  }

  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = {
    if (item.isInstanceOf[MapItem]) {
      for ((mapKey, value) <- fields.asScala) {
        val key: String = mapKey
        val required: SequenceType = value
        val groundValue: GroundedValue =
          item.asInstanceOf[MapItem].get(new StringValue(key))
        if (value == null) {
          if (!Cardinality.allowsZero(required.getCardinality)) {
            return Some("Field " + key + " is absent; it must have a value")
          }
        } else {
          try if (!required.matches(groundValue, th)) {
            var s = "Field " + key + " has value " + Err
              .depictSequence(groundValue) +
              " which does not match the required type " +
              required.toString
            val more = required.explainMismatch(groundValue, th)
            if (more.isDefined) {
              s += ". " + more.get
            }
            return Some(s)
          } catch {
            case err: XPathException => return None

          }
        }
      }
      if (!extensible) {
        val keyIter: AtomicIterator[_ <: AtomicValue] =
          item.asInstanceOf[MapItem].keys
        var key: AtomicValue = null
        while (({
          key = keyIter.next()
          key
        }) != null) if (!(key.isInstanceOf[
          StringValue])) {
          return Some(
            "Undeclared field " + key +
              " is present, but it is not a string, and the tuple type is not extensible")
        } else if (!fields.containsKey(key.getStringValue)) {
          return Some(
            "Undeclared field " + key +
              " is present, but the tuple type is not extensible")
        }
      }
    }
    None
  }

  override def makeFunctionSequenceCoercer(exp: Expression,
                                           role: RoleDiagnostic): Expression =
    new SpecificFunctionType(getArgumentTypes, getResultType)
      .makeFunctionSequenceCoercer(exp, role)

}
