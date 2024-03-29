////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited

package org.orbeon.saxon.ma.map

import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.expr.{Expression, StaticProperty}
import org.orbeon.saxon.ma.map.MapType._
import org.orbeon.saxon.model.Affinity.Affinity
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.{Genre, Item}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{Cardinality, SequenceType}

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object MapType {

  val ANY_MAP_TYPE: MapType =
    new MapType(BuiltInAtomicType.ANY_ATOMIC, SequenceType.ANY_SEQUENCE)

  // The type of a map with no entries. It's handled specially in some static type inferencing rules
  val EMPTY_MAP_TYPE: MapType =
    new MapType(BuiltInAtomicType.ANY_ATOMIC, SequenceType.ANY_SEQUENCE, true)

  /**
    * A type that allows a sequence of zero or one map items
    */ /**
    * A type that allows a sequence of zero or one map items
    */
  val OPTIONAL_MAP_ITEM: SequenceType = SequenceType.makeSequenceType(
    ANY_MAP_TYPE,
    StaticProperty.ALLOWS_ZERO_OR_ONE)

  val SINGLE_MAP_ITEM: SequenceType =
    SequenceType.makeSequenceType(ANY_MAP_TYPE, StaticProperty.ALLOWS_ONE)

  val SEQUENCE_OF_MAPS: SequenceType = SequenceType.makeSequenceType(
    ANY_MAP_TYPE,
    StaticProperty.ALLOWS_ZERO_OR_MORE)

}

/**
  * An instance of this class represents a specific map item type, for example
  * map(x:integer, element(employee))
  */
class MapType(@BeanProperty var keyType: AtomicType,
              @BeanProperty var valueType: SequenceType)
  extends AnyFunctionType {

  private var mustBeEmpty: Boolean = false

  def this(keyType: AtomicType,
           valueType: SequenceType,
           mustBeEmpty: Boolean) = {
    this(keyType, valueType)
    this.mustBeEmpty = mustBeEmpty
  }

  /**
    * Determine the Genre (top-level classification) of this type
    *
    * @return the Genre to which this type belongs, specifically {@link Genre#MAP}
    */
  override def getGenre: Genre = Genre.MAP

  /**
    * Ask whether this function item type is a map type. In this case function coercion (to the map type)
    * will never succeed.
    *
    * @return true if this FunctionItemType is a map type
    */
  override def isMapType: Boolean = true

  /**
    * Ask whether this function item type is an array type. In this case function coercion (to the array type)
    * will never succeed.
    *
    * @return true if this FunctionItemType is an array type
    */
  override def isArrayType: Boolean = false

  /**
    * Get an alphabetic code representing the type, or at any rate, the nearest built-in type
    * from which this type is derived. The codes are designed so that for any two built-in types
    * A and B, alphaCode(A) is a prefix of alphaCode(B) if and only if A is a supertype of B.
    *
    * @return the alphacode for the nearest containing built-in type
    */
  override def getBasicAlphaCode: String = "FM"

  override def isAtomizable(th: TypeHierarchy): Boolean = false

  /**
    * Get the default priority when this ItemType is used as an XSLT pattern
    *
    * @return the default priority
    */
  override def getDefaultPriority: Double =
    keyType.getNormalizedDefaultPriority *
      valueType.getPrimaryType.getNormalizedDefaultPriority

  /**
    * Test whether a given item conforms to this type
    *
    * @param item The item to be tested
    * @param th
    * @return true if the item is an instance of this type; false otherwise
    */
  override def matches(item: Item, th: TypeHierarchy): Boolean = {
    if (! item.isInstanceOf[MapItem])
      return false
    if (item.asInstanceOf[MapItem].isEmpty) {
      return true
    } else if (mustBeEmpty) {
      return false
    }
    if (this == ANY_MAP_TYPE)
      true
    else
      item.asInstanceOf[MapItem].conforms(keyType, valueType, th)
  }

  def getArity: Int = 1

  override def getArgumentTypes: Array[SequenceType] =
    Array(
      SequenceType.makeSequenceType(BuiltInAtomicType.ANY_ATOMIC,
        StaticProperty.EXACTLY_ONE))

  override def getResultType: SequenceType =
    if (Cardinality.allowsZero(valueType.getCardinality)) {
      valueType
    } else {
      SequenceType.makeSequenceType(
        valueType.getPrimaryType,
        Cardinality.union(valueType.getCardinality,
          StaticProperty.ALLOWS_ZERO))
    }

  /**
    * Produce a representation of this type name for use in error messages.
    *
    * @return a string representation of the type, in notation resembling but not necessarily
    *         identical to XPath syntax
    */
  override def toString: String =
    if (this == ANY_MAP_TYPE) {
      "map(*)"
    } else if (this == EMPTY_MAP_TYPE) {
      "map{}"
    } else {
      val sb = new FastStringBuffer(100)
      sb.append("map(")
      sb.append(keyType.toString)
      sb.append(", ")
      sb.append(valueType.toString)
      sb.append(")")
      sb.toString
    }

  /**
    * Return a string representation of this ItemType suitable for use in stylesheet
    * export files. This differs from the result of toString() in that it will not contain
    * any references to anonymous types. Note that it may also use the Saxon extended syntax
    * for union types and tuple types. The default implementation returns the result of
    * calling {@code #toString()}.
    *
    * @return the string representation as an instance of the XPath SequenceType construct
    */
  override def toExportString: String =
    if (this == ANY_MAP_TYPE) {
      "map(*)"
    } else if (this == EMPTY_MAP_TYPE) {
      "map{}"
    } else {
      val sb = new FastStringBuffer(100)
      sb.append("map(")
      sb.append(keyType.toExportString)
      sb.append(", ")
      sb.append(valueType.toExportString)
      sb.append(")")
      sb.toString
    }

  override def equals(other: Any): Boolean =
    other match {
      case o: MapType if this eq o => true
      case o: MapType => keyType == o.keyType && valueType == o.valueType && mustBeEmpty == o.mustBeEmpty
      case _ => false
    }

  /**
    * Returns a hash code value for the object.
    */
  override def hashCode: Int = keyType.hashCode ^ valueType.hashCode

  override def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity =
    if (other == AnyFunctionType) {
      Affinity.SUBSUMED_BY
    } else if (equals(other)) {
      Affinity.SAME_TYPE
    } else if (other == MapType.ANY_MAP_TYPE) {
      Affinity.SUBSUMED_BY
    } else if (other.isArrayType) {
      Affinity.DISJOINT
    } else if (other.isInstanceOf[TupleItemType]) {
      TypeHierarchy.inverseRelationship(other.relationship(this, th))
    } else if (other.isInstanceOf[MapType]) {
      val f2: MapType = other.asInstanceOf[MapType]
      val keyRel: Affinity = th.relationship(keyType, f2.keyType)
      if (keyRel == Affinity.DISJOINT) {
        return Affinity.OVERLAPS
      }
      val valueRel: Affinity =
        th.sequenceTypeRelationship(valueType, f2.valueType)
      if (valueRel == Affinity.DISJOINT) {
        return Affinity.OVERLAPS
      }
      if (keyRel == valueRel) {
        return keyRel
      }
      if ((keyRel == Affinity.SAME_TYPE || keyRel == Affinity.SUBSUMES) &&
        (valueRel == Affinity.SAME_TYPE || valueRel == Affinity.SUBSUMES)) {
        return Affinity.SUBSUMES
      }
      if ((keyRel == Affinity.SAME_TYPE || keyRel == Affinity.SUBSUMED_BY) &&
        (valueRel == Affinity.SAME_TYPE || valueRel == Affinity.SUBSUMED_BY)) {
        return Affinity.SUBSUMED_BY
      }
      Affinity.OVERLAPS
    } else {
      new SpecificFunctionType(getArgumentTypes, getResultType)
        .relationship(other, th)
    }

  /**
    * Get extra diagnostic information about why a supplied item does not conform to this
    * item type, if available. If extra information is returned, it should be in the form of a complete
    * sentence, minus the closing full stop. No information should be returned for obvious cases.
    *
    * @param item the item that doesn't match this type
    * @param th   the type hierarchy cache
    * @return optionally, a message explaining why the item does not match the type
    */
  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = {
    item match {
      case mapItem: MapItem =>
        for (kvp <- mapItem.keyValuePairs.asScala) {
          if (!keyType.matches(kvp.key, th)) {
            val s = "The map contains a key (" + kvp.key + ") of type " +
              kvp.key.getItemType +
              " that is not an instance of the required type " +
              keyType
           return Some(s)
          }
          try
            if (!valueType.matches(kvp.value, th)) {
              var s = "The map contains an entry with key (" + kvp.key + ") whose corresponding value (" +
                Err.depictSequence(kvp.value) +
                ") is not an instance of the required type " +
                valueType
              val more = valueType.explainMismatch(kvp.value, th)
              if (more.isDefined)
                s = s + ". " + more.get
              return Some(s)
            }
          catch {
            case _: XPathException =>
          }
        }
      case _ =>
    }
    None
  }

  override def makeFunctionSequenceCoercer(exp: Expression,
                                           role: RoleDiagnostic): Expression =
    new SpecificFunctionType(getArgumentTypes, getResultType)
      .makeFunctionSequenceCoercer(exp, role)

}

