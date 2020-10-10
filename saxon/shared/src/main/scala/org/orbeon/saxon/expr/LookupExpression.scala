////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.ma.arrays.ArrayFunctionSet

import org.orbeon.saxon.ma.arrays.ArrayItem

import org.orbeon.saxon.ma.arrays.ArrayItemType

import org.orbeon.saxon.ma.map.MapItem

import org.orbeon.saxon.ma.map.MapType

import org.orbeon.saxon.ma.map.TupleItemType

import org.orbeon.saxon.ma.map.TupleType

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NameChecker

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.value._

import LookupExpression._


object LookupExpression {

  def mustBeArrayOrMap(exp: Expression, baseItem: Item): Unit = {
    val exception: XPathException = new XPathException(
      "The items on the LHS of the '?' operator must be maps or arrays; but value (" +
        baseItem.toShortString +
        ") was supplied",
      "XPTY0004")
    exception.setIsTypeError(true)
    exception.setLocation(exp.getLocation)
    exception.setFailingExpression(exp)
    throw exception
  }

}

class LookupExpression(start: Expression, step: Expression)
  extends BinaryExpression(start, Token.QMARK, step) {

  private var isClassified: Boolean = false

  var isArrayLookup: Boolean = false

  var isMapLookup: Boolean = false

  var isSingleContainer: Boolean = false

  var isSingleEntry: Boolean = false

  override def getOperandRole(arg: Int): OperandRole =
    if (arg == 0) OperandRole.INSPECT else OperandRole.ABSORB

  override def getExpressionName: String = "lookupExp"

  /*@NotNull*/

  def getItemType: ItemType = {
    if (isClassified) {
      if (isArrayLookup) {
        val arrayType: ItemType = getLhsExpression.getItemType
        if (arrayType.isInstanceOf[ArrayItemType]) {
          arrayType.asInstanceOf[ArrayItemType].getMemberType.getPrimaryType
        }
      } else if (isMapLookup) {
        val mapType: ItemType = getLhsExpression.getItemType
        if (mapType.isInstanceOf[TupleItemType] && getRhsExpression
          .isInstanceOf[StringLiteral]) {
          val fieldName: String =
            getRhsExpression.asInstanceOf[StringLiteral].getStringValue
          val fieldType: SequenceType =
            mapType.asInstanceOf[TupleItemType].getFieldType(fieldName)
          if (fieldType == null) {
            if (mapType.asInstanceOf[TupleItemType].isExtensible)
              AnyItemType
            else ErrorType
          } else {
            fieldType.getPrimaryType
          }
        } else if (mapType.isInstanceOf[MapType]) {
          mapType.asInstanceOf[MapType].getValueType.getPrimaryType
        }
      }
    }
    AnyItemType
  }

  /**
   * Get the static type of the expression as a UType, following precisely the type
   * inference rules defined in the XSLT 3.0 specification.
   *
   * @param contextItemType not used
   * @return the static item type of the expression according to the XSLT 3.0 defined rules
   */
  override def getStaticUType(contextItemType: UType): UType =
    getItemType.getUType

  /*@NotNull*/

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
    // with a tuple type. So check this first.
    val originalType: ItemType = getLhsExpression.getItemType
    // Check the first operand
    getLhs.typeCheck(visitor, contextInfo)
    val containerType: ItemType = getLhsExpression.getItemType
    isArrayLookup = containerType.isInstanceOf[ArrayItemType]
    val isTupleLookup: Boolean = containerType
      .isInstanceOf[TupleType] || originalType.isInstanceOf[TupleType]
    isMapLookup = containerType.isInstanceOf[MapType] || isTupleLookup
    if (containerType.isInstanceOf[AnyExternalObjectType]) {
      config.checkLicensedFeature(
        Configuration.LicenseFeature.PROFESSIONAL_EDITION,
        "use of lookup expressions on external objects",
        -1)
      config
        .makeObjectLookupExpression(getLhsExpression, getRhsExpression)
        .typeCheck(visitor, contextInfo)
    }
    isSingleContainer = getLhsExpression.getCardinality == StaticProperty.EXACTLY_ONE
    if (!isArrayLookup && !isMapLookup) {
      if (th.relationship(containerType, MapType.ANY_MAP_TYPE) ==
        Affinity.DISJOINT &&
        th.relationship(containerType, AnyItemType) ==
          Affinity.DISJOINT &&
        th.relationship(containerType, AnyExternalObjectType.THE_INSTANCE) ==
          Affinity.DISJOINT) {
        val err = new XPathException(
          "The left-hand operand of '?' must be a map or an array; the supplied expression is of type " +
            containerType,
          "XPTY0004")
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        err.setFailingExpression(this)
        throw err
      }
    }
    getRhs.typeCheck(visitor, contextInfo)
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "?", 1)
    val tc: TypeChecker = config.getTypeChecker(false)
    var req: SequenceType = BuiltInAtomicType.ANY_ATOMIC.zeroOrMore
    if (isArrayLookup) {
      req = BuiltInAtomicType.INTEGER.zeroOrMore
    }
    this.setRhsExpression(
      tc.staticTypeCheck(getRhsExpression, req, role, visitor))
    isSingleEntry = getRhsExpression.getCardinality == StaticProperty.EXACTLY_ONE
    if (isTupleLookup && getRhsExpression.isInstanceOf[StringLiteral]) {
      val tt: TupleType =
        (if (containerType.isInstanceOf[TupleType]) containerType
        else originalType).asInstanceOf[TupleType]
      if (!tt.isExtensible) {
        val fieldName: String =
          getRhsExpression.asInstanceOf[StringLiteral].getStringValue
        if (tt.getFieldType(fieldName) == null) {
          val err = new XPathException(
            "Field " + fieldName + " is not defined in the tuple type",
            "XPTY0004")
          err.setIsTypeError(true)
          err.setLocation(getLocation)
          throw err
        }
      }
    }
    isClassified = true
    this
  }

  // Running typeCheck on the first operand can lose static type information if it's declared
  // Now check the second operand
  // Running typeCheck on the first operand can lose static type information if it's declared
  // Now check the second operand

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.optimize(visitor, contextInfo)
    getRhs.optimize(visitor, contextInfo)
    this
  }

  /**
   * Return the estimated cost of evaluating an expression. This is a very crude measure based
   * on the syntactic form of the expression (we have no knowledge of data values). We take
   * the cost of evaluating a simple scalar comparison or arithmetic expression as 1 (one),
   * and we assume that a sequence has length 5. The resulting estimates may be used, for
   * example, to reorder the predicates in a filter expression so cheaper predicates are
   * evaluated first.
   *
   * @return a rough estimate of the cost of evaluation
   */
  override def getCost: Double =
    getLhsExpression.getCost * getRhsExpression.getCost

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *         { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  /*@NotNull*/

  def copy(rebindings: RebindingMap): LookupExpression = {
    val exp: LookupExpression = new LookupExpression(
      getLhsExpression.copy(rebindings),
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp.isArrayLookup = isArrayLookup
    exp.isMapLookup = isMapLookup
    exp.isSingleEntry = isSingleEntry
    exp.isSingleContainer = isSingleContainer
    exp
  }

  override def computeCardinality(): Int = {
    if (isSingleContainer && isSingleEntry) {
      if (isArrayLookup) {
        val arrayType: ItemType = getLhsExpression.getItemType
        if (arrayType.isInstanceOf[ArrayItemType]) {
          arrayType.asInstanceOf[ArrayItemType].getMemberType.getCardinality
        }
      } else if (isMapLookup) {
        val mapType: ItemType = getLhsExpression.getItemType
        if (mapType.isInstanceOf[TupleItemType] && getRhsExpression
          .isInstanceOf[StringLiteral]) {
          val fieldName: String =
            getRhsExpression.asInstanceOf[StringLiteral].getStringValue
          val fieldType: SequenceType =
            mapType.asInstanceOf[TupleItemType].getFieldType(fieldName)
          if (fieldType == null) {
            if (mapType.asInstanceOf[TupleItemType].isExtensible)
              StaticProperty.ALLOWS_ZERO_OR_MORE
            else StaticProperty.ALLOWS_ZERO
          } else {
            fieldType.getCardinality
          }
        } else if (mapType.isInstanceOf[MapType]) {
          (Cardinality.union(
            mapType.asInstanceOf[MapType].getValueType.getCardinality,
            StaticProperty.ALLOWS_ZERO))
        }
      }
    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[LookupExpression])) {
      return false
    }
    val p: LookupExpression = other.asInstanceOf[LookupExpression]
    getLhsExpression.isEqual(p.getLhsExpression) && getRhsExpression.isEqual(
      p.getRhsExpression)
  }

  override def computeHashCode(): Int =
    "LookupExpression".hashCode ^ getLhsExpression.hashCode ^
      getRhsExpression.hashCode

  /*@NotNull*/

  override def iterate(context: XPathContext): SequenceIterator = {
    val config: Configuration = context.getConfiguration
    if (isArrayLookup) {
      if (isSingleContainer && isSingleEntry) {
        val array: ArrayItem =
          getLhsExpression.evaluateItem(context).asInstanceOf[ArrayItem]
        val subscript: IntegerValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[IntegerValue]
        val index: Int =
          ArrayFunctionSet.checkSubscript(subscript, array.arrayLength())
        array.get(index - 1).iterate()
      } else if (isSingleEntry) {
        val baseIterator: SequenceIterator = getLhsExpression.iterate(context)
        val subscriptValue: IntegerValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[IntegerValue]
        val subscript: Int = subscriptValue.asSubscript() - 1
        new MappingIterator(
          baseIterator,
          (baseItem) => {
            val array: ArrayItem = baseItem.asInstanceOf[ArrayItem]
            if (subscript >= 0 && subscript < array.arrayLength()) {
              array.get(subscript).iterate()
            } else {
              ArrayFunctionSet.checkSubscript(subscriptValue,
                array.arrayLength())
              null
            }
          }
        )
        // reuse the diagnostic logic
        // shouldn't happen
      } else {
        val baseIterator: SequenceIterator = getLhsExpression.iterate(context)
        val rhs: GroundedValue =
          getRhsExpression.iterate(context).materialize()
        new MappingIterator(
          baseIterator,
          (baseItem) =>
            new MappingIterator(
              rhs.iterate(),
              (index) => {
                val array: ArrayItem = baseItem.asInstanceOf[ArrayItem]
                val subscript: Int = ArrayFunctionSet.checkSubscript(
                  index.asInstanceOf[IntegerValue],
                  array.arrayLength()) -
                  1
                array.get(subscript).iterate()
              }
            )
        )
      }
    } else if (isMapLookup) {
      if (isSingleContainer && isSingleEntry) {
        val map: MapItem =
          getLhsExpression.evaluateItem(context).asInstanceOf[MapItem]
        val key: AtomicValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        val value: GroundedValue = map.get(key)
        if (value == null) EmptyIterator.emptyIterator else value.iterate()
      } else if (isSingleEntry) {
        val baseIterator: SequenceIterator = getLhsExpression.iterate(context)
        val key: AtomicValue =
          getRhsExpression.evaluateItem(context).asInstanceOf[AtomicValue]
        new MappingIterator(
          baseIterator,
          (baseItem) => {
            val value: GroundedValue = baseItem.asInstanceOf[MapItem].get(key)
            if (value == null) EmptyIterator.emptyIterator
            else value.iterate()
          }
        )
      } else {
        val baseIterator: SequenceIterator = getLhsExpression.iterate(context)
        val rhs: GroundedValue =
          getRhsExpression.iterate(context).materialize()
        new MappingIterator(
          baseIterator,
          (baseItem) =>
            new MappingIterator(
              rhs.iterate(),
              (index) => {
                val value: GroundedValue = baseItem
                  .asInstanceOf[MapItem]
                  .get(index.asInstanceOf[AtomicValue])
                if (value == null) EmptyIterator.emptyIterator
                else value.iterate()
              }
            )
        )
      }
    } else {
      val baseIterator: SequenceIterator = getLhsExpression.iterate(context)
      val rhs: GroundedValue = getRhsExpression.iterate(context).materialize()
      val mappingFunction: MappingFunction = (baseItem) =>
        if (baseItem.isInstanceOf[ArrayItem]) {
          var arrayAccess: MappingFunction = (index) =>
            if (index.isInstanceOf[IntegerValue]) {
              var member: GroundedValue = baseItem
                .asInstanceOf[ArrayItem]
                .get(index.asInstanceOf[IntegerValue].longValue().toInt - 1)
              member.iterate()
            } else {
              var exception: XPathException = new XPathException(
                "An item on the LHS of the '?' operator is an array, but a value on the RHS of the operator (" +
                  baseItem.toShortString +
                  ") is not an integer",
                "XPTY0004"
              )
              exception.setIsTypeError(true)
              exception.setLocation(getLocation)
              exception.setFailingExpression(LookupExpression.this)
              throw exception
            }
          var rhsIter: SequenceIterator = rhs.iterate()
          new MappingIterator(rhsIter, arrayAccess)
        } else if (baseItem.isInstanceOf[MapItem]) {
          var rhsIter: SequenceIterator = rhs.iterate()
          new MappingIterator(
            rhsIter,
            (key) => {
              var value: GroundedValue = baseItem
                .asInstanceOf[MapItem]
                .get(key.asInstanceOf[AtomicValue])
              if (value == null) EmptyIterator.emptyIterator
              else value.iterate()
            }
          )
        } else if (baseItem.isInstanceOf[ObjectValue[_]]) {
          if (!rhs.isInstanceOf[StringValue]) {
            var exception: XPathException = new XPathException(
              "An item on the LHS of the '?' operator is an external object, but a value on the RHS of the operator (" +
                baseItem.toShortString +
                ") is not a singleton string",
              "XPTY0004"
            )
            exception.setIsTypeError(true)
            exception.setLocation(getLocation)
            exception.setFailingExpression(LookupExpression.this)
            throw exception
          }
          var key: String = rhs.getStringValue
          config
            .externalObjectAsMap(baseItem.asInstanceOf[ObjectValue[_]], key)
            .get(rhs.asInstanceOf[StringValue])
            .iterate()
        } else {
          mustBeArrayOrMap(this, baseItem)
          null
        }
      new MappingIterator(baseIterator, mappingFunction)
    }
  }

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("lookup", this)
    getLhsExpression.export(destination)
    getRhsExpression.export(destination)
    destination.endElement()
  }

  override def toString: String = {
    var rhs: String = null
    if (getRhsExpression.isInstanceOf[Literal]) {
      val lit: Literal = getRhsExpression.asInstanceOf[Literal]
      rhs =
        if (lit.isInstanceOf[StringLiteral] &&
          NameChecker.isValidNCName(
            lit.asInstanceOf[StringLiteral].getStringValue))
          lit.asInstanceOf[StringLiteral].getStringValue
        else if (lit.getValue.isInstanceOf[Int64Value]) lit.getValue.toString
        else ExpressionTool.parenthesize(lit)
    } else {
      rhs = ExpressionTool.parenthesize(getRhsExpression)
    }
    ExpressionTool.parenthesize(getLhsExpression) + "?" +
      rhs
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A lookup expression is an expression of the form A?B. Here A must be a sequence of maps or arrays.
 * In the general case B is an expression that computes a key/index into the map or array; the case where
 * B is constant needs to be handled efficiently. The class also implements the unary lookup expression
 * ?B, which is interpreted as .?B. It does not handle the case A?* - that is handled as a LookupAllExpression.
 */
