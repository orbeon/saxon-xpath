////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException




trait SchemaType extends SchemaComponent {

  /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the derivation by <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#key-typeRestriction'>
    * restriction</a> if complex types are involved, or a <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-restriction'>
    * restriction</a> if simple types are involved.
    * <br>  The reference type definition is derived by restriction from the
    * other type definition if the other type definition is the same as the
    * reference type definition, or if the other type definition can be
    * reached recursively following the {base type definition} property
    * from the reference type definition, and all the <em>derivation methods</em> involved are restriction.
    */ /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the derivation by <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#key-typeRestriction'>
    * restriction</a> if complex types are involved, or a <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-restriction'>
    * restriction</a> if simple types are involved.
    * <br>  The reference type definition is derived by restriction from the
    * other type definition if the other type definition is the same as the
    * reference type definition, or if the other type definition can be
    * reached recursively following the {base type definition} property
    * from the reference type definition, and all the <em>derivation methods</em> involved are restriction.
    */
  var DERIVATION_RESTRICTION: Int = 0x00000001

  /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the derivation by <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#key-typeExtension'>
    * extension</a>.
    * <br>  The reference type definition is derived by extension from the
    * other type definition if the other type definition can be reached
    * recursively following the {base type definition} property from the
    * reference type definition, and at least one of the <em>derivation methods</em> involved is an extension.
    */ /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the derivation by <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#key-typeExtension'>
    * extension</a>.
    * <br>  The reference type definition is derived by extension from the
    * other type definition if the other type definition can be reached
    * recursively following the {base type definition} property from the
    * reference type definition, and at least one of the <em>derivation methods</em> involved is an extension.
    */
  var DERIVATION_EXTENSION: Int = 0x00000002

  /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-union'>
    * union</a> if simple types are involved.
    * <br> The reference type definition is derived by union from the other
    * type definition if there exists two type definitions T1 and T2 such
    * as the reference type definition is derived from T1 by
    * <code>DERIVATION_RESTRICTION</code> or
    * <code>DERIVATION_EXTENSION</code>, T2 is derived from the other type
    * definition by <code>DERIVATION_RESTRICTION</code>, T1 has {variety} <em>union</em>, and one of the {member type definitions} is T2. Note that T1 could be
    * the same as the reference type definition, and T2 could be the same
    * as the other type definition.
    */ /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-union'>
    * union</a> if simple types are involved.
    * <br> The reference type definition is derived by union from the other
    * type definition if there exists two type definitions T1 and T2 such
    * as the reference type definition is derived from T1 by
    * <code>DERIVATION_RESTRICTION</code> or
    * <code>DERIVATION_EXTENSION</code>, T2 is derived from the other type
    * definition by <code>DERIVATION_RESTRICTION</code>, T1 has {variety} <em>union</em>, and one of the {member type definitions} is T2. Note that T1 could be
    * the same as the reference type definition, and T2 could be the same
    * as the other type definition.
    */
  var DERIVATION_UNION: Int = 0x00000004

  /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-list'>list</a>.
    * <br> The reference type definition is derived by list from the other
    * type definition if there exists two type definitions T1 and T2 such
    * as the reference type definition is derived from T1 by
    * <code>DERIVATION_RESTRICTION</code> or
    * <code>DERIVATION_EXTENSION</code>, T2 is derived from the other type
    * definition by <code>DERIVATION_RESTRICTION</code>, T1 has {variety} <em>list</em>, and T2 is the {item type definition}. Note that T1 could be the same as
    * the reference type definition, and T2 could be the same as the other
    * type definition.
    */ /**
    * If the document's schema is an XML Schema [<a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/'>XML Schema Part 1</a>]
    * , this constant represents the <a href='http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/#element-list'>list</a>.
    * <br> The reference type definition is derived by list from the other
    * type definition if there exists two type definitions T1 and T2 such
    * as the reference type definition is derived from T1 by
    * <code>DERIVATION_RESTRICTION</code> or
    * <code>DERIVATION_EXTENSION</code>, T2 is derived from the other type
    * definition by <code>DERIVATION_RESTRICTION</code>, T1 has {variety} <em>list</em>, and T2 is the {item type definition}. Note that T1 could be the same as
    * the reference type definition, and T2 could be the same as the other
    * type definition.
    */
  var DERIVATION_LIST: Int = 0x00000008

  var DERIVE_BY_SUBSTITUTION: Int = 16

  /*@Nullable*/

  def getName: String

  /*@Nullable*/

  def getTargetNamespace: String

  def getFingerprint: Int

  def getDisplayName: String

  def getStructuredQName: StructuredQName

  def getEQName: String

  def isComplexType: Boolean

  def isSimpleType: Boolean

  def isAtomicType: Boolean

  def isAnonymousType: Boolean

  def getBlock: Int

  def getBaseType: SchemaType

  def getNearestNamedType: SchemaType = {
    var `type`: SchemaType = this
    while (`type`.isAnonymousType) `type` = `type`.getBaseType
    `type`
  }

  def getDerivationMethod: Int

  def getFinalProhibitions: Int

  def allowsDerivation(derivation: Int): Boolean

  def analyzeContentExpression(expression: Expression, kind: Int): Unit

  def atomize(node: NodeInfo): AtomicSequence

  def isSameType(other: SchemaType): Boolean

  def getDescription: String

  def checkTypeDerivationIsOK(base: SchemaType, block: Int): Unit

  /*@Nullable*/

  def getSystemId: String

  def isIdType: Boolean

  def isIdRefType: Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * SchemaType is an interface implemented by all schema types: simple and complex types, built-in and
  * user-defined types.
  * <p>There is a hierarchy of interfaces that extend SchemaType, representing the top levels of the schema
  * type system: SimpleType and ComplexType, with SimpleType further subdivided into List, Union, and Atomic
  * types.</p>
  * <p>The implementations of these interfaces are organized into a different hierarchy: on the one side,
  * built-in types such as AnyType, AnySimpleType, and the built-in atomic types and list types; on the other
  * side, user-defined types defined in a schema.</p>
  */
