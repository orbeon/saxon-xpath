////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import java.util.{HashMap, Map}

import org.orbeon.saxon.expr.instruct.ValueOf
import org.orbeon.saxon.expr.{Expression, Literal, StaticProperty}
import org.orbeon.saxon.lib.{ConversionRules, NamespaceConstant}
import org.orbeon.saxon.model.SchemaComponent.ValidationStatus
import org.orbeon.saxon.model.SchemaComponent.ValidationStatus.VALIDATED
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._

/**
 * This class represents a built-in atomic type, which may be either a primitive type
 * (such as xs:decimal or xs:anyURI) or a derived type (such as xs:ID or xs:dayTimeDuration).
 */
object BuiltInAtomicType {

  private val byAlphaCode: Map[String, BuiltInAtomicType] = new HashMap(60)

  val ANY_ATOMIC           : BuiltInAtomicType = makeAtomicType(StandardNames.XS_ANY_ATOMIC_TYPE,      AnySimpleType,        "A",        ordered = true )
  val STRING               : BuiltInAtomicType = makeAtomicType(StandardNames.XS_STRING,               ANY_ATOMIC,           "AS",       ordered = true )
  val BOOLEAN              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_BOOLEAN,              ANY_ATOMIC,           "AB",       ordered = true )
  val DURATION             : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DURATION,             ANY_ATOMIC,           "AR",       ordered = false)
  val DATE_TIME            : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DATE_TIME,            ANY_ATOMIC,           "AM",       ordered = true )
  val DATE                 : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DATE,                 ANY_ATOMIC,           "AA",       ordered = true )
  val TIME                 : BuiltInAtomicType = makeAtomicType(StandardNames.XS_TIME,                 ANY_ATOMIC,           "AT",       ordered = true )
  val G_YEAR_MONTH         : BuiltInAtomicType = makeAtomicType(StandardNames.XS_G_YEAR_MONTH,         ANY_ATOMIC,           "AH",       ordered = false)
  val G_MONTH              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_G_MONTH,              ANY_ATOMIC,           "AI",       ordered = false)
  val G_MONTH_DAY          : BuiltInAtomicType = makeAtomicType(StandardNames.XS_G_MONTH_DAY,          ANY_ATOMIC,           "AJ",       ordered = false)
  val G_YEAR               : BuiltInAtomicType = makeAtomicType(StandardNames.XS_G_YEAR,               ANY_ATOMIC,           "AG",       ordered = false)
  val G_DAY                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_G_DAY,                ANY_ATOMIC,           "AK",       ordered = false)
  val HEX_BINARY           : BuiltInAtomicType = makeAtomicType(StandardNames.XS_HEX_BINARY,           ANY_ATOMIC,           "AX",       ordered = true )
  val BASE64_BINARY        : BuiltInAtomicType = makeAtomicType(StandardNames.XS_BASE64_BINARY,        ANY_ATOMIC,           "A2",       ordered = true )
  val ANY_URI              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_ANY_URI,              ANY_ATOMIC,           "AU",       ordered = true )
  val QNAME                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_QNAME,                ANY_ATOMIC,           "AQ",       ordered = false)
  val NOTATION             : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NOTATION,             ANY_ATOMIC,           "AN",       ordered = false)
  val UNTYPED_ATOMIC       : BuiltInAtomicType = makeAtomicType(StandardNames.XS_UNTYPED_ATOMIC,       ANY_ATOMIC,           "AZ",       ordered = true )
  val DECIMAL              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DECIMAL,              ANY_ATOMIC,           "AD",       ordered = true )
  val FLOAT                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_FLOAT,                ANY_ATOMIC,           "AF",       ordered = true )
  val DOUBLE               : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DOUBLE,               ANY_ATOMIC,           "AO",       ordered = true )
  val INTEGER              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_INTEGER,              DECIMAL,              "ADI",      ordered = true )
  val NON_POSITIVE_INTEGER : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NON_POSITIVE_INTEGER, INTEGER,              "ADIN",     ordered = true )
  val NEGATIVE_INTEGER     : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NEGATIVE_INTEGER,     NON_POSITIVE_INTEGER, "ADINN",    ordered = true )
  val LONG                 : BuiltInAtomicType = makeAtomicType(StandardNames.XS_LONG,                 INTEGER,              "ADIL",     ordered = true )
  val INT                  : BuiltInAtomicType = makeAtomicType(StandardNames.XS_INT,                  LONG,                 "ADILI",    ordered = true )
  val SHORT                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_SHORT,                INT,                  "ADILIS",   ordered = true )
  val BYTE                 : BuiltInAtomicType = makeAtomicType(StandardNames.XS_BYTE,                 SHORT,                "ADILISB",  ordered = true )
  val NON_NEGATIVE_INTEGER : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NON_NEGATIVE_INTEGER, INTEGER,              "ADIP",     ordered = true )
  val POSITIVE_INTEGER     : BuiltInAtomicType = makeAtomicType(StandardNames.XS_POSITIVE_INTEGER,     NON_NEGATIVE_INTEGER, "ADIPP",    ordered = true )
  val UNSIGNED_LONG        : BuiltInAtomicType = makeAtomicType(StandardNames.XS_UNSIGNED_LONG,        NON_NEGATIVE_INTEGER, "ADIPL",    ordered = true )
  val UNSIGNED_INT         : BuiltInAtomicType = makeAtomicType(StandardNames.XS_UNSIGNED_INT,         UNSIGNED_LONG,        "ADIPLI",   ordered = true )
  val UNSIGNED_SHORT       : BuiltInAtomicType = makeAtomicType(StandardNames.XS_UNSIGNED_SHORT,       UNSIGNED_INT,         "ADIPLIS",  ordered = true )
  val UNSIGNED_BYTE        : BuiltInAtomicType = makeAtomicType(StandardNames.XS_UNSIGNED_BYTE,        UNSIGNED_SHORT,       "ADIPLISB", ordered = true )
  val YEAR_MONTH_DURATION  : BuiltInAtomicType = makeAtomicType(StandardNames.XS_YEAR_MONTH_DURATION,  DURATION,             "ARY",      ordered = true )
  val DAY_TIME_DURATION    : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DAY_TIME_DURATION,    DURATION,             "ARD",      ordered = true )
  val NORMALIZED_STRING    : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NORMALIZED_STRING,    STRING,               "ASN",      ordered = true )
  val TOKEN                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_TOKEN,                NORMALIZED_STRING,    "ASNT",     ordered = true )
  val LANGUAGE             : BuiltInAtomicType = makeAtomicType(StandardNames.XS_LANGUAGE,             TOKEN,                "ASNTL",    ordered = true )
  val NAME                 : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NAME,                 TOKEN,                "ASNTN",    ordered = true )
  val NMTOKEN              : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NMTOKEN,              TOKEN,                "ASNTK",    ordered = true )
  val NCNAME               : BuiltInAtomicType = makeAtomicType(StandardNames.XS_NCNAME,               NAME,                 "ASNTNC",   ordered = true )
  val ID                   : BuiltInAtomicType = makeAtomicType(StandardNames.XS_ID,                   NCNAME,               "ASNTNCI",  ordered = true )
  val IDREF                : BuiltInAtomicType = makeAtomicType(StandardNames.XS_IDREF,                NCNAME,               "ASNTNCR",  ordered = true )
  val ENTITY               : BuiltInAtomicType = makeAtomicType(StandardNames.XS_ENTITY,               NCNAME,               "ASNTNCE",  ordered = true )
  val DATE_TIME_STAMP      : BuiltInAtomicType = makeAtomicType(StandardNames.XS_DATE_TIME_STAMP,      DATE_TIME,            "AMP",      ordered = true )

  // We were getting an IntelliJ warning here about potential class loading deadlock. See bug #2524. Have moved the
  // static initializers here, and removed the dependency on static initialization in StringConverter, which hopefully
  // solves the problem.
  ANY_ATOMIC.stringConverter           = StringConverter.StringToString.INSTANCE
  STRING.stringConverter               = StringConverter.StringToString.INSTANCE
  LANGUAGE.stringConverter             = StringConverter.StringToLanguage.INSTANCE
  NORMALIZED_STRING.stringConverter    = StringConverter.StringToNormalizedString.INSTANCE
  TOKEN.stringConverter                = StringConverter.StringToToken.INSTANCE
  NCNAME.stringConverter               = StringConverter.StringToNCName.TO_NCNAME
  NAME.stringConverter                 = StringConverter.StringToName.INSTANCE
  NMTOKEN.stringConverter              = StringConverter.StringToNMTOKEN.INSTANCE
  ID.stringConverter                   = StringConverter.StringToNCName.TO_ID
  IDREF.stringConverter                = StringConverter.StringToNCName.TO_IDREF
  ENTITY.stringConverter               = StringConverter.StringToNCName.TO_ENTITY
  DECIMAL.stringConverter              = StringConverter.StringToDecimal.INSTANCE
  INTEGER.stringConverter              = StringConverter.StringToInteger.INSTANCE
  DURATION.stringConverter             = StringConverter.StringToDuration.INSTANCE
  G_MONTH.stringConverter              = StringConverter.StringToGMonth.INSTANCE
  G_MONTH_DAY.stringConverter          = StringConverter.StringToGMonthDay.INSTANCE
  G_DAY.stringConverter                = StringConverter.StringToGDay.INSTANCE
  DAY_TIME_DURATION.stringConverter    = StringConverter.StringToDayTimeDuration.INSTANCE
  YEAR_MONTH_DURATION.stringConverter  = StringConverter.StringToYearMonthDuration.INSTANCE
  TIME.stringConverter                 = StringConverter.StringToTime.INSTANCE
  BOOLEAN.stringConverter              = StringConverter.StringToBoolean.INSTANCE
  HEX_BINARY.stringConverter           = StringConverter.StringToHexBinary.INSTANCE
  BASE64_BINARY.stringConverter        = StringConverter.StringToBase64Binary.INSTANCE
  UNTYPED_ATOMIC.stringConverter       = StringConverter.StringToUntypedAtomic.INSTANCE
  NON_POSITIVE_INTEGER.stringConverter = new StringConverter.StringToIntegerSubtype(NON_POSITIVE_INTEGER)
  NEGATIVE_INTEGER.stringConverter     = new StringConverter.StringToIntegerSubtype(NEGATIVE_INTEGER)
  LONG.stringConverter                 = new StringConverter.StringToIntegerSubtype(LONG)
  INT.stringConverter                  = new StringConverter.StringToIntegerSubtype(INT)
  SHORT.stringConverter                = new StringConverter.StringToIntegerSubtype(SHORT)
  BYTE.stringConverter                 = new StringConverter.StringToIntegerSubtype(BYTE)
  NON_NEGATIVE_INTEGER.stringConverter = new StringConverter.StringToIntegerSubtype(NON_NEGATIVE_INTEGER)
  POSITIVE_INTEGER.stringConverter     = new StringConverter.StringToIntegerSubtype(POSITIVE_INTEGER)
  UNSIGNED_LONG.stringConverter        = new StringConverter.StringToIntegerSubtype(UNSIGNED_LONG)
  UNSIGNED_INT.stringConverter         = new StringConverter.StringToIntegerSubtype(UNSIGNED_INT)
  UNSIGNED_SHORT.stringConverter       = new StringConverter.StringToIntegerSubtype(UNSIGNED_SHORT)
  UNSIGNED_BYTE.stringConverter        = new StringConverter.StringToIntegerSubtype(UNSIGNED_BYTE)

  def fromAlphaCode(code: String): BuiltInAtomicType = byAlphaCode.get(code)

  /**
   * Analyze an expression to see whether the expression is capable of delivering a value of this
   * type.
   *
   * @param simpleType the simple type against which the expression is to be checked
   * @param expression the expression that delivers the content
   * @param kind       the node kind whose content is being delivered: { @link Type#ELEMENT},
   *                                                                           { @link Type#ATTRIBUTE}, or { @link Type#DOCUMENT}
   * if the expression will never deliver a value of the correct type
   */
  @throws[XPathException]
  def analyzeContentExpression(simpleType: SimpleType, expression: Expression, kind: Int): Unit =
    if (kind == Type.ELEMENT) {
      expression.checkPermittedContents(simpleType, whole = true)
      //            // if we are building the content of an element or document, no atomization will take
      //            // place, and therefore the presence of any element or attribute nodes in the content will
      //            // cause a validity error, since only simple content is allowed
      //            if (Type.isSubType(itemType, NodeKindTest.makeNodeKindTest(Type.ELEMENT))) {
      //                throw new XPathException("The content of an element with a simple type must not include any element nodes");
      //            }
      //            if (Type.isSubType(itemType, NodeKindTest.makeNodeKindTest(Type.ATTRIBUTE))) {
      //                throw new XPathException("The content of an element with a simple type must not include any attribute nodes");
    } else if (kind == Type.ATTRIBUTE) {
      // for attributes, do a check only for text nodes and atomic values: anything else gets atomized
      if (expression.isInstanceOf[ValueOf] || expression.isInstanceOf[Literal])
        expression.checkPermittedContents(simpleType, whole = true)
    }

  /**
   * Internal factory method to create a BuiltInAtomicType. There is one instance for each of the
   * built-in atomic types
   *
   * @param fingerprint The name of the type
   * @param baseType    The base type from which this type is derived
   * @param code        Alphabetic code chosen to enable ordering of types according to the type hierarchy
   * @param ordered     true if the type is ordered
   * @return the newly constructed built in atomic type
   */
  /*@NotNull*/
  private def makeAtomicType(fingerprint: Int, baseType: SimpleType, code: String, ordered: Boolean) = {
    val t = new BuiltInAtomicType(fingerprint)
    t.setBaseTypeFingerprint(baseType.getFingerprint)
    if (t.isPrimitiveType)
      t.primitiveFingerprint = fingerprint
    else
      t.primitiveFingerprint = baseType.asInstanceOf[AtomicType].getPrimitiveType
    t.uType = UType.fromTypeCode(t.primitiveFingerprint)
    t.ordered = ordered
    t.alphaCode = code
    BuiltInType.register(fingerprint, t)
    byAlphaCode.put(code, t)
    t
  }
}

class BuiltInAtomicType private(var fingerprint: Int) extends AtomicType with ItemType.WithSequenceTypeCache {

  private var baseFingerprint      : Int = _
  private var primitiveFingerprint : Int = _
  private var uType                : UType = _
  private var alphaCode            : String = _
  private var ordered              : Boolean = false
  var stringConverter              : StringConverter = _
  private var _one                 : SequenceType = _
  private var _oneOrMore           : SequenceType = _
  private var _zeroOrOne           : SequenceType = _
  private var _zeroOrMore          : SequenceType = _

  /**
   * Get the local name of this type
   *
   * @return the local name of this type definition, if it has one. Return null in the case of an
   *         anonymous type.
   */
  def getName: String = StandardNames.getLocalName(fingerprint)

  /**
   * Get the corresponding `org.orbeon.saxon.model.UType`. A UType is a union of primitive item
   * types.
   *
   * @return the smallest UType that subsumes this item type
   */
  def getUType: UType = uType

  /**
   * Get the target namespace of this type
   *
   * @return the target namespace of this type definition, if it has one. Return null in the case
   *         of an anonymous type, and in the case of a global type defined in a no-namespace schema.
   */
  def getTargetNamespace: String = NamespaceConstant.SCHEMA

  /**
   * Get the name of this type as an EQName, that is, a string in the format Q{uri}local.
   *
   * @return an EQName identifying the type.
   */
  def getEQName: String = "Q{" + NamespaceConstant.SCHEMA + "}" + getName

  /**
   * Determine whether the type is abstract, that is, whether it cannot have instances that are not also
   * instances of some concrete subtype
   */
  def isAbstract: Boolean = fingerprint match {
    case StandardNames.XS_NOTATION        |
         StandardNames.XS_ANY_ATOMIC_TYPE |
         StandardNames.XS_NUMERIC         |
         StandardNames.XS_ANY_SIMPLE_TYPE =>
       true
    case _ =>
      false
  }

  /**
   * Determine whether this is a built-in type or a user-defined type
   */
  def isBuiltInType = true

  /**
   * Get the name of this type as a StructuredQName, unless the type is anonymous, in which case
   * return null
   *
   * @return the name of the atomic type, or null if the type is anonymous.
   */
  def getTypeName =
    new StructuredQName(StandardNames.getPrefix(fingerprint), StandardNames.getURI(fingerprint), StandardNames.getLocalName(fingerprint))

  /**
   * Get an alphabetic code representing the type, or at any rate, the nearest built-in type
   * from which this type is derived. The codes are designed so that for any two built-in types
   * A and B, alphaCode(A) is a prefix of alphaCode(B) if and only if A is a supertype of B.
   *
   * @return the alphacode for the nearest containing built-in type
   */
  def getBasicAlphaCode: String = alphaCode

  /**
   * Get a sequence type representing exactly one instance of this atomic type
   *
   * @return a sequence type representing exactly one instance of this atomic type
   * @since 9.8.0.2
   */
  def one: SequenceType = {
    if (_one == null)
      _one = new SequenceType(this, StaticProperty.EXACTLY_ONE)
    _one
  }

  /**
   * Get a sequence type representing zero or one instances of this atomic type
   *
   * @return a sequence type representing zero or one instances of this atomic type
   * @since 9.8.0.2
   */
  def zeroOrOne: SequenceType = {
    if (_zeroOrOne == null)
      _zeroOrOne = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_ONE)
    _zeroOrOne
  }

  /**
   * Get a sequence type representing one or more instances of this atomic type
   *
   * @return a sequence type representing one or more instances of this atomic type
   * @since 9.8.0.2
   */
  def oneOrMore: SequenceType = {
    if (_oneOrMore == null)
      _oneOrMore = new SequenceType(this, StaticProperty.ALLOWS_ONE_OR_MORE)
    _oneOrMore
  }

  def zeroOrMore: SequenceType = {
    if (_zeroOrMore == null)
      _zeroOrMore = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_MORE)
    _zeroOrMore
  }

  /**
   * Get the redefinition level. This is zero for a component that has not been redefined;
   * for a redefinition of a level-0 component, it is 1; for a redefinition of a level-N
   * component, it is N+1. This concept is used to support the notion of "pervasive" redefinition:
   * if a component is redefined at several levels, the top level wins, but it is an error to have
   * two versions of the component at the same redefinition level.
   *
   * @return the redefinition level
   */
  def getRedefinitionLevel = 0

  /**
   * Determine whether the atomic type is ordered, that is, whether less-than and greater-than comparisons
   * are permitted
   *
   * @param optimistic if true, the function takes an optimistic view, returning true if ordering comparisons
   *                   are available for some subtype. This mainly affects xs:duration, where the function returns true if
   *                   optimistic is true, false if it is false.
   * @return true if ordering operations are permitted
   */
  def isOrdered(optimistic: Boolean): Boolean =
    ordered || (optimistic && ((this eq BuiltInAtomicType.DURATION) || (this eq BuiltInAtomicType.ANY_ATOMIC)))

  /**
   * Get the URI of the schema document where the type was originally defined.
   *
   * @return the URI of the schema document. Returns null if the information is unknown or if this
   *         is a built-in type
   */
  /*@Nullable*/
  def getSystemId: String = null

  /**
   * Determine whether the atomic type is numeric
   *
   * @return true if the type is a built-in numeric type
   */
  def isPrimitiveNumeric:Boolean = getFingerprint match {
    case StandardNames.XS_INTEGER |
         StandardNames.XS_DECIMAL |
         StandardNames.XS_DOUBLE  |
         StandardNames.XS_FLOAT =>
      true
    case _ =>
      false
  }

  /**
   * Get the validation status - always valid
   */
  final def getValidationStatus: ValidationStatus.ValidationStatus = VALIDATED

  /**
   * Returns the value of the 'block' attribute for this type, as a bit-significant
   * integer with fields such as `SchemaType` and `SchemaType`
   *
   * @return the value of the 'block' attribute for this type
   */
  final def getBlock = 0

  /**
   * Gets the integer code of the derivation method used to derive this type from its
   * parent. Returns zero for primitive types.
   *
   * @return a numeric code representing the derivation method, for example { @link SchemaType#DERIVATION_RESTRICTION}
   */
  final def getDerivationMethod: Int = DERIVATION_RESTRICTION

  /**
   * Determines whether derivation (of a particular kind)
   * from this type is allowed, based on the "final" property
   *
   * @param derivation the kind of derivation, for example { @link SchemaType#DERIVATION_LIST}
   * @return true if this kind of derivation is allowed
   */
  final def allowsDerivation(derivation: Int) = true

  /**
   * Get the types of derivation that are not permitted, by virtue of the "final" property.
   *
   * @return the types of derivation that are not permitted, as a bit-significant integer
   *         containing bits such as { @link org.orbeon.saxon.model.SchemaType#DERIVATION_EXTENSION}
   */
  def getFinalProhibitions = 0

  /**
   * Set the base type of this type
   *
   * @param baseFingerprint the namepool fingerprint of the name of the base type
   */
  final def setBaseTypeFingerprint(baseFingerprint: Int): Unit = this.baseFingerprint = baseFingerprint

  /**
   * Get the fingerprint of the name of this type
   *
   * @return the fingerprint. Returns an invented fingerprint for an anonymous type.
   */
  final def getFingerprint: Int = fingerprint

  /**
   * Get the name of the type as a QName
   *
   * @return a StructuredQName containing the name of the type. The conventional prefix "xs" is used
   *         to represent the XML Schema namespace
   */
  final def getStructuredQName = new StructuredQName("xs", NamespaceConstant.SCHEMA, StandardNames.getLocalName(fingerprint))

  /**
   * Get the display name of the type: that is, a lexical QName with an arbitrary prefix
   *
   * @return a lexical QName identifying the type
   */
  def getDisplayName: String = StandardNames.getDisplayName(fingerprint)

  /**
   * Ask whether the atomic type is a primitive type.  The primitive types are
   * the 19 primitive types of XML Schema, plus xs:integer, xs:dayTimeDuration and xs:yearMonthDuration;
   * xs:untypedAtomic; and all supertypes of these (xs:anyAtomicType, xs:numeric, ...)
   *
   * @return true if the type is considered primitive under the above rules
   */
  final def isPrimitiveType: Boolean = Type.isPrimitiveAtomicType(fingerprint)

  /**
   * Ask whether this SchemaType is a complex type
   *
   * @return true if this SchemaType is a complex type
   */
  final def isComplexType = false

  /**
   * Ask whether this is an anonymous type
   *
   * @return true if this SchemaType is an anonymous type
   */
  final def isAnonymousType = false

  /**
   * Ask whether this is a plain type (a type whose instances are always atomic values)
   *
   * @return true
   */
  def isPlainType = true

  /**
   * Returns the base type that this type inherits from. This method can be used to get the
   * base type of a type that is known to be valid.
   * If this type is a Simpletype that is a built in primitive type then null is returned.
   *
   * @return the base type.
   * @throws IllegalStateException if this type is not valid.
   */
  final def getBaseType: SchemaType =
    if (baseFingerprint == -1)
      null
    else
      BuiltInType.getSchemaType(baseFingerprint)

  /**
   * Test whether a given item conforms to this type
   *
   * @param item The item to be tested
   * @param th   The type hierarchy cache
   * @return true if the item is an instance of this type; false otherwise
   */
  def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[AtomicValue] && Type.isSubType(item.asInstanceOf[AtomicValue].getItemType, this)

  /**
   * Get the primitive item type corresponding to this item type. For item(),
   * this is Type.ITEM. For node(), it is Type.NODE. For specific node kinds,
   * it is the value representing the node kind, for example Type.ELEMENT.
   * For anyAtomicValue it is Type.ATOMIC_VALUE. For numeric it is Type.NUMBER.
   * For other atomic types it is the primitive type as defined in XML Schema,
   * except that INTEGER is considered to be a primitive type.
   */
  def getPrimitiveItemType: AtomicType =
    if (isPrimitiveType)
      this
    else {
      val s = getBaseType.asInstanceOf[ItemType]
      assert(s != null)
      if (s.isPlainType)
        s.getPrimitiveItemType.asInstanceOf[BuiltInAtomicType]
      else
        this
    }

  /**
   * Get the primitive type corresponding to this item type. For item(),
   * this is Type.ITEM. For node(), it is Type.NODE. For specific node kinds,
   * it is the value representing the node kind, for example Type.ELEMENT.
   * For anyAtomicValue it is Type.ATOMIC_VALUE. For numeric it is Type.NUMBER.
   * For other atomic types it is the primitive type as defined in XML Schema,
   * except that INTEGER is considered to be a primitive type.
   */
  def getPrimitiveType: Int = primitiveFingerprint

  /**
   * Determine whether this type is supported when using XSD 1.0
   *
   * @return true if this type is permitted in XSD 1.0
   */
  def isAllowedInXSD10: Boolean = getFingerprint != StandardNames.XS_DATE_TIME_STAMP

  override def toString: String = getDisplayName

  /**
   * Get the item type of the atomic values that will be produced when an item
   * of this type is atomized
   */
  def getAtomizedItemType: PlainType = this

  /**
   * Ask whether values of this type are atomizable
   *
   * @return true unless it is known that these items will be elements with element-only
   *         content, in which case return false
   * @param th The type hierarchy cache
   */
  def isAtomizable(th: TypeHierarchy) = true

  def getKnownBaseType: SchemaType = getBaseType

  /**
   * Test whether this is the same type as another type. They are considered to be the same type
   * if they are derived from the same type definition in the original XML representation (which
   * can happen when there are multiple includes of the same file)
   */
  def isSameType(other: SchemaType): Boolean = other.getFingerprint == getFingerprint

  def getDescription: String = getDisplayName

  /**
   * Check that this type is validly derived from a given type
   *
   * @param type  the type from which this type is derived
   * @param block the derivations that are blocked by the relevant element declaration
   * @throws SchemaException if the derivation is not allowed
   */
  @throws[SchemaException]
  def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit =
    if (`type` eq AnySimpleType) {
      // OK
    } else if (isSameType(`type`)) {
    } else {
      val base = getBaseType
      if (base == null)
        throw new SchemaException("The type " + getDescription + " is not validly derived from the type " + `type`.getDescription)
      try base.checkTypeDerivationIsOK(`type`, block)
      catch {
        case _: SchemaException =>
          throw new SchemaException("The type " + getDescription + " is not validly derived from the type " + `type`.getDescription)
      }
    }

  /**
   * Returns true if this SchemaType is a SimpleType
   *
   * @return true (always)
   */
  final def isSimpleType = true

  /**
   * Test whether this Simple Type is an atomic type
   *
   * @return true, this is an atomic type
   */
  def isAtomicType = true

  /**
   * Ask whether this type is an ID type. This is defined to be any simple type
   * who typed value may contain atomic values of type xs:ID: that is, it includes types derived
   * from ID by restriction, list, or union. Note that for a node to be treated
   * as an ID, its typed value must be a *single* atomic value of type ID; the type of the
   * node, however, can still allow a list.
   */
  def isIdType: Boolean = fingerprint == StandardNames.XS_ID

  /**
   * Ask whether this type is an IDREF or IDREFS type. This is defined to be any simple type
   * who typed value may contain atomic values of type xs:IDREF: that is, it includes types derived
   * from IDREF or IDREFS by restriction, list, or union
   */
  def isIdRefType: Boolean = fingerprint == StandardNames.XS_IDREF

  /**
   * Returns true if this type is derived by list, or if it is derived by restriction
   * from a list type, or if it is a union that contains a list as one of its members
   *
   * @return true if this is a list type
   */
  def isListType = false

  /**
   * Return true if this type is a union type (that is, if its variety is union)
   *
   * @return true for a union type
   */
  def isUnionType = false

  /**
   * Determine the whitespace normalization required for values of this type
   *
   * @return one of PRESERVE, REPLACE, COLLAPSE
   */
  def getWhitespaceAction: Int = getFingerprint match {
    case StandardNames.XS_STRING            => Whitespace.PRESERVE
    case StandardNames.XS_NORMALIZED_STRING => Whitespace.REPLACE
    case _                                  => Whitespace.COLLAPSE
  }

  /**
   * Returns the built-in base type this type is derived from.
   *
   * @return the first built-in type found when searching up the type hierarchy
   */
  def getBuiltInBaseType: SchemaType = {
    var base = this
    while ({(base != null) && (base.getFingerprint > 1023)})
      base = base.getBaseType.asInstanceOf[BuiltInAtomicType]
    base
  }

  /**
   * Test whether this simple type is namespace-sensitive, that is, whether
   * it is derived from xs:QName or xs:NOTATION.  Note that
   * the result for xs:anyAtomicType is false, even though an instance might be a QName.
   *
   * @return true if this type is derived from xs:QName or xs:NOTATION
   */
  def isNamespaceSensitive: Boolean = {
    var base = this
    var fp = base.getFingerprint
    while (fp > 1023) {
      base = base.getBaseType.asInstanceOf[BuiltInAtomicType]
      assert(base != null)
      fp = base.getFingerprint
    }
    fp == StandardNames.XS_QNAME || fp == StandardNames.XS_NOTATION
  }

  /**
   * Check whether a given input string is valid according to this SimpleType
   *
   * @param value      the input string to be checked
   * @param nsResolver a namespace resolver used to resolve namespace prefixes if the type
   *                   is namespace sensitive. The value supplied may be null; in this case any namespace-sensitive
   *                   content will throw an UnsupportedOperationException.
   * @param rules      conversion rules e.g for namespace-sensitive content
   * @return XPathException if the value is invalid. Note that the exception is returned rather than being thrown.
   *         Returns null if the value is valid.
   * @throws UnsupportedOperationException if the type is namespace-sensitive and no namespace
   *                                       resolver is supplied
   */
  def validateContent(value: CharSequence, nsResolver: NamespaceResolver, rules: ConversionRules): ValidationFailure = {
    val f = getFingerprint
    if (f == StandardNames.XS_STRING || f == StandardNames.XS_ANY_SIMPLE_TYPE || f == StandardNames.XS_UNTYPED_ATOMIC || f == StandardNames.XS_ANY_ATOMIC_TYPE)
      return null
    var converter = stringConverter
    if (converter == null) {
      converter = getStringConverter(rules)
      if (isNamespaceSensitive) {
        if (nsResolver == null)
          throw new UnsupportedOperationException("Cannot validate a QName without a namespace resolver")
        converter = converter.setNamespaceResolver(nsResolver).asInstanceOf[StringConverter]
        val result = converter.convertString(value)
        result match {
          case failure: ValidationFailure => return failure
          case _ =>
        }
        if (fingerprint == StandardNames.XS_NOTATION) {
          val nv = result.asInstanceOf[NotationValue]
          // This check added in 9.3. The XSLT spec says that this check should not be performed during
          // validation. However, this appears to be based on an incorrect assumption: see spec bug 6952
          if (!rules.isDeclaredNotation(nv.getNamespaceURI, nv.getLocalName))
            return new ValidationFailure("Notation {" + nv.getNamespaceURI + "}" + nv.getLocalName + " is not declared in the schema")
        }
        return null
      }
    }
    converter.validate(value)
  }

  /**
   * Get a StringConverter, an object that converts strings in the lexical space of this
   * data type to instances (in the value space) of the data type.
   *
   * @return a StringConverter to do the conversion. Note that in the case of namespace-sensitive
   *         types, the resulting converter needs to be supplied with a NamespaceResolver to handle prefix
   *         resolution.
   */
  def getStringConverter(rules: ConversionRules): StringConverter = {
    if (stringConverter != null) return stringConverter
    fingerprint match {
      case StandardNames.XS_DOUBLE | StandardNames.XS_NUMERIC =>
        rules.getStringToDoubleConverter
      case StandardNames.XS_FLOAT =>
        new StringConverter.StringToFloat(rules)
      case StandardNames.XS_DATE_TIME =>
        new StringConverter.StringToDateTime(rules)
      case StandardNames.XS_DATE_TIME_STAMP =>
        new StringConverter.StringToDateTimeStamp(rules)
      case StandardNames.XS_DATE =>
        new StringConverter.StringToDate(rules)
      case StandardNames.XS_G_YEAR =>
        new StringConverter.StringToGYear(rules)
      case StandardNames.XS_G_YEAR_MONTH =>
        new StringConverter.StringToGYearMonth(rules)
      case StandardNames.XS_ANY_URI =>
        new StringConverter.StringToAnyURI(rules)
      case StandardNames.XS_QNAME =>
        new StringConverter.StringToQName(rules)
      case StandardNames.XS_NOTATION =>
        new StringConverter.StringToNotation(rules)
      case _ =>
        throw new AssertionError("No string converter available for " + this)
    }
  }

  /**
   * Get the typed value of a node that is annotated with this schema type.
   *
   * @param node the node whose typed value is required
   * @return the typed value.
   * @since 8.5
   */
  @throws[XPathException]
  def atomize(node: NodeInfo): AtomicSequence = { // Fast path for common cases
    val stringValue = node.getStringValueCS
    if (stringValue.length == 0 && node.isNilled)
      return AtomicArray.EMPTY_ATOMIC_ARRAY
    if (fingerprint == StandardNames.XS_STRING)
      return StringValue.makeStringValue(stringValue)
    else if (fingerprint == StandardNames.XS_UNTYPED_ATOMIC)
      return new UntypedAtomicValue(stringValue)
    var converter = stringConverter
    if (converter == null) {
      converter = getStringConverter(node.getConfiguration.getConversionRules)
      if (isNamespaceSensitive) {
        val container = if (node.getNodeKind == Type.ELEMENT) node else node.getParent
        converter = converter.setNamespaceResolver(container.getAllNamespaces).asInstanceOf[StringConverter]
      }
    }
    converter.convertString(stringValue).asAtomic
  }

  /**
   * Get the typed value corresponding to a given string value, assuming it is
   * valid against this type (and that the containing node is not nilled)
   *
   * @param value    the string value
   * @param resolver a namespace resolver used to resolve any namespace prefixes appearing
   *                 in the content of values. Can supply null, in which case any namespace-sensitive content
   *                 will be rejected.
   * @param rules    the conversion rules to be used
   * @return an iterator over the atomic sequence comprising the typed value. The objects
   *         returned by this SequenceIterator will all be of type { @link AtomicValue}
   * @throws ValidationException This method should be called only if it is known that the value is
   *                             valid. If the value is not valid, there is no guarantee that this method will perform validation,
   *                             but if it does detect a validity error, then it MAY throw a ValidationException.
   */
  @throws[ValidationException]
  def getTypedValue(value: CharSequence, resolver: NamespaceResolver, rules: ConversionRules): AtomicSequence = {
    if (fingerprint == StandardNames.XS_STRING)
      return StringValue.makeStringValue(value)
    else if (fingerprint == StandardNames.XS_UNTYPED_ATOMIC)
      return new UntypedAtomicValue(value)
    var converter = getStringConverter(rules)
    if (isNamespaceSensitive)
      converter = converter.setNamespaceResolver(resolver).asInstanceOf[StringConverter]
    converter.convertString(value).asAtomic
  }

  /**
   * Two types are equal if they have the same fingerprint.
   * Note: it is normally safe to use ==, because we always use the static constants, one instance
   * for each built in atomic type. However, after serialization and deserialization a different instance
   * can appear.
   */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[BuiltInAtomicType] && getFingerprint == obj.asInstanceOf[BuiltInAtomicType].getFingerprint

  /**
   * The fingerprint can be used as a hashcode
   */
  override def hashCode: Int = getFingerprint

  /**
   * Validate that a primitive atomic value is a valid instance of a type derived from the
   * same primitive type.
   *
   * @param primValue    the value in the value space of the primitive type.
   * @param lexicalValue the value in the lexical space. If null, the string value of primValue
   *                     is used. This value is checked against the pattern facet (if any)
   * @param rules        the conversion rules to be used
   * @return null if the value is valid; otherwise, a ValidationFailure object indicating
   *         the nature of the error.
   * @throws UnsupportedOperationException in the case of an external object type
   */
  override def validate(primValue: AtomicValue, lexicalValue: CharSequence, rules: ConversionRules): ValidationFailure = fingerprint match {
    case StandardNames.XS_NUMERIC              |
         StandardNames.XS_STRING               |
         StandardNames.XS_BOOLEAN              |
         StandardNames.XS_DURATION             |
         StandardNames.XS_DATE_TIME            |
         StandardNames.XS_DATE                 |
         StandardNames.XS_TIME                 |
         StandardNames.XS_G_YEAR_MONTH         |
         StandardNames.XS_G_MONTH              |
         StandardNames.XS_G_MONTH_DAY          |
         StandardNames.XS_G_YEAR               |
         StandardNames.XS_G_DAY                |
         StandardNames.XS_HEX_BINARY           |
         StandardNames.XS_BASE64_BINARY        |
         StandardNames.XS_ANY_URI              |
         StandardNames.XS_QNAME                |
         StandardNames.XS_NOTATION             |
         StandardNames.XS_UNTYPED_ATOMIC       |
         StandardNames.XS_DECIMAL              |
         StandardNames.XS_FLOAT                |
         StandardNames.XS_DOUBLE               |
         StandardNames.XS_INTEGER =>
      null
    case StandardNames.XS_NON_POSITIVE_INTEGER |
         StandardNames.XS_NEGATIVE_INTEGER     |
         StandardNames.XS_LONG                 |
         StandardNames.XS_INT                  |
         StandardNames.XS_SHORT                |
         StandardNames.XS_BYTE                 |
         StandardNames.XS_NON_NEGATIVE_INTEGER |
         StandardNames.XS_POSITIVE_INTEGER     |
         StandardNames.XS_UNSIGNED_LONG        |
         StandardNames.XS_UNSIGNED_INT         |
         StandardNames.XS_UNSIGNED_SHORT       |
         StandardNames.XS_UNSIGNED_BYTE =>
      primValue.asInstanceOf[IntegerValue].validateAgainstSubType(this)
    case StandardNames.XS_YEAR_MONTH_DURATION  |
         StandardNames.XS_DAY_TIME_DURATION =>
      null // treated as primitive
    case StandardNames.XS_DATE_TIME_STAMP =>
      if (primValue.asInstanceOf[CalendarValue].getTimezoneInMinutes == CalendarValue.NO_TIMEZONE)
        new ValidationFailure("xs:dateTimeStamp value must have a timezone")
      else
        null
    case StandardNames.XS_NORMALIZED_STRING   |
         StandardNames.XS_TOKEN               |
         StandardNames.XS_LANGUAGE            |
         StandardNames.XS_NAME                |
         StandardNames.XS_NMTOKEN             |
         StandardNames.XS_NCNAME              |
         StandardNames.XS_ID                  |
         StandardNames.XS_IDREF               |
         StandardNames.XS_ENTITY =>
      stringConverter.validate(primValue.getStringValueCS)
    case _ =>
      throw new IllegalArgumentException
  }

  /**
   * Analyze an expression to see whether the expression is capable of delivering a value of this
   * type.
   *
   * @param expression the expression that delivers the content
   * @param kind       the node kind whose content is being delivered: { @link Type#ELEMENT},
   *                                                                           { @link Type#ATTRIBUTE}, or { @link Type#DOCUMENT}
   * if the expression will never deliver a value of the correct type
   */
  @throws[XPathException]
  override def analyzeContentExpression(expression: Expression, kind: Int): Unit =
    BuiltInAtomicType.analyzeContentExpression(this, expression, kind)

  /**
   * Apply any pre-lexical facets, other than whitespace. At the moment the only such
   * facet is saxon:preprocess
   *
   * @param input the value to be preprocessed
   * @return the value after preprocessing
   */
  override def preprocess(input: CharSequence): CharSequence = input

  /**
   * Reverse any pre-lexical facets, other than whitespace. At the moment the only such
   * facet is saxon:preprocess. This is called when converting a value of this type to
   * a string
   *
   * @param input the value to be postprocessed: this is the "ordinary" result of converting
   *              the value to a string
   * @return the value after postprocessing
   */
  override def postprocess(input: CharSequence): CharSequence = input

  /**
   * Get the list of plain types that are subsumed by this type
   *
   * @return for an atomic type, the type itself; for a plain union type, the list of plain types
   *         in its transitive membership, in declaration order
   */
  override def getPlainMemberTypes = Set(this)

  /**
   * Ask whether a built-in type is a numeric type (integer, float, double)
   *
   * @return true if the type is numeric
   */
  def isNumericType: Boolean = {
    val p = getPrimitiveItemType
    (p eq NumericType.getInstance)     ||
      (p eq BuiltInAtomicType.DECIMAL) ||
      (p eq BuiltInAtomicType.DOUBLE)  ||
      (p eq BuiltInAtomicType.FLOAT)   ||
      (p eq BuiltInAtomicType.INTEGER)
  }
}