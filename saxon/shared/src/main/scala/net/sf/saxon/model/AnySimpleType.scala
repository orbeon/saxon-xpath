////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


package net.sf.saxon.model

import net.sf.saxon.expr.Expression
import net.sf.saxon.lib.{ConversionRules, NamespaceConstant}
import net.sf.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}
import net.sf.saxon.om._
import net.sf.saxon.value.{UntypedAtomicValue, Whitespace}


/**
  * This class has a singleton instance which represents the XML Schema built-in type xs:anySimpleType
  */
object AnySimpleType extends SimpleType {


  /*@NotNull*/

  def getName(): String = "anySimpleType"

  def getTargetNamespace(): String = NamespaceConstant.SCHEMA

  /**
    * Get the name of this type as an EQName, that is, a string in the format Q{uri}local.
    *
    * @return an EQName identifying the type. In the case of an anonymous type, an internally-generated
    *         name is returned
    */
  def getEQName(): String =
    "Q{" + NamespaceConstant.SCHEMA + "}anySimpleType"

  def isBuiltInType(): Boolean = true

  def isIdType(): Boolean = false

  def isIdRefType(): Boolean = false

  def getRedefinitionLevel(): Int = 0

  /*@Nullable*/

  def getSystemId(): String = null

  /**
    * Get the validation status - always valid
    */
  def getValidationStatus(): ValidationStatus = VALIDATED

  def getBaseType(): SchemaType = AnyType.getInstance

  def isComplexType(): Boolean = false

  def isSimpleType(): Boolean = true

  def getFingerprint(): Int = StandardNames.XS_ANY_SIMPLE_TYPE

  /**
    * Get the name of the type as a StructuredQName
    *
    * @return a StructuredQName identifying the type.  In the case of an anonymous type, an internally-generated
    * name is returned
    */
  def getStructuredQName(): StructuredQName = NAME

  /*@NotNull*/

  def getDescription(): String = "xs:anySimpleType"

  /*@NotNull*/

  def getDisplayName(): String = "xs:anySimpleType"

  def isSameType(other: SchemaType): Boolean = other == AnySimpleType

  /*@NotNull*/

  def atomize(node: NodeInfo): AtomicSequence =
    new UntypedAtomicValue(node.getStringValueCS)

  def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit = {
    if (`type` == this) {
      return
    }
    throw new SchemaException(
      "Cannot derive xs:anySimpleType from another type")
  }

  def isAtomicType(): Boolean = false

  def isAnonymousType(): Boolean = false

  /**
    * Determine whether this is a list type
    *
    * @return false (it isn't a list type)
    */
  def isListType(): Boolean = false

  /**
    * Determin whether this is a union type
    *
    * @return false (it isn't a union type)
    */
  def isUnionType(): Boolean = false

  /*@NotNull*/

  def getBuiltInBaseType(): SchemaType = this

  /*@NotNull*/

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicSequence =
    new UntypedAtomicValue(value)

  /*@Nullable*/

  def validateContent(value: CharSequence,
                      nsResolver: NamespaceResolver,
                      rules: ConversionRules): ValidationFailure = null

  /**
    * Test whether this type represents namespace-sensitive content
    *
    * @return false
    */
  def isNamespaceSensitive(): Boolean = false

  def getBlock(): Int = 0

  def getDerivationMethod(): Int = DERIVATION_RESTRICTION

  def allowsDerivation(derivation: Int): Boolean = true

  /**
    * Get the types of derivation that are not permitted, by virtue of the "final" property.
    *
    * @return the types of derivation that are not permitted, as a bit-significant integer
    *         containing bits such as {@link net.sf.saxon.model.SchemaType#DERIVATION_EXTENSION}
    */
  def getFinalProhibitions(): Int = 0

  def getWhitespaceAction(): Int = Whitespace.PRESERVE

  def analyzeContentExpression(expression: Expression, kind: Int): Unit = ()
//return;
//return;

  def preprocess(input: CharSequence): CharSequence = input

  def postprocess(input: CharSequence): CharSequence = input

  /*@NotNull*/

  def getInstance(): AnySimpleType.type = AnySimpleType

  val NAME: StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "anySimpleType")

//  implicit def convertValue(v: Value): AnySimpleType.type =
//    v.asInstanceOf[AnySimpleType.type]

}

