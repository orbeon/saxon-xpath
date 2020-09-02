////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.expr.{Expression, StaticProperty}
import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}
import net.sf.saxon.om.{AtomicSequence, NodeInfo, StandardNames, StructuredQName}
import net.sf.saxon.value.UntypedAtomicValue
import net.sf.saxon.z.IntHashSet



object AnyType extends Enumeration {

  val INSTANCE: AnyType = new AnyType()

  class AnyType extends Val with ComplexType {

    /*@NotNull*/

    def getName: String = "anyType"

    /**
      * Get the name of this type as an EQName, that is, a string in the format Q{uri}local.
      *
      * @return an EQName identifying the type, specifically "Q{http://www.w3.org/2001/XMLSchema}anyType"
      */
    def getEQName(): String = "Q{" + NamespaceConstant.SCHEMA + "}anyType"

    def getTargetNamespace(): String = NamespaceConstant.SCHEMA

    def getVariety(): Int = ComplexType.VARIETY_MIXED

    /**
      * Get the validation status - always valid
      */
    def getValidationStatus(): ValidationStatus = VALIDATED

    def getRedefinitionLevel(): Int = 0

    /*@Nullable*/

    def getBaseType(): SchemaType = null

    /*@Nullable*/

    def getKnownBaseType: SchemaType = null

    def getDerivationMethod(): Int = 0

    def allowsDerivation(derivation: Int): Boolean = true

    /**
      * Get the types of derivation that are not permitted, by virtue of the "final" property.
      *
      * @return the types of derivation that are not permitted, as a bit-significant integer
      *         containing bits such as {@link net.sf.saxon.model.SchemaType#DERIVATION_EXTENSION}
      */
    def getFinalProhibitions(): Int = 0

    def isAbstract(): Boolean = false

    def isComplexType(): Boolean = true

    def isAnonymousType(): Boolean = false

    def isSimpleType(): Boolean = false

    def isAtomicType: Boolean = false

    def isIdType(): Boolean = false

    def isIdRefType(): Boolean = false

    def getBlock(): Int = 0

    /**
      * Test whether this complex type has complex content
      *
      * @return true: this complex type has complex content
      */
    def isComplexContent(): Boolean = true

    def isSimpleContent(): Boolean = false

    def isAllContent(): Boolean = false

    /*@Nullable*/

    def getSimpleContentType(): SimpleType = null

    /**
      * Test whether this complex type is derived by restriction
      *
      * @return false: this type is not a restriction
      */
    def isRestricted(): Boolean = false

    def isEmptyContent(): Boolean = false

    def isEmptiable(): Boolean = true

    def isMixedContent(): Boolean = true

    def getFingerprint(): Int = StandardNames.XS_ANY_TYPE

    /**
      * Get the name of the type as a StructuredQName
      *
      * @return a StructuredQName identifying the type.  In the case of an anonymous type, an internally-generated
      * name is returned
      */
    def getStructuredQName: StructuredQName = QNAME

    /*@NotNull*/

    def getDescription: String = "xs:anyType"

    /*@NotNull*/

    def getDisplayName: String = "xs:anyType"

    /*@Nullable*/

    def getSystemId: String = null

    def isSameType(other: SchemaType): Boolean = other.isInstanceOf[AnyType]

    def analyzeContentExpression(expression: Expression, kind: Int): Unit = ()
//return;
//return;

    /*@NotNull*/

    def atomize(node: NodeInfo): AtomicSequence =
      new UntypedAtomicValue(node.getStringValue)

    def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit = {
      if (!(`type`.isInstanceOf[AnyType])) {
        throw new SchemaException("Cannot derive xs:anyType from another type")
      }
    }

    /*@NotNull*/

    def getElementParticleType(elementName: Int,
                               considerExtensions: Boolean): SchemaType = this

    def getElementParticleCardinality(elementName: Int,
                                      considerExtensions: Boolean): Int =
      StaticProperty.ALLOWS_ZERO_OR_MORE

    /*@NotNull*/

    def getAttributeUseType(attributeName: StructuredQName): SimpleType =
      AnySimpleType

    def getAttributeUseCardinality(attributeName: StructuredQName): Int =
      StaticProperty.ALLOWS_ZERO_OR_ONE

    def allowsAttributes(): Boolean = true

    def gatherAllPermittedChildren(children: IntHashSet,
                                   ignoreWildcards: Boolean): Unit = {
      children.add(-1)
    }

    def gatherAllPermittedDescendants(descendants: IntHashSet): Unit = {
      descendants.add(-1)
    }

    /*@NotNull*/

    def getDescendantElementType(fingerprint: Int): SchemaType = this

    def getDescendantElementCardinality(elementFingerprint: Int): Int =
      StaticProperty.ALLOWS_ZERO_OR_MORE

    def containsElementWildcard(): Boolean = true

    /**
      * Ask whether there are any assertions defined on this complex type
      *
      * @return true if there are any assertions
      */
    def hasAssertions(): Boolean = false

  }

  /*@NotNull*/

  def getInstance: AnyType = INSTANCE

  val QNAME: StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "anyType")

  implicit def convertValue(v: Value): AnyType = v.asInstanceOf[AnyType]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class has a singleton instance which represents the XML Schema built-in type xs:anyType,
  * also known as the urtype.
  * <p>See XML Schema 1.1 Part 1 section 3.4.7</p>
  */
