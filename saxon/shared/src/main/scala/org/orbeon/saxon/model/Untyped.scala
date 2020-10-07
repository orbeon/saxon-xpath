////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.AtomicSequence
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.value.UntypedAtomicValue
import org.orbeon.saxon.z.IntHashSet
import org.orbeon.saxon.model.SchemaComponent.ValidationStatus.{VALIDATED, ValidationStatus}




object Untyped extends Enumeration {

  val INSTANCE: Untyped = new Untyped()

  class Untyped extends Val with ComplexType {

    /**
      * Get the validation status - always valid
      */
    def getValidationStatus(): ValidationStatus = VALIDATED

    /*@NotNull*/

    def getName: String = "untyped"

    /**
      * Get the name of this type as an EQName, that is, a string in the format Q{uri}local.
      *
      * @return an EQName identifying the type, specifically "Q{http://www.w3.org/2001/XMLSchema}untyped"
      */
    def getEQName(): String = "Q{" + NamespaceConstant.SCHEMA + "}untyped"

    def getRedefinitionLevel(): Int = 0

    def getTargetNamespace(): String = NamespaceConstant.SCHEMA

    def getVariety(): Int = ComplexType.VARIETY_MIXED

    /*@Nullable*/

    def getSystemId: String = null

    def getBlock(): Int = 0

    def getDerivationMethod(): Int = 0

    def allowsDerivation(derivation: Int): Boolean = false

    /**
      * Get the types of derivation that are not permitted, by virtue of the "final" property.
      *
      * @return the types of derivation that are not permitted, as a bit-significant integer
      *         containing bits such as {@link org.orbeon.saxon.model.SchemaType#DERIVATION_EXTENSION}
      */
    def getFinalProhibitions(): Int = 0

    def checkTypeDerivationIsOK(`type`: SchemaType, block: Int): Unit = ()

    def getFingerprint: Int = StandardNames.XS_UNTYPED

    /*@NotNull*/

    def getDisplayName: String = "xs:untyped"

    /**
      * Get the name of the type as a StructuredQName
      *
      * @return a StructuredQName identifying the type.  In the case of an anonymous type, an internally-generated
      * name is returned
      */
    def getStructuredQName: StructuredQName = NAME

    def isComplexType(): Boolean = true

    def isAnonymousType(): Boolean = false

    /*@NotNull*/

    def getKnownBaseType: SchemaType = AnyType.getInstance

    def isSameType(other: SchemaType): Boolean = other == INSTANCE

    /*@NotNull*/

    def getBaseType(): SchemaType = AnyType.getInstance

    def isAbstract(): Boolean = false

    def isSimpleType(): Boolean = false

    def isAtomicType: Boolean = false

    def isIdType(): Boolean = false

    def isIdRefType(): Boolean = false

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
      * @return true: this type is treated as a restriction of xs:anyType
      */
    def isRestricted(): Boolean = true

    def isEmptyContent(): Boolean = false

    def isEmptiable(): Boolean = true

    def isMixedContent(): Boolean = true

    /*@NotNull*/

    def getDescription: String = "xs:untyped"

    def analyzeContentExpression(expression: Expression, kind: Int): Unit = ()
//return;
//return;

    /*@NotNull*/

    def atomize(node: NodeInfo): AtomicSequence =
      new UntypedAtomicValue(node.getStringValueCS)

    /*@NotNull*/

    def getElementParticleType(elementName: Int,
                               considerExtensions: Boolean): SchemaType = this

    def getElementParticleCardinality(elementName: Int,
                                      considerExtensions: Boolean): Int =
      StaticProperty.ALLOWS_ZERO_OR_MORE

    /*@NotNull*/

    def getAttributeUseType(attributeName: StructuredQName): SimpleType =
      BuiltInAtomicType.UNTYPED_ATOMIC

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

  def getInstance: Untyped = INSTANCE

  val NAME: StructuredQName =
    new StructuredQName("xs", NamespaceConstant.SCHEMA, "untyped")

  implicit def convertValue(v: Value): Untyped = v.asInstanceOf[Untyped]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class has a singleton instance which represents the complex type xdt:untyped,
  * used for elements that have not been validated.
  */
