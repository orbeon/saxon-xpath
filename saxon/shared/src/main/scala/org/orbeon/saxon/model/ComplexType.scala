////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.z.IntHashSet


/**
 * A complex type as defined in XML Schema: either a user-defined complex type, or xs:anyType, or xs:untyped.
 * In the non-schema-aware version of the Saxon product, the only complex type encountered is xs:untyped.
 */
trait ComplexType extends SchemaType {

  def getVariety: Int
  def isAbstract: Boolean
  def isComplexContent: Boolean
  def isSimpleContent: Boolean
  def isAllContent: Boolean

  /*@Nullable*/
  def getSimpleContentType: SimpleType
  def isRestricted: Boolean
  def isEmptyContent: Boolean
  def isEmptiable: Boolean
  def isMixedContent: Boolean

  /*@Nullable*/
  def getElementParticleType(elementName: Int, considerExtensions: Boolean): SchemaType
  def getElementParticleCardinality(elementName: Int, considerExtensions: Boolean): Int

  /*@Nullable*/
  def getAttributeUseType(attributeName: StructuredQName): SimpleType
  def getAttributeUseCardinality(attributeName: StructuredQName): Int
  def allowsAttributes(): Boolean
  def gatherAllPermittedChildren(children: IntHashSet, ignoreWildcards: Boolean): Unit
  def gatherAllPermittedDescendants(descendants: IntHashSet): Unit

  /*@Nullable*/
  def getDescendantElementType(fingerprint: Int): SchemaType
  def getDescendantElementCardinality(elementFingerprint: Int): Int
  def containsElementWildcard(): Boolean
  def hasAssertions: Boolean
}

object ComplexType {
  var VARIETY_EMPTY           : Int = 0
  var VARIETY_SIMPLE          : Int = 1
  var VARIETY_ELEMENT_ONLY    : Int = 2
  var VARIETY_MIXED           : Int = 3
  var OPEN_CONTENT_ABSENT     : Int = 0
  var OPEN_CONTENT_NONE       : Int = 1
  var OPEN_CONTENT_INTERLEAVE : Int = 2
  var OPEN_CONTENT_SUFFIX     : Int = 3
}