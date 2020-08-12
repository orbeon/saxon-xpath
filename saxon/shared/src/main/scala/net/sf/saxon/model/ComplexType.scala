////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.z.IntHashSet


trait ComplexType extends SchemaType {

  def getVariety(): Int

  def isAbstract(): Boolean

  def isComplexContent(): Boolean

  def isSimpleContent(): Boolean

  def isAllContent(): Boolean

  /*@Nullable*/

  def getSimpleContentType(): SimpleType

  def isRestricted(): Boolean

  def isEmptyContent(): Boolean

  def isEmptiable(): Boolean

  def isMixedContent(): Boolean

  /*@Nullable*/

  def getElementParticleType(elementName: Int,
                             considerExtensions: Boolean): SchemaType

  def getElementParticleCardinality(elementName: Int,
                                    considerExtensions: Boolean): Int

  /*@Nullable*/

  def getAttributeUseType(attributeName: StructuredQName): SimpleType

  def getAttributeUseCardinality(attributeName: StructuredQName): Int

  def allowsAttributes(): Boolean

  def gatherAllPermittedChildren(children: IntHashSet,
                                 ignoreWildcards: Boolean): Unit

  def gatherAllPermittedDescendants(descendants: IntHashSet): Unit

  /*@Nullable*/

  def getDescendantElementType(fingerprint: Int): SchemaType

  def getDescendantElementCardinality(elementFingerprint: Int): Int

  def containsElementWildcard(): Boolean

  def hasAssertions(): Boolean

}

object ComplexType {
  var VARIETY_EMPTY: Int = 0

  var VARIETY_SIMPLE: Int = 1

  var VARIETY_ELEMENT_ONLY: Int = 2

  var VARIETY_MIXED: Int = 3

  var OPEN_CONTENT_ABSENT: Int = 0

  var OPEN_CONTENT_NONE: Int = 1

  var OPEN_CONTENT_INTERLEAVE: Int = 2

  var OPEN_CONTENT_SUFFIX: Int = 3
}