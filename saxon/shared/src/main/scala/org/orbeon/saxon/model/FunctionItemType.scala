////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.model.Affinity.Affinity
import org.orbeon.saxon.om.Genre
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.value.SequenceType


trait FunctionItemType extends ItemType {
  override def getGenre: Genre = Genre.FUNCTION
  def getArgumentTypes: Array[SequenceType]
  def getResultType: SequenceType
  def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity
  def getAnnotationAssertions: AnnotationList
  def makeFunctionSequenceCoercer(exp: Expression, role: RoleDiagnostic): Expression
  def isMapType: Boolean
  def isArrayType: Boolean
}
