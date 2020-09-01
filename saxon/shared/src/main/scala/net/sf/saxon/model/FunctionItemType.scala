////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.parser.RoleDiagnostic
import net.sf.saxon.model.Affinity.Affinity
import net.sf.saxon.om.Genre
import net.sf.saxon.om.Genre.Genre
import net.sf.saxon.query.AnnotationList
import net.sf.saxon.value.SequenceType


trait FunctionItemType extends ItemType {

  override def getGenre: Genre = Genre.FUNCTION

  def getArgumentTypes: Array[SequenceType]

  def getResultType: SequenceType

  def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity

  def getAnnotationAssertions: AnnotationList

  def makeFunctionSequenceCoercer(exp: Expression,
                                  role: RoleDiagnostic): Expression

  def isMapType: Boolean

  def isArrayType: Boolean

}
