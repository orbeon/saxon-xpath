package org.orbeon.saxon.model

import org.orbeon.saxon.expr.{Expression, ItemChecker}
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.om.{Function, Item}
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.value.SequenceType

object AnyFunctionType extends AnyFunctionType

trait AnyFunctionType extends FunctionItemType {

  def getUType: UType = UType.FUNCTION
  def isAtomicType: Boolean = false
  def isPlainType: Boolean = false
  def isMapType: Boolean = false
  def isArrayType: Boolean = false
  override def getDefaultPriority: Double = -0.5
  override def getBasicAlphaCode: String = "F"
  def getArgumentTypes: Array[SequenceType] = null
  def getAnnotationAssertions: AnnotationList = AnnotationList.EMPTY

  def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[Function]

  def getPrimitiveItemType: ItemType = this
  def getPrimitiveType: Int = Type.FUNCTION

  override def toString: String = "function(*)"

  def getAtomizedItemType: PlainType = null
  def isAtomizable(th: TypeHierarchy): Boolean = true

  def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity.Affinity =
    if (other == this) {
      Affinity.SAME_TYPE
    } else {
      Affinity.SUBSUMES
    }

  def makeFunctionSequenceCoercer(exp: Expression, role: RoleDiagnostic): Expression =
    new ItemChecker(exp, this, role)

  def getResultType: SequenceType = SequenceType.ANY_SEQUENCE
}
