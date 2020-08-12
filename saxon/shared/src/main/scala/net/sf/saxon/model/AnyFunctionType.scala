package net.sf.saxon.model

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.ItemChecker

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.om.Function

import net.sf.saxon.om.Item

import net.sf.saxon.query.AnnotationList

import net.sf.saxon.value.SequenceType

import AnyFunctionType._

object AnyFunctionType {

  val ANY_FUNCTION: AnyFunctionType = new AnyFunctionType()

  def getInstance(): AnyFunctionType = ANY_FUNCTION

}

class AnyFunctionType extends FunctionItemType {

  def getUType(): UType = UType.FUNCTION

  def isAtomicType(): Boolean = false

  def isPlainType(): Boolean = false

  def isMapType(): Boolean = false

  def isArrayType(): Boolean = false

  override def getDefaultPriority(): Double = -0.5

  override def getBasicAlphaCode(): String = "F"

  def getArgumentTypes(): Array[SequenceType] = null

  def getAnnotationAssertions(): AnnotationList = AnnotationList.EMPTY

  def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[Function]

  def getPrimitiveItemType(): ItemType = ANY_FUNCTION

  def getPrimitiveType(): Int = Type.FUNCTION

  override def toString(): String = "function(*)"

  def getAtomizedItemType(): PlainType = null

  def isAtomizable(th: TypeHierarchy): Boolean = true

  def relationship(other: FunctionItemType, th: TypeHierarchy): Affinity.Affinity =
    if (other == this) {
      Affinity.SAME_TYPE
    } else {
      Affinity.SUBSUMES
    }

  def makeFunctionSequenceCoercer(exp: Expression,
                                  role: RoleDiagnostic): Expression =
    new ItemChecker(exp, this, role)

  def getResultType(): SequenceType = SequenceType.ANY_SEQUENCE

}
