package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.LocalBinding

import net.sf.saxon.expr.VariableReference

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.FunctionStreamability

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

class UserFunctionParameter extends LocalBinding {

  @BeanProperty
  var requiredType: SequenceType = _

  @BeanProperty
  var variableQName: StructuredQName = _

  private var slotNumber: Int = _

  @BeanProperty
  var referenceCount: Int = 999

  private var isIndexed: Boolean = false

  @BeanProperty
  var functionStreamability: FunctionStreamability.FunctionStreamability =
    FunctionStreamability.UNCLASSIFIED

  def isGlobal(): Boolean = false

  def isAssignable(): Boolean = false

  def setSlotNumber(slot: Int): Unit = {
    slotNumber = slot
  }

  def getLocalSlotNumber(): Int = slotNumber

  def getIntegerBoundsForVariable(): Array[IntegerValue] = null

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = {}

  def setIndexedVariable(indexed: Boolean): Unit = {
    isIndexed = indexed
  }

  override def setIndexedVariable(): Unit = {
    this.setIndexedVariable(true)
  }

  def isIndexedVariable(): Boolean = isIndexed

  def evaluateVariable(context: XPathContext): Sequence =
    context.evaluateLocalVariable(slotNumber)

}
