package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.LocalBinding

import org.orbeon.saxon.expr.VariableReference

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.FunctionStreamability

import org.orbeon.saxon.value.IntegerValue

import org.orbeon.saxon.value.SequenceType

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

  def isGlobal: Boolean = false

  def isAssignable: Boolean = false

  def setSlotNumber(slot: Int): Unit = {
    slotNumber = slot
  }

  def getLocalSlotNumber: Int = slotNumber

  def getIntegerBoundsForVariable: Array[IntegerValue] = null

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = ()

  def setIndexedVariable(indexed: Boolean): Unit = {
    isIndexed = indexed
  }

  override def setIndexedVariable(): Unit = {
    this.setIndexedVariable(true)
  }

  def isIndexedVariable: Boolean = isIndexed

  def evaluateVariable(context: XPathContext): Sequence =
    context.evaluateLocalVariable(slotNumber)

}
