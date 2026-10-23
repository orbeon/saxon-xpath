package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.{LocalBinding, VariableReference, XPathContext}
import org.orbeon.saxon.om.{Sequence, StructuredQName}
import org.orbeon.saxon.trans.FunctionStreamability
import org.orbeon.saxon.value.{IntegerValue, SequenceType}

class UserFunctionParameter extends LocalBinding {

  var requiredType: SequenceType = _

  var variableQName: StructuredQName = _
  def getVariableQName: StructuredQName = variableQName

  private var slotNumber: Int = _

  var referenceCount: Int = 999

  private var isIndexed: Boolean = false

  var functionStreamability: FunctionStreamability.FunctionStreamability =
    FunctionStreamability.UNCLASSIFIED

  def isGlobal: Boolean = false

  def isAssignable: Boolean = false

  def setSlotNumber(slot: Int): Unit =
    slotNumber = slot

  def getLocalSlotNumber: Int = slotNumber

  def getIntegerBoundsForVariable: Array[IntegerValue] = null

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = ()

  def setIndexedVariable(indexed: Boolean): Unit =
    isIndexed = indexed

  override def setIndexedVariable(): Unit =
    this.setIndexedVariable(true)

  def isIndexedVariable: Boolean = isIndexed

  def evaluateVariable(context: XPathContext): Sequence =
    context.evaluateLocalVariable(slotNumber)
}
