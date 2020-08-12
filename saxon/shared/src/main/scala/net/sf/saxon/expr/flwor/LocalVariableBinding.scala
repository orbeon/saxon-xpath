////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.expr.FilterExpression

import net.sf.saxon.expr.LocalBinding

import net.sf.saxon.expr.VariableReference

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}




class LocalVariableBinding(private var variableName: StructuredQName,
                           @BeanProperty var requiredType: SequenceType)
    extends LocalBinding {

  private var slotNumber: Int = -999

  private var refCount: Int = 0

  def copy(): LocalVariableBinding = {
    val lb2: LocalVariableBinding =
      new LocalVariableBinding(variableName, requiredType)
    lb2.slotNumber = slotNumber
    lb2.refCount = refCount
    lb2
  }

  def getVariableQName(): StructuredQName = variableName

  /**
    * If the variable is bound to an integer, get the minimum and maximum possible values.
    * Return null if unknown or not applicable
    */
  def getIntegerBoundsForVariable(): Array[IntegerValue] = null

  def getNominalReferenceCount(): Int = refCount

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = {
    if (refCount != FilterExpression.FILTERED) {
      refCount += (if (isLoopingReference) 10 else 1)
    }
  }

  def setIndexedVariable(): Unit = {
    refCount = FilterExpression.FILTERED
  }

  override def isIndexedVariable(): Boolean =
    refCount == FilterExpression.FILTERED

  def setVariableQName(variableName: StructuredQName): Unit = {
    this.variableName = variableName
  }

  def setSlotNumber(nr: Int): Unit = {
    slotNumber = nr
  }

  def getLocalSlotNumber(): Int = slotNumber

  def evaluateVariable(context: XPathContext): Sequence =
    context.evaluateLocalVariable(slotNumber)

  def isAssignable(): Boolean = false

  def isGlobal(): Boolean = false

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Represents the defining occurrence of a variable declared within a FLWOR expression,
  * for example the $p in "for $x at $p in ...". Also used for the variables bound to the context
  * item in a pattern that uses current()
  */
