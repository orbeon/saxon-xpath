////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.sxpath

import org.orbeon.saxon.expr.LocalBinding

import org.orbeon.saxon.expr.VariableReference

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.value.IntegerValue

import org.orbeon.saxon.value.SequenceType

import XPathVariable._

import scala.beans.{BeanProperty, BooleanBeanProperty}


object XPathVariable {

  def make(name: StructuredQName): XPathVariable = {
    val v: XPathVariable = new XPathVariable()
    v.name = name
    v
  }

}

class XPathVariable private() extends LocalBinding {

  private var name: StructuredQName = _

  @BeanProperty
  var requiredType: SequenceType = SequenceType.ANY_SEQUENCE

  @BeanProperty
  var defaultValue: Sequence = _

  private var slotNumber: Int = _

  def isGlobal: Boolean = false

  def isAssignable: Boolean = false

  /*@Nullable*/

  def getIntegerBoundsForVariable: Array[IntegerValue] = null

  def setSlotNumber(slotNumber: Int): Unit = {
    this.slotNumber = slotNumber
  }

  def getLocalSlotNumber: Int = slotNumber

  def getVariableQName: StructuredQName = name

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = ()

  // no action
  // no action

  def evaluateVariable(context: XPathContext): Sequence =
    context.evaluateLocalVariable(slotNumber)

  /**
   * Say that the bound value has the potential to be indexed
   */
  def setIndexedVariable(): Unit = ()

  /**
   * Ask whether the binding is to be indexed
   *
   * @return true if the variable value can be indexed
   */
  def isIndexedVariable: Boolean = false

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An object representing an XPath variable for use in the standalone XPath API. The object
 * can only be created by calling the declareVariable method of class {@link IndependentContext}.
 * Note that once declared, this object is thread-safe: it does not hold the actual variable
 * value, which means it can be used with any number of evaluations of a given XPath expression,
 * in series or in parallel.
 * <p>A variable can be given a value by calling
 * {@link XPathDynamicContext#setVariable(XPathVariable, org.orbeon.saxon.om.Sequence)}.
 * Note that the value of the variable is not held in the XPathVariable object, but in the
 * XPathDynamicContext, which means that the XPathVariable itself can be used in multiple threads.</p>
 */
