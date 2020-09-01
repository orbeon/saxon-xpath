////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Binding is a interface used to represent the run-time properties and methods
  * associated with a variable: specifically, a method to get the value
  * of the variable.
  */

package net.sf.saxon.expr

import net.sf.saxon.om.{Sequence, StructuredQName}
import net.sf.saxon.value.{IntegerValue, SequenceType}


trait Binding {
  def getRequiredType: SequenceType
  /*@Nullable*/
  def getIntegerBoundsForVariable: Array[IntegerValue]
  def evaluateVariable(context: XPathContext): Sequence
  def isGlobal: Boolean
  def isAssignable: Boolean
  def getVariableQName: StructuredQName
  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit
}
