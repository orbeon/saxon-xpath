////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.trans.SymbolicName




/**
  * Represents an expression or instruction such as call-template, or a user function call, or
  * a global variable reference, that needs to be bound to a target component, and can potentially
  * be re-bound when the containing component is copied into another package.
  */
trait ComponentInvocation {

  def getFixedTarget(): Component

  def setBindingSlot(slot: Int): Unit

  def getBindingSlot(): Int

  def getSymbolicName(): SymbolicName

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
