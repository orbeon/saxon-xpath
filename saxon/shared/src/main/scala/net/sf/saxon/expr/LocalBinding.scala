////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr




/**
  * Binding for local variables and parameters: anything that is allocated a slot on the XPathContext
  * stack frame.
  */
trait LocalBinding extends Binding {

  def getLocalSlotNumber(): Int

  def setIndexedVariable(): Unit

  def isIndexedVariable(): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
