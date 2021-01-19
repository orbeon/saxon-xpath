////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.value.SequenceType


/**
 * BindingReference is a interface used to mark references to a variable declaration. The main
 * implementation is VariableReference, which represents a reference to a variable in an XPath
 * expression, but it is also used to represent a reference to a variable in a saxon:assign instruction.
 */
trait BindingReference {

  def setStaticType(
    `type`        : SequenceType,
    constantValue : GroundedValue,
    properties    : Int
  ): Unit

  def fixup(binding: Binding): Unit
}
