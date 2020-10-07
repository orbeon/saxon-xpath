////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans




object VisibilityProvenance extends Enumeration {

  val DEFAULTED: VisibilityProvenance = new VisibilityProvenance()

  val EXPLICIT: VisibilityProvenance = new VisibilityProvenance()

  val EXPOSED: VisibilityProvenance = new VisibilityProvenance()

  val ACCEPTED: VisibilityProvenance = new VisibilityProvenance()

  val DERIVED: VisibilityProvenance = new VisibilityProvenance()

  class VisibilityProvenance extends Val

  implicit def convertValue(v: Value): VisibilityProvenance =
    v.asInstanceOf[VisibilityProvenance]

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Indicates where the visibility property of a component came from
  */
