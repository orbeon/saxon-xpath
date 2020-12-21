////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model




object Affinity extends Enumeration {

  val SAME_TYPE: Affinity = new Affinity()

  val SUBSUMES: Affinity = new Affinity()

  val SUBSUMED_BY: Affinity = new Affinity()

  val DISJOINT: Affinity = new Affinity()

  val OVERLAPS: Affinity = new Affinity()

  class Affinity extends Val

  implicit def convertValue(v: Value): Affinity = v.asInstanceOf[Affinity]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A set of constants enumerating the possible relationships between one type and another
  */
