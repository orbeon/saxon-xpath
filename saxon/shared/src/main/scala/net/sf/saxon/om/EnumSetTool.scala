////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import java.util.EnumSet




object EnumSetTool {

  def intersect[P <: Enum[P]](a: EnumSet[P], b: EnumSet[P]): EnumSet[P] = {
    val e: EnumSet[P] = EnumSet.copyOf(a)
    e.retainAll(b)
    e
  }

  def union[P <: Enum[P]](a: EnumSet[P], b: EnumSet[P]): EnumSet[P] = {
    val e: EnumSet[P] = EnumSet.copyOf(a)
    e.addAll(b)
    e
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A couple of utility methods for handling EnumSet objects.
  */
