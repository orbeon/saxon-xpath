////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2012 Michael Froh.
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.ma.trie




class Tuple2[T1, T2](val _1: T1, val _2: T2) {

  override def equals(o: Any): Boolean = {
    if (this == o) return true
    if (o == null || getClass != o.getClass) return false
    val tuple: Tuple2[T1, T2] = o.asInstanceOf[Tuple2[T1, T2]]
    if (if (_1 != null) _1 != tuple._1 else tuple._1 != null) return false
    if (if (_2 != null) _2 != tuple._2 else tuple._2 != null) return false
    true
  }

  override def hashCode: Int = {
    var result: Int = if (_1 != null) _1.hashCode else 0
    result = 31 * result + (if (_2 != null) _2.hashCode else 0)
    result
  }

  override def toString: String = "(" + _1 + ',' + _2 + ')'

}

