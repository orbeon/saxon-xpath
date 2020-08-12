////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.parser

import net.sf.saxon.expr.Binding

import java.util.IdentityHashMap

import java.util.Map




/**
  * A map from old bindings to new bindings, maintained during a copy() operation applied
  * to an expression tree. When local variable bindings are copied, the mapping from old to
  * new binding can be added to the list; when local variable references are encountered,
  * the binding can be switched from the old binding to the new.
  *
  * Bindings are compared strictly using object identity. Some classes that implement the
  * Binding interface have their own equals() method that could return true for two
  * bindings that need to be treated as distinct.
  */
class RebindingMap {

// created lazily
  private var map: Map[Binding, Binding] = null

  def put(oldBinding: Binding, newBinding: Binding): Unit = {
    if (map == null) {
      map = new IdentityHashMap[Binding, Binding]()
    }
    map.put(oldBinding, newBinding)
  }

  def get(oldBinding: Binding): Binding =
    if (map == null) null else map.get(oldBinding)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
