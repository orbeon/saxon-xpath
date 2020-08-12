////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import scala.beans.{BeanProperty}

object Genre extends Enumeration {

  val ANY: Genre = new Genre("any item")

  val ATOMIC: Genre = new Genre("an atomic value")

  val NODE: Genre = new Genre("a node")

  val FUNCTION: Genre = new Genre("a function")

  val MAP: Genre = new Genre("a map")

  val ARRAY: Genre = new Genre("an array")

  val EXTERNAL: Genre = new Genre("an external object")

  class Genre(desc: String) extends Val {

    @BeanProperty
    var description: String = desc

  }

  implicit def convertValue(v: Value): Genre = v.asInstanceOf[Genre]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The Genre of an item is the top-level classification of its item type: one of Atomic, Node, Function,
  * Map, Array, or External
  */
