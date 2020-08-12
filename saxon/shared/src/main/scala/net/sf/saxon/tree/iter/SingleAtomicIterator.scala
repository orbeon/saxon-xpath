////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.value.AtomicValue




class SingleAtomicIterator[T <: AtomicValue](value: T)
    extends SingletonIterator[T](value)
    with AtomicIterator[T]
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator {

  /*@NotNull*/

  override def getReverseIterator(): SingleAtomicIterator[T] =
    new SingleAtomicIterator[T](getValue)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * SingletonIterator: an iterator over a sequence of zero or one values
  */
