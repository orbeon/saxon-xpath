////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator




trait ConstrainedIterator[T <: Item]
    extends SequenceIterator
    with UnfailingIterator
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator {

  def hasNext(): Boolean

  /*@Nullable*/

  def next(): T

  def getLength(): Int

  /*@NotNull*/

  def materialize(): GroundedValue

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * SingletonIterator: an iterator over a sequence of zero or one values
  */
