////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.sort

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException




trait GroupIterator extends SequenceIterator {

  /*@Nullable*/

  def getCurrentGroupingKey(): AtomicSequence

  def iterateCurrentGroup(): SequenceIterator

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A GroupIterator is an iterator that iterates over a sequence of groups.
  * The normal methods such as next() and current() always deliver the leading item
  * of the group. Additional methods are available to get the grouping key for the
  * current group (only applicable to group-by and group-adjacent), and to get all the
  * members of the current group.
  */
