////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.om.{GroundedValue, SequenceIterator}


/**
  * This interface is an extension to the SequenceIterator interface; it represents
  * a SequenceIterator that is based on an in-memory representation of a sequence,
  * and that is therefore capable of returning a Sequence containing all the items
  * in the sequence.
  *
  * We stretch the concept to consider an iterator over a MemoClosure as a grounded
  * iterator, on the basis that the in-memory sequence might exist already or might
  * be created as a side-effect of navigating the iterator. This is why materializing
  * the iterator can raise an exception.
  */
trait GroundedIterator extends SequenceIterator {
  def materialize: GroundedValue
  def getResidue: GroundedValue
}
